  (ns testvg.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [cljs-time.core :as time]
              [cljs-time.format :as tf]
              [cljs-time.coerce :as tc]
              [clojure.string :as string]
              [ajax.core :refer [GET POST]]))

;; --------------------------
;; Define counter stuff


;; A counter keeps track of a value and has a step function. By default it is inc, but can be any function you like
(defn counter
  "Given a label, an initial value and a step function, creates a counter. Defaults are 0 and inc. If you specify a step function, you must also specify an initial value."
  ([label] (counter label 0 inc))
  ([label init] (counter label init inc))
  ([label init step]
    (let [value (reagent/atom init)]
      {:value value
       :step #(swap! value step)
       :label label})))

(defn counter-component
  [counter]
  [:div
   "The atom " [:code (:label counter)] " has value: "
   (deref (:value counter)) ". "
   [:input {:type "button" :value "Click me!"
            :on-click (:step counter)}]])
          
;; A timer is a counter with an additional atom to track the id of the JS Interval that is advancing the count.
(defn timer
  "Given a counter, returns a timer"
  [counter]
  (assoc counter :interval (reagent/atom 0)))
          
(defn timer-component
  ([timer] (timer-component timer false))
  ([timer plural]
    [:div
     "The atom " [:code (:label timer)] " has value: "
     (deref (:value timer)) ". "
     [:input {:type "button" :value (if (= (deref (:interval timer)) 0) "Start!" "Stop!")
              :on-click #(if (= (deref (:interval timer)) 0)
                             (reset! (:interval timer) (js/setInterval (:step timer) 1000))
                             (do
                               (js/clearInterval (deref (:interval timer)))
                               (reset! (:interval timer) 0)))}]
                             
      (if plural
          [:input {:type "button" :value "Remove timer"
               :on-click plural}]
             nil)]))
                             
(defn timers-component
  [timers]
  [:div
   (doall (for [timer @timers]
               ^{:key (:label timer)}
               [timer-component timer (fn [] (swap! timers #(remove #{timer} %)))]))
   [:input {:type "button" :value "Add a timer!"
            :on-click (fn [] (swap! timers
                                    #(conj % (timer (counter (gensym))))))}]])
            
;; Counter for test1
(def counter1 (counter "counter1"))

;; Counters for test2
(def counter2a (counter "counter2a"))
(def counter2b (counter "counter2a"))

;; Counter for test3
(def timer3 (timer (counter "timer3")))

;; Counters for test4
(def timers4 (reagent/atom [(timer (counter (gensym)))]))


;; -------------------------
;; API stuff

(def vgteam-tv-shows-data
  (reagent/atom []))


(defn api-url
  [category endpoint]
  (str "http://api.vigiglobe.com/api/" category "/v1/" endpoint))

(defn put-api-data-in-atom
  [url opts atm]
  (GET
    url
    (assoc opts :handler #(reset! atm %))))
  
(def time-format (tf/formatter "yyyy-MM-ddThh:mm:ss.000Z"))

(defn update-tv-show-data
  []
  (put-api-data-in-atom
      (api-url "statistics" "volume")
      {:params {:project_id "vgteam-TV_Shows"
                :granularity "minute"
                :timeFrom (tf/unparse
                                      time-format
                                      (time/minus (time/now) (time/hours 1)))
                }}
      vgteam-tv-shows-data))
  
(update-tv-show-data)

(js/setInterval
  update-tv-show-data
  60000)

  

;; -------------------------
;; Chart stuff

(defn scale-to-dimension
  [data domain o-range]
  (let [scale (.linear js/d3.scale)
        _ (.domain scale (to-array domain))
        _ (.range scale (to-array o-range))]
     (doall (map scale data))
  ))

(defn line-chart
  [title pairs]
  (let [x (map #(tc/to-long (tf/parse time-format (first %))) pairs)
        y (map second pairs)
        height 300
        width 300
        scaled-x (scale-to-dimension x [(first x), (last x)] [0, width])
        scaled-y (scale-to-dimension y [0, (apply max y)  ] [(- height 20), -20])
        ;;Points drops the first and last element because they do not usually correspond to full minutes, so their values are misleadingly low
        points (drop 1 (take (- (count pairs) 1) (map vector scaled-x scaled-y)))]
      [:div
       [:h3 title]
      [:svg {:height height :width width}
       [:path {:d (str "M" (string/join "L" (map #(string/join "," %) points)))
               :fill "transparent"
               :stroke "black"
               :stroke-width "1.0"}]]]))


;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Welcome to testvg"]
   [:div [:a {:href "/about"} "go to about page"]]
   [:div [:a {:href "/test1"} "go to first test page"]]
   [:div [:a {:href "/test2"} "go to second test page"]]
   [:div [:a {:href "/test3"} "go to third test page"]]
   [:div [:a {:href "/bonus"} "go to bonus page"]]
   [:div [:a {:href "/chart"} "go to charts!"]]])

(defn about-page []
  [:div [:h2 "About testvg"]
   [:p "Thank you for giving me the opportunity to work on this problem. I had never used reagen or figwheel before and was very pleased with both. I had a lot of fun coming up with the appropriate abstractions to make each solution as general and customizable as possible."]
   [:p "By far the hardest part was dusting off my d3 knowledge and trying to use it in idiomatic ClojureScript. In many cases I found it was easier to manipulate the SVG directly, which was a learning curve of its own. Because of these two factors, I polished the graphs less than I would have liked; I didn't use transitions or axes because they were firmly embedded in d3's idiom, and a more comprehensive approach would have been necessary to use them here."]
   [:p "I didn't have time to implement the visualization I really wanted to: an interactive chloropleth globe with country-specific volume levels. When you click on a country, the highest quality (or perhaps klout or influent) messages from that country are displayed."]
   [:p "Thank you again for giving me this chance to demonstrate (and grow!) my skills. I hope you will consider me for this position."]
   [:p "[C Warren] Dale"]
   [:div [:a {:href "/"} "go to the home page"]]])
 
(defn test1-page []
  [:div [:h2 "The first test page"]
   (counter-component counter1)
   [:div [:a {:href "/"} "go to the home page"]]])
 
(defn test2-page []
  [:div [:h2 "The second test page"]
   (counter-component counter2a)
   (counter-component counter2b)
   [:div [:a {:href "/"} "go to the home page"]]])
  
(defn test3-page []
  [:div [:h2 "The third test page"]
   (timer-component timer3)
   [:div [:a {:href "/"} "go to the home page"]]])
 
(defn test4-page []
  [:div [:h2 "The bonus page!"]
   (timers-component timers4)
   [:div [:a {:href "/"} "go to the home page"]]])
 
 
(defn chart-page []
  (let [data @vgteam-tv-shows-data]
  [:div [:h2 "The charts page!"]
   [:div (line-chart "Message volume for the last hour, by minute"
                     (get (get data "data") "messages"))]
   [:p "Updated every 60 seconds"]
   [:div [:a {:href "/"} "go to the home page"]]]))


(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

(secretary/defroute "/test1" []
  (session/put! :current-page #'test1-page))

(secretary/defroute "/test2" []
  (session/put! :current-page #'test2-page))

(secretary/defroute "/test3" []
  (session/put! :current-page #'test3-page))

(secretary/defroute "/bonus" []
  (session/put! :current-page #'test4-page))

(secretary/defroute "/chart" []
  (session/put! :current-page #'chart-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!)
  (accountant/dispatch-current!)
  (mount-root))
