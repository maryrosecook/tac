(ns tac.core
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as dom]
            [goog.events :as events]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(def screen (.getContext (.getElementById js/document "screen") "2d"))
(def width (aget screen "canvas" "width"))
(def height (aget screen "canvas" "height"))
(def window (dom/getWindow))
(def key-state (atom {:left nil :right nil :up nil :down nil}))
(def grid 5)

(defn latest-key [key-state]
  (if (not (empty? key-state))
    (ffirst (reverse (sort-by val key-state)))))

(defn move-player [state]
  (let [down-key-state (into {} (filter second @key-state))]
    (-> state
        ((fn [state]
           (latest-key (select-keys down-key-state [:left :right]))
           (if-let [direction (latest-key (select-keys down-key-state [:left :right]))]
             (update-in state [:player :x] (if (= :left direction) #(- % grid) #(+ % grid)))
             state)))
        ((fn [state]
           (if-let [direction (latest-key (select-keys down-key-state [:up :down]))]
             (update-in state [:player :y] (if (= :up direction) #(- % grid) #(+ % grid)))
             state))))))

(defn step [state]
  (move-player state))

(defn draw [state]
  (.clearRect screen 0 0 width height)
  (let [player (get state :player)]
    (.fillRect screen (get player :x) (get player :y) 5 5)))

(defn tick [state]
  "Schedules next step and draw"
  (.requestAnimationFrame
   js/window
   (fn []
     (let [new-state (step state)]
       (draw new-state)
       (tick new-state)))))

(defn keyboard-input-to-key-state []
  (let [event->key-id (fn [e] (get {37 :left 39 :right 38 :up 40 :down} (.-keyCode e)))]
    (events/listen window
                   (.-KEYDOWN events/EventType)
                   (fn [e]
                     (let [key-id (event->key-id e)]
                       (if (and key-id (nil? (get @key-state key-id)))
                         (swap! key-state assoc key-id (.getTime (js/Date.)))))))
    (events/listen window
                   (.-KEYUP events/EventType)
                   (fn [e]
                     (if-let [key-id (event->key-id e)]
                       (swap! key-state assoc key-id nil))))))

(keyboard-input-to-key-state)
(tick {:player {:x 10 :y 30}})
