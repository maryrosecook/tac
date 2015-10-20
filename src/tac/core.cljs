(ns tac.core
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as dom]
            [goog.events :as events]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(def screen (.getContext (.getElementById js/document "screen") "2d"))
(def width (aget screen "canvas" "width"))
(def height (aget screen "canvas" "height"))
(def key-state (atom {:left false :right false :up false :down false}))
(def grid 5)

(defn move-player [state]
  (-> state
      ((fn [state] (if (get @key-state :left) (update-in state [:player :x] #(- % grid)) state)))
      ((fn [state] (if (get @key-state :right) (update-in state [:player :x] #(+ % grid)) state)))
      ((fn [state] (if (get @key-state :up) (update-in state [:player :y] #(- % grid)) state)))
      ((fn [state] (if (get @key-state :down) (update-in state [:player :y] #(+ % grid)) state)))))

(defn step [state]
  (move-player state))

(defn draw [state]
  (.clearRect screen 0 0 width height)
  (let [player (get state :player)]
    (.fillRect screen (get player :x) (get player :y) 5 5)))

(defn tick [state]
  (.requestAnimationFrame
   js/window
   (fn []
     (let [new-state (step state)]
       (draw new-state)
       (tick new-state)))))

(defn keyboard-input-to-key-state []
  (let [window (dom/getWindow)
        key-code->key-id {37 :left 39 :right 38 :up 40 :down}
        set-key (fn [state e]
                  (if-let [key-id (get key-code->key-id (.-keyCode e))]
                    (swap! key-state assoc key-id state)))]
    (events/listen window (.-KEYDOWN events/EventType) (partial set-key true))
    (events/listen window (.-KEYUP events/EventType) (partial set-key false))))

(keyboard-input-to-key-state)
(tick {:player {:x 10 :y 30}})
