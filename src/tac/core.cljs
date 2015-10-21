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
(def grid 10)
(def move-frequency 200)

(defn latest-key [key-state]
  (if (not (empty? key-state))
    (ffirst (reverse (sort-by val key-state)))))

(defn passed [last wait]
  (< (+ last wait) (.getTime (js/Date.))))

(defn now []
  (.getTime (js/Date.)))

(defn move-player [state]
  (let [down-key-state (into {} (filter second @key-state))
        can-move (passed (get-in state [:player :last-move]) move-frequency)
        moves
        (->> [(if-let [direction (latest-key (select-keys down-key-state [:left :right]))]
                (fn [state]
                  (update-in state [:player :x]
                             (if (= :left direction) #(- % grid) #(+ % grid)))))
              (if-let [direction (latest-key (select-keys down-key-state [:up :down]))]
                (fn [state]
                  (update-in state [:player :y]
                             (if (= :up direction) #(- % grid) #(+ % grid)))))]
             (keep identity))]
    (if (and can-move (not (empty? moves)))
      (assoc-in (reduce #(%2 %1) state moves) [:player :last-move] (now))
      state)))

;; stolen from github.com/jackschaedler/goya/blob/master/src/cljs/goya/components/bresenham.cljs
(defn bresenham-line [{x0 :x y0 :y} {x1 :x y1 :y}]
  (let [len-x (js/Math.abs (- x1 x0))
        len-y (js/Math.abs (- y1 y0))
        is-steep (> len-y len-x)]
    (let [[x0 y0 x1 y1] (if is-steep [y0 x0 y1 x1] [x0 y0 x1 y1])]
      (let [[x0 y0 x1 y1] (if (> x0 x1) [x1 y1 x0 y0] [x0 y0 x1 y1])]
        (let [delta-x (- x1 x0)
              delta-y (js/Math.abs (- y1 y0))
              y-step (if (< y0 y1) 1 -1)]
          (loop [x x0
                 y y0
                 error (js/Math.floor (/ delta-x 2))
                 pixels (if is-steep [{:x y :y x}] [{:x x :y y}])]
            (if (> x x1)
              pixels
              (if (< error delta-y)
                (recur (inc x)
                       (+ y y-step)
                       (+ error (- delta-x delta-y))
                       (if is-steep (conj pixels {:x y :y x}) (conj pixels {:x x :y y})))
                (recur (inc x)
                       y
                       (- error delta-y)
                       (if is-steep (conj pixels {:x y :y x}) (conj pixels {:x x :y y}))
                       )))))))))

(defn step [state]
  (move-player state))

(defn draw [state]
  (.clearRect screen 0 0 width height)
  (let [player (get state :player)]
    (.fillRect screen (get player :x) (get player :y) grid grid)))

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
                         (swap! key-state assoc key-id (now))))))
    (events/listen window
                   (.-KEYUP events/EventType)
                   (fn [e]
                     (if-let [key-id (event->key-id e)]
                       (swap! key-state assoc key-id nil))))))

(keyboard-input-to-key-state)
(tick {:player {:x 10 :y 30 :last-move 0}})
