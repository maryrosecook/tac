(ns tac.core
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as dom]
            [goog.events :as events]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(def grid 10)
(def screen (.getContext (.getElementById js/document "screen") "2d"))
(def width (/ (aget screen "canvas" "width") grid))
(def height (/ (aget screen "canvas" "height") grid))
(def window (dom/getWindow))
(def key-state (atom {:left nil :right nil :up nil :down nil}))

(defn latest-key [key-state]
  (if (not (empty? key-state))
    (ffirst (reverse (sort-by val key-state)))))

(defn passed [last wait]
  (< (+ last wait) (.getTime (js/Date.))))

(defn now []
  (.getTime (js/Date.)))

(defn original-if-nil [f]
  (fn [value] (or (f value) value)))

(defn move-player-controlled-object [obj]
  (let [down-key-state (into {} (filter second @key-state))
        can-move (passed (:last-move obj) (:move-every obj))
        moves
        (->> [(if-let [direction (latest-key (select-keys down-key-state [:left :right]))]
                (fn [obj]
                  (update obj :x (if (= :left direction) #(- % 1) #(+ % 1)))))
              (if-let [direction (latest-key (select-keys down-key-state [:up :down]))]
                (fn [obj]
                  (update obj :y (if (= :up direction) #(- % 1) #(+ % 1)))))]
             (keep identity))]
    (if (and can-move (not (empty? moves)))
      (assoc (reduce #(%2 %1) obj moves) :last-move (now)))))

(defn move-crosshair [crosshair]
  (if (:shift @key-state)
    (move-player-controlled-object crosshair)))

(defn move-player [player]
  (if (not (:shift @key-state))
    (move-player-controlled-object player)))

(defn colliding? [a b]
  (= (count (set [(select-keys a [:x :y])
                  (select-keys b [:x :y])]))
     1))

(defn bodies [state]
  (conj (:bodies state) (:player state)))

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

(defn line-of-sight [a b bodies]
  (bresenham-line a b))

(defn step [state]
  (-> state
      (update :player (original-if-nil move-player))
      (update :crosshair (original-if-nil move-crosshair))))

(defn fill-block [color block]
  (set! (.-fillStyle screen) color)
  (.fillRect screen
             (* (:x block) grid) (* (:y block) grid)
             grid grid))

(defn stroke-block [color block]
  (set! (.-strokeStyle screen) color)
  (.strokeRect screen
               (+ (* (:x block) grid) 0.5) (+ (* (:y block) grid) 0.5)
               (dec grid) (dec grid)))

(defn draw [state]
  (.clearRect screen 0 0 (* width grid) (* height grid))
  (let [crosshair (:crosshair state)
        blocks (:blocks state)]

    (dorun (map (partial fill-block "#999") blocks))
    (let [los (line-of-sight (:player state) crosshair (bodies state))]
      (dorun (map (partial fill-block "rgba(255, 0, 0, 0.2)") los)))

    (fill-block "black" (:player state))

    (stroke-block "red" crosshair)))

(defn tick [state]
  "Schedules next step and draw"
  (.requestAnimationFrame
   js/window
   (fn []
     (let [new-state (step state)]
       (draw new-state)
       (tick new-state)))))

(defn generate-blocks [width height]
  (into [] (set (map (fn [_] {:x (rand-int width) :y (rand-int height)})
                     (range 100)))))

(defn keyboard-input-to-key-state []
  (let [event->key-id (fn [e] (get {37 :left 39 :right 38 :up 40 :down 16 :shift}
                                   (.-keyCode e)))]
    (events/listen window
                   (.-KEYDOWN events/EventType)
                   (fn [e]
                     (let [key-id (event->key-id e)]
                       (if (and key-id (nil? (key-id @key-state)))
                         (swap! key-state assoc key-id (now))))))
    (events/listen window
                   (.-KEYUP events/EventType)
                   (fn [e]
                     (if-let [key-id (event->key-id e)]
                       (swap! key-state assoc key-id nil))))))

(keyboard-input-to-key-state)
(tick {:player {:x 5 :y 10 :last-move 0 :move-every 200}
       :crosshair {:x 3 :y 4 :last-move 0 :move-every 50}
       :blocks (generate-blocks width height)})
