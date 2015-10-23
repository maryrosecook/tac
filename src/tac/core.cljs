(ns tac.core
  (:require [clojure.browser.repl :as repl]
            [clojure.set :as set]
            [goog.dom :as dom]
            [goog.events :as events]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(def grid 10)
(def screen (.getContext (.getElementById js/document "screen") "2d"))
(def screen-size {:x (/ (aget screen "canvas" "width") grid)
                  :y (/ (aget screen "canvas" "height") grid)})
(def window (dom/getWindow))
(def key-state (atom {:left nil :right nil :up nil :down nil}))

(defn latest-key [key-state]
  (if (not (empty? key-state))
    (ffirst (reverse (sort-by val key-state)))))

(defn passed [last wait]
  (< (+ last wait) (.getTime (js/Date.))))

(defn now []
  (.getTime (js/Date.)))

(defn can-move [obj]
  (passed (:last-move obj) (:move-every obj)))

(defn original-if-nil [f]
  (fn [value] (or (f value) value)))

(defn new-player-controlled-object-pos [obj]
  (let [down-key-state (into {} (filter second @key-state))
        moves
        [(if-let [direction (latest-key (select-keys down-key-state [:left :right]))]
           (fn [obj]
             (update obj :x (if (= :left direction) #(- % grid) #(+ % grid)))))
         (if-let [direction (latest-key (select-keys down-key-state [:up :down]))]
           (fn [obj]
             (update obj :y (if (= :up direction) #(- % grid) #(+ % grid)))))]]
    (pos (reduce #(%2 %1) obj (keep identity moves)))))

(defn pos [b]
  (select-keys b [:x :y]))

(defn move-crosshair [crosshair]
  (let [new-pos (new-player-controlled-object-pos crosshair)]
    (if (and (can-move crosshair) (:shift @key-state) (not= new-pos (pos crosshair)))
      (merge crosshair new-pos {:last-move (now)}))))

(defn move-player [player]
  (let [new-pos (new-player-controlled-object-pos player)]
    (if (and (can-move player) (not (:shift @key-state)) (not= new-pos (pos player)))
      (merge player new-pos {:last-move (now)}))))

(defn colliding? [b1 b2]
  (= (pos b1) (pos b2)))

;; stolen from github.com/jackschaedler/goya/blob/master/src/cljs/goya/components/bresenham.cljs
(defn bresenham-line [{x0 :x y0 :y} {x1 :x y1 :y}]
  (let [original-start {:x x0 :y y0}
        len-x (js/Math.abs (- x1 x0))
        len-y (js/Math.abs (- y1 y0))
        is-steep (> len-y len-x)]
    (let [[x0 y0 x1 y1] (if is-steep [y0 x0 y1 x1] [x0 y0 x1 y1])]
      (let [[x0 y0 x1 y1] (if (> x0 x1) [x1 y1 x0 y0] [x0 y0 x1 y1])]
        (let [delta-x (- x1 x0)
              delta-y (js/Math.abs (- y1 y0))
              y-step (if (< y0 y1) grid (- grid))]
          (loop [x x0
                 y y0
                 error (js/Math.floor (/ delta-x 2))
                 pixels (if is-steep [{:x y :y x}] [{:x x :y y}])]
            (if (> x x1)
              (distinct (if (= (first pixels) original-start) pixels (reverse pixels)))
              (if (< error delta-y)
                (recur (+ x grid)
                       (+ y y-step)
                       (+ error (- delta-x delta-y))
                       (if is-steep (conj pixels {:x y :y x}) (conj pixels {:x x :y y})))
                (recur (+ x grid)
                       y
                       (- error delta-y)
                       (if is-steep (conj pixels {:x y :y x}) (conj pixels {:x x :y y})))))))))))

(defn line-of-sight [a b bodies]
  (take-while (fn [body] (empty? (filter (partial colliding? body) bodies)))
              (rest (bresenham-line a b))))

(defn step [state]
  (-> state
      (update :player (original-if-nil move-player))
      (update :crosshair (original-if-nil move-crosshair))))

(defn fill-block [color block]
  (set! (.-fillStyle screen) color)
  (.fillRect screen (:x block) (:y block) grid grid))

(defn stroke-block [color block]
  (set! (.-strokeStyle screen) color)
  (.strokeRect screen
               (+ (:x block) 0.5) (+ (:y block) 0.5)
               (dec grid) (dec grid)))

(defn draw [state]
  (set! (.-fillStyle screen) "black")
  (.fillRect screen 0 0 (* (:x screen-size) grid) (* (:y screen-size) grid))
  (let [player (:player state)
        crosshair (:crosshair state)
        walls (:walls state)]

    (dorun (map (partial fill-block "white") walls))

    (let [los (line-of-sight player crosshair walls)]
      (dorun (map (partial fill-block "rgba(255, 0, 0, 0.5)") los)))

    (fill-block "orange" player)

    (stroke-block "red" crosshair)))

(defn tick [state]
  "Schedules next step and draw"
  (.requestAnimationFrame
   js/window
   (fn []
     (let [new-state (step state)]
       (draw new-state)
       (tick new-state)))))

(defn add-walls [state]
  (assoc state :walls [{:x 70 :y 70} {:x 80 :y 70} {:x 70 :y 80} {:x 80 :y 80}
                       {:x 150 :y 150} {:x 160 :y 150} {:x 150 :y 160} {:x 160 :y 160}]))

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
(tick
 (-> {:player {:x 50 :y 100 :last-move 0 :move-every 200}
      :crosshair {:x 30 :y 40 :last-move 0 :move-every 50}}
     (add-walls)))
