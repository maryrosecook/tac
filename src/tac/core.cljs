(ns tac.core
  (:require [clojure.browser.repl :as repl]
            [clojure.set :as set]
            [goog.dom :as dom]
            [goog.events :as events]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(def key-maps
  {:player-1-dvorak {:left 65 :right 69 :up 188 :down 79 :aim 16 :switch 17}
   :player-2-dvorak {:left 72 :right 78 :up 67 :down 84 :aim 18 :switch 16}})

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

(defn pos [b]
  (select-keys b [:x :y]))

(defn new-player-controlled-object-pos [obj action-to-key-code]
  (let [key-code-to-action (set/map-invert action-to-key-code)
        down-key-state (into {} (->> @key-state
                                     (filter second)
                                     (map #(assoc % 0 (get key-code-to-action (first %))))
                                     (filter first)))
        moves
        [(if-let [direction (latest-key (select-keys down-key-state [:left :right]))]
           (fn [obj]
             (update obj :x (if (= :left direction) #(- % grid) #(+ % grid)))))
         (if-let [direction (latest-key (select-keys down-key-state [:up :down]))]
           (fn [obj]
             (update obj :y (if (= :up direction) #(- % grid) #(+ % grid)))))]]
    (pos (reduce #(%2 %1) obj (keep identity moves)))))

(defn move-crosshair [crosshair action-to-key-code]
  (let [new-pos (new-player-controlled-object-pos crosshair action-to-key-code)]
    (if (and (can-move crosshair)
             (get @key-state (:aim action-to-key-code))
             (not= new-pos (pos crosshair)))
      (merge crosshair new-pos {:last-move (now)})
      crosshair)))

(defn move-player [player action-to-key-code]
  (let [new-pos (new-player-controlled-object-pos player action-to-key-code)]
    (if (and (can-move player)
             (not (get @key-state (:aim action-to-key-code)))
             (not= new-pos (pos player)))
      (merge player new-pos {:last-move (now)})
      player)))

(defn step-player [player]
  (assoc (move-player player (:action-to-key-code player))
    :crosshair (move-crosshair (:crosshair player) (:action-to-key-code player))))

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
  (assoc state :players (map step-player (:players state))))

(defn fill-block [color block]
  (set! (.-fillStyle screen) color)
  (.fillRect screen (:x block) (:y block) grid grid))

(defn stroke-block [color block]
  (set! (.-strokeStyle screen) color)
  (.strokeRect screen
               (+ (:x block) 0.5) (+ (:y block) 0.5)
               (- grid 1) (- grid 1)))

(defn draw-player [player walls]
  (let [crosshair (:crosshair player)
        los (line-of-sight player crosshair walls)]
    (fill-block (:color player) player)
    (dorun (map (partial fill-block (:color crosshair)) los))
    (stroke-block (:color player) crosshair)))

(defn draw [state]
  (set! (.-fillStyle screen) "black")
  (.fillRect screen 0 0 (* (:x screen-size) grid) (* (:y screen-size) grid))
  (let [players (:player state)
        crosshair (get-in state [:player :crosshair])
        walls (:walls state)]
    (dorun (map (partial fill-block "white") walls))
    (dorun (map #(draw-player % walls) (:players state)))))

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
  (events/listen window
                 (.-KEYUP events/EventType)
                 (fn [e]
                   (let [key-code (.-keyCode e)]
                     (swap! key-state assoc key-code nil))))

  (events/listen window
                 (.-KEYDOWN events/EventType)
                 (fn [e]
                   (let [key-code (.-keyCode e)]
                     (if (nil? (get @key-state key-code))
                       (swap! key-state assoc key-code (now)))))))

(keyboard-input-to-key-state)
(tick
 (-> {:players [{:x 50 :y 50 :last-move 0 :move-every 200 :color "red"
                 :action-to-key-code (:player-1-dvorak key-maps)
                 :crosshair {:x 0 :y 0 :last-move 0 :move-every 50
                             :color "rgba(255, 0, 0, 0.5)"}}
                {:x 350 :y 350 :last-move 0 :move-every 200 :color "blue"
                 :action-to-key-code (:player-2-dvorak key-maps)
                 :crosshair {:x 390 :y 390 :last-move 0 :move-every 50
                             :color "rgba(0, 0, 255, 0.5)"}}]}
     (add-walls)))