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
(def screen-size {:x (aget screen "canvas" "width")
                  :y (aget screen "canvas" "height")})
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

(defn magnitude
  [v]
  (js/Math.sqrt (+ (* (:x v) (:x v)) (* (:y v) (:y v)))))

(defn deg->rad
  [d]
  (* d 0.01745))

(defn angle-to-vector
  [angle radius]
  (let [radians (deg->rad angle)]
    {:x (* radius (js/Math.cos radians))
     :y (* radius (js/Math.sin radians))}))

(defn new-rotating-aim-angle [angle action-to-key-code]
  (let [turn-speed 3
        key-code-to-action (set/map-invert action-to-key-code)
        down-key-state (into {} (->> @key-state
                                     (filter second)
                                     (map #(assoc % 0 (get key-code-to-action (first %))))
                                     (filter first)))
        direction (latest-key (select-keys down-key-state [:left :right]))]
    (if direction
      (+ angle (* (if (= direction :left) -1 1) turn-speed))
      angle)))

(defn move-mortar [crosshair action-to-key-code]
  (let [new-pos (new-player-controlled-object-pos crosshair action-to-key-code)]
    (if (and (can-move crosshair)
             (get @key-state (:aim action-to-key-code))
             (not= new-pos (pos crosshair)))
      (merge crosshair new-pos {:last-move (now)})
      crosshair)))

(defn move-rifle [crosshair action-to-key-code]
  (let [angle (new-rotating-aim-angle (:angle crosshair) action-to-key-code)]
    (if (and (can-move crosshair)
             (get @key-state (:aim action-to-key-code))
             (not= angle (:angle crosshair)))
      (merge crosshair {:angle angle :last-move (now)})
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
    :crosshair (move-rifle (:crosshair player) (:action-to-key-code player))))

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

(defn to-grid
  [grid n]
  (- n (mod n grid)))

(defn view-offset
  [obj]
  {:x (- (- (:x obj) (/ (:x screen-size) 2)))
   :y (- (- (:y obj) (/ (:y screen-size) 2)))})

(defmulti draw-player (fn [player walls] (get-in player [:crosshair :type])))

(defmethod draw-player :rifle [player walls]
  (let [crosshair-pos (-> (angle-to-vector (get-in player [:crosshair :angle])
                                           (magnitude screen-size))
                          (update :x #((partial to-grid grid) (+ % (:x player))))
                          (update :y #((partial to-grid grid) (+ % (:y player)))))
        los (line-of-sight player crosshair-pos walls)]
    (fill-block (:color player) player)
    (dorun (map (partial fill-block (get-in player [:crosshair :color])) los))))

(defn draw [state]
  (let [players (:players state)
        player-to-center-on (nth players 0)
        view-offset' (view-offset player-to-center-on)
        crosshair (get-in state [:player :crosshair])
        walls (:walls state)]

    ;; center on player
    (.save screen)
    (.translate screen (:x view-offset') (:y view-offset'))

    ;; clear screen
    (set! (.-fillStyle screen) "black")
    (.fillRect screen
               (- (:x player-to-center-on) (/ (:x screen-size) 2))
               (- (:y player-to-center-on) (/ (:y screen-size) 2))
               (:x screen-size)
               (:y screen-size))

    ;; draw scene
    (dorun (map (partial fill-block "white") walls))
    (dorun (map #(draw-player % walls) players))

    ;; center back on origin
    (.restore screen)))

(defn tick [state]
  "Schedules next step and draw"
  (.requestAnimationFrame
   js/window
   (fn []
     (let [new-state (step state)]
       (draw new-state)
       (tick new-state)))))

(defn make-walls
  []
  (let [d 300]
    (concat
     ;; pillars
     [{:x 70 :y 70} {:x 80 :y 70} {:x 70 :y 80} {:x 80 :y 80}
      {:x 150 :y 150} {:x 160 :y 150} {:x 150 :y 160} {:x 160 :y 160}]

     ;; border walls
     (map #(hash-map :x %          :y 0)          (range 0 (- d grid) grid))
     (map #(hash-map :x (- d grid) :y %)          (range 0 (- d grid) grid))
     (map #(hash-map :x %          :y (- d grid)) (range grid d grid))
     (map #(hash-map :x 0          :y %)          (range grid d grid)))))

(defn make-player
  [x y color crosshair-color key-map]
  {:x x
   :y y
   :last-move 0
   :move-every 200
   :color color
   :action-to-key-code key-map
   :weapon :rifle
   :crosshair {:type :rifle
               :x (+ x grid)
               :y (+ x grid)
               :last-move 0
               :move-every 0
               :color crosshair-color}})

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

;; start

(keyboard-input-to-key-state)
(tick
 (-> {:players [(make-player 50 50 "red" "rgba(255, 0, 0, 0.5)" (:player-1-dvorak key-maps))
                (make-player 250 250 "blue" "rgba(0, 0, 255, 0.5)" (:player-2-dvorak key-maps))]}
     (assoc :walls (make-walls))))