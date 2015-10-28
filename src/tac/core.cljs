(ns tac.core
  (:require [clojure.browser.repl :as repl]
            [clojure.set :as set]
            [goog.dom :as dom]
            [goog.events :as events]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(def key-maps
  {:player-1-dvorak {:left 65 :right 69 :up 188 :down 79 :aim 16 :firing 85 :switch 73}
   :player-2-dvorak {:left 72 :right 78 :up 67 :down 84 :aim 18 :switch 16}})

(def rifle-turn-speed 5)
(def grid 10)
(def level-dimensions 500)
(def screen (.getContext (.getElementById js/document "screen") "2d"))
(def screen-size {:x (aget screen "canvas" "width")
                  :y (aget screen "canvas" "height")})
(def window (dom/getWindow))
(def key-state (atom {}))

(defn latest-key [key-state]
  (if (not (empty? key-state))
    (ffirst (reverse (sort-by val key-state)))))

(defn passed [last wait]
  (< (+ last wait) (.getTime (js/Date.))))

(defn now []
  (.getTime (js/Date.)))

(defn moved-too-recently? [obj]
  (not (passed (:last-move obj) (:move-every obj))))

(defn pos [b]
  (select-keys b [:x :y]))

(defn active-keys
  [key-code->action down-or-pressed]
  (into {} (->> @key-state
                (map (fn [[key-code down-and-pressed]]
                       [key-code (get down-and-pressed down-or-pressed)])) ;; get down or pressed
                (filter second) ;; drop inactive
                (map #(assoc % 0 (get key-code->action (first %)))) ;; key-code->action
                (filter first)))) ;; drop ones not present in key map

(defn new-player-controlled-object-pos [obj action->key-code]
  (let [key-code->action (set/map-invert action->key-code)
        down-keys (active-keys key-code->action :down)
        moves
        [(if-let [direction (latest-key (select-keys down-keys [:left :right]))]
           (fn [obj]
             (update obj :x (if (= :left direction) #(- % grid) #(+ % grid)))))
         (if-let [direction (latest-key (select-keys down-keys [:up :down]))]
           (fn [obj]
             (update obj :y (if (= :up direction) #(- % grid) #(+ % grid)))))]]
    (pos (reduce #(%2 %1) obj (keep identity moves)))))

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

(defn magnitude
  [v]
  (js/Math.sqrt (+ (* (:x v) (:x v)) (* (:y v) (:y v)))))

(defn deg->rad
  [d]
  (* d 0.01745))

(defn on-screen?
  [screen-center obj]
  (not (or (< (:x obj) (- (:x screen-center) (/ (:x screen-size) 2)))
           (> (:x obj) (+ (:x screen-center) (/ (:x screen-size) 2)))
           (< (:y obj) (- (:y screen-center) (/ (:y screen-size) 2)))
           (> (:y obj) (+ (:y screen-center) (/ (:y screen-size) 2))))))

(defn colliding? [b1 b2]
  (= (pos b1) (pos b2)))

(defn angle->vector
  [angle radius]
  (let [radians (deg->rad angle)]
    {:x (* radius (js/Math.cos radians))
     :y (* radius (js/Math.sin radians))}))

(defn ->grid
  [grid n]
  (- n (mod n grid)))

(defmulti crosshair-position (fn [player] (get-in player [:weapon :type])))

(defmethod crosshair-position :rifle [player]
  (-> (angle->vector (get-in player [:weapon :angle])
                     (magnitude {:x level-dimensions :y level-dimensions}))
      (update :x #((partial ->grid grid) (+ % (:x player))))
      (update :y #((partial ->grid grid) (+ % (:y player))))))

(defmulti make-projectile (fn [player] (get-in player [:weapon :type])))

(defmethod make-projectile :rifle
  [player]
  (let [[start & path] (rest (bresenham-line player (crosshair-position player)))]
    (merge start
           {:type :bullet
            :last-move 0
            :move-every 100
            :path path})))

(defn new-rotating-aim-angle [angle action->key-code]
  (let [key-code->action (set/map-invert action->key-code)
        down-keys (active-keys key-code->action :down)
        direction (latest-key (select-keys down-keys [:left :right]))]
    (if direction
      (+ angle (* (if (= direction :left) -1 1) rifle-turn-speed))
      angle)))

(defn move-rifle [action->key-code weapon]
  (let [angle (new-rotating-aim-angle (:angle weapon) action->key-code)]
    (if (and (not (moved-too-recently? weapon))
             (get-in @key-state [(:aim action->key-code) :down])
             (not= angle (:angle weapon)))
      (merge weapon {:angle angle :last-move (now)})
      weapon)))

(defn update-firing-status [action->key-code weapon]
  (if (contains? (active-keys (set/map-invert action->key-code) :pressed) :firing)
    (do
      (update weapon :firing (fn [firing] (not firing))))
    weapon))

(defn move-player [player other-bodies action->key-code]
  (let [new-pos (new-player-controlled-object-pos player action->key-code)]
    (if (and (not (moved-too-recently? player))
             (not (get-in @key-state [(:aim action->key-code) :down]))
             (not= new-pos (pos player))
             (not-any? (partial colliding? new-pos) other-bodies))
      (merge player new-pos {:last-move (now)})
      player)))

(defn generate-projectiles [players]
  (->> players
       (filter #(let [weapon (:weapon %)]
                  (and (:firing weapon)
                       (passed (:last-shot weapon) (:shoot-every weapon)))))
       (reduce (fn [a player] (assoc a player (make-projectile player))) {})))

(defn step-player
  [{action->key-code :action->key-code :as player} other-bodies]
  (-> player
      (move-player other-bodies action->key-code)
      (update :weapon (partial update-firing-status action->key-code))
      (update :weapon (partial move-rifle action->key-code))))

(defn step-projectiles
  [bodies projectiles]
  (->> projectiles
       (filter #(not-any? (partial colliding? %) bodies))
       (map (fn [projectile]
              (if (not (moved-too-recently? projectile))
                (let [[new-pos & line-tail] (:path projectile)]
                  (merge projectile new-pos {:path line-tail}))
                projectile)))))

(defn bodies
  [state]
  (set (concat (:walls state) (:players state))))

(defn line-of-sight [a b bodies screen-center]
  (take-while (fn [point] (and (on-screen? screen-center point)
                               (not-any? (partial colliding? point) bodies)))
              (rest (bresenham-line a b))))

(defn unpress-keys
  []
  (dorun (map (fn [key-code] (swap! key-state
                                    assoc
                                    key-code
                                    (assoc (get @key-state key-code) :pressed nil)))
              (keys @key-state))))

(defn step [state]
  (let [bodies' (bodies state)]
    (-> state
        (assoc :players (map #(step-player % (disj bodies' %)) (:players state)))
        ((fn [state]
           (let [projectiles (generate-projectiles (:players state))]
             (-> state
                 (update :projectiles (partial concat (vals projectiles)))
                 (update :players
                         (partial map (fn [player]
                                        (if (contains? projectiles player)
                                          (assoc-in player [:weapon :last-shot] (now))
                                          player))))))))
        (update :projectiles (partial step-projectiles (bodies state))))))

(defn fill-block [color block]
  (set! (.-fillStyle screen) color)
  (.fillRect screen (:x block) (:y block) grid grid))

(defn stroke-block [color block]
  (set! (.-strokeStyle screen) color)
  (.strokeRect screen
               (+ (:x block) 0.5) (+ (:y block) 0.5)
               (- grid 1) (- grid 1)))

(defn view-offset
  [obj]
  {:x (- (- (:x obj) (/ (:x screen-size) 2)))
   :y (- (- (:y obj) (/ (:y screen-size) 2)))})

(defn draw-crosshair [player other-bodies]
  (let [los (line-of-sight player (crosshair-position player) other-bodies player)]
    (fill-block (:color player) player)
    (dorun (map (partial fill-block (get-in player [:weapon :color])) los))))

(defn draw [state]
  (let [players (:players state)
        player-to-center-on (nth players 0)
        view-offset' (view-offset player-to-center-on)
        weapon (get-in state [:player :weapon])
        on-screen-bodies (filter (partial on-screen? player-to-center-on) (bodies state))]

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

    ;; draw walls
    (dorun (map (partial fill-block "white")
                (filter (partial on-screen? player-to-center-on) (:walls state))))

    ;; draw players
    (dorun (map #(fill-block (:color %) %) players))
    (draw-crosshair player-to-center-on (disj (set on-screen-bodies) player-to-center-on))

    ;; draw projectiles
    (dorun (map (partial fill-block "yellow")
                (filter (partial on-screen? player-to-center-on) (:projectiles state))))

    ;; center back on origin
    (.restore screen)))

(defn tick [state]
  "Schedules next step and draw"
  (.requestAnimationFrame
   js/window
   (fn []
     (let [new-state (step state)]
       (draw new-state)
       (tick new-state)
       (unpress-keys)))))

(defn make-walls
  []
  (let [d level-dimensions]
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
   :action->key-code key-map
   :weapon {:type :rifle
            :x (+ x grid)
            :y (+ x grid)
            :last-move 0
            :move-every 0
            :last-shot 0
            :shoot-every 200
            :firing false
            :color crosshair-color}})

(defn keyboard-input->key-state []
  (events/listen window
                 (.-KEYUP events/EventType)
                 (fn [e]
                   (let [key-code (.-keyCode e)]
                     (swap! key-state
                            assoc
                            key-code
                            {:down nil :pressed nil}))))

  (events/listen window
                 (.-KEYDOWN events/EventType)
                 (fn [e]
                   (let [key-code (.-keyCode e)
                         now' (now)]
                     (if (nil? (get-in @key-state [key-code :down]))
                       (swap! key-state
                              assoc
                              key-code
                              {:down now' :pressed now'}))))))

;; start

(keyboard-input->key-state)
(tick
 (-> {:players [(make-player 50 50 "red" "rgba(255, 0, 0, 0.5)" (:player-1-dvorak key-maps))
                (make-player 250 250 "blue" "rgba(0, 0, 255, 0.5)" (:player-2-dvorak key-maps))]
      :projectiles []}
     (assoc :walls (make-walls))))