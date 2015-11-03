(ns tac.core
  (:require [clojure.browser.repl :as repl]
            [clojure.set :as set]
            [goog.dom :as dom]
            [goog.events :as events]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(def key-maps
  {:player-1-dvorak {:left 65 :right 69 :up 188 :down 79 :aim 16 :firing 85 :switch 73}
   :player-2-dvorak {:left 72 :right 78 :up 67 :down 84 :aim 18 :firing 83 :switch 189}})

(def grid 10)
(def level-dimensions 500)
(def screen1 (.getContext (.getElementById js/document "screen1") "2d"))
(def screen2 (.getContext (.getElementById js/document "screen2") "2d"))
(def screen-size {:x (aget screen1 "canvas" "width")
                  :y (aget screen1 "canvas" "height")})
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

(defn unit-vector
  [v]
  {:x (/ (:x v) (magnitude v))
   :y (/ (:y v) (magnitude v))})

(defn deg->rad
  [d]
  (* d 0.01745))

(defn rad->deg
  [r]
  (/ r 0.01745))

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

(defn vector->angle
  [v]
  (let [unit-vector' (unit-vector v)
        uncorrected-angle (rad->deg (js/Math.atan2 (:x unit-vector') (- (:y unit-vector'))))]
    (if (< uncorrected-angle 0) (+ uncorrected-angle 360) uncorrected-angle)))

(defn ->grid
  [grid n]
  (- n (mod n grid)))

(defmulti crosshair-position (fn [soldier] (get-in soldier [:weapon :type])))

(defmethod crosshair-position :rifle [soldier]
  (-> (angle->vector (get-in soldier [:weapon :angle])
                     (magnitude {:x level-dimensions :y level-dimensions}))
      (update :x #((partial ->grid grid) (+ % (:x soldier))))
      (update :y #((partial ->grid grid) (+ % (:y soldier))))))

(defmethod crosshair-position :mortar [soldier]
  (get-in soldier [:weapon]))

(defmulti make-projectile (fn [soldier] (get-in soldier [:weapon :type])))

(defmethod make-projectile :rifle
  [soldier]
  (let [[start & path] (rest (bresenham-line soldier (crosshair-position soldier)))]
    (merge start
           {:type :bullet
            :last-move 0
            :move-every 100
            :path path})))

(defmethod make-projectile :mortar
  [soldier]
  (let [[start & path] (rest (bresenham-line soldier (crosshair-position soldier)))]
    (merge start
           {:type :bomb
            :last-move 0
            :move-every 100
            :path path})))

(defn new-rotating-aim-angle [angle action->key-code]
  (let [key-code->action (set/map-invert action->key-code)
        down-keys (active-keys key-code->action :down)
        direction (latest-key (select-keys down-keys [:left :right]))]
    (if direction
      (+ angle (if (= direction :left) -1 1))
      angle)))

(defmulti move-crosshair (fn [action->key-code soldier] (:type soldier)))

(defmethod move-crosshair :rifle
  [action->key-code weapon]
  (let [angle (new-rotating-aim-angle (:angle weapon) action->key-code)]
    (if (and (not (moved-too-recently? weapon))
             (get-in @key-state [(:aim action->key-code) :down])
             (not= angle (:angle weapon)))
      (merge weapon {:angle angle :last-move (now)})
      weapon)))

(defmethod move-crosshair :mortar
  [action->key-code weapon]
  (let [new-pos (new-player-controlled-object-pos weapon action->key-code)]
    (if (and (not (moved-too-recently? weapon))
             (get-in @key-state [(:aim action->key-code) :down])
             (not= new-pos (select-keys weapon [:x :y])))
      (merge weapon new-pos {:last-move (now)})
      weapon)))

(defn update-firing-status [action->key-code weapon]
  (if (contains? (active-keys (set/map-invert action->key-code) :pressed) :firing)
    (do
      (update weapon :firing (fn [firing] (not firing))))
    weapon))

(defn move-soldier [soldier other-bodies action->key-code]
  (let [new-pos (new-player-controlled-object-pos soldier action->key-code)]
    (if (and (not (moved-too-recently? soldier))
             (not (get-in @key-state [(:aim action->key-code) :down]))
             (not= new-pos (pos soldier))
             (not-any? (partial colliding? new-pos) other-bodies))
      (merge soldier new-pos {:last-move (now)})
      soldier)))

(defn bodies
  [state]
  (concat (:walls state) (:soldiers state)))

(defn generate-projectiles [soldiers]
  (->> soldiers
       (filter #(let [weapon (:weapon %)]
                  (and (:firing weapon)
                       (passed (:last-shot weapon) (:shoot-every weapon)))))
       (reduce (fn [a soldier] (assoc a soldier (make-projectile soldier))) {})))

(defn player-current-soldier
  [soldiers {player-id :player-id current-soldier-id :current-soldier-id}]
  (some #(if (and (= player-id (:player-id %))
                  (= current-soldier-id (get-in % [:weapon :type])))
           %)
        soldiers))

(defn player-soldiers
  [soldiers {player-id :player-id}]
  (filter #(= player-id (:player-id %)) soldiers))

(defn other-soldier
  [soldiers player]
  (first (set/difference (set (player-soldiers soldiers player))
                         #{(player-current-soldier soldiers player)})))

(defn step-projectile-movement
  [{projectiles :projectiles :as state}]
  (assoc state :projectiles
         (->> projectiles
              (map (fn [projectile]
                     (if (not (moved-too-recently? projectile))
                       (let [[new-pos & line-tail] (:path projectile)]
                         (merge projectile new-pos {:path line-tail}))
                       projectile))))))

(defn maybe-switch-player-from-dead-soldier
  [soldiers player]
  (let [other-soldier' (other-soldier soldiers player)]
    (if (and other-soldier'
             (not (contains? (set (player-soldiers soldiers player))
                             (player-current-soldier soldiers player))))
      (assoc player :current-soldier-id (get-in other-soldier' [:weapon :type]))
      player)))

(defn step-projectile-destruction
  [{:keys [projectiles soldiers] :as state}]
  (let [bodies' (bodies state)]
    (-> state
        (assoc :soldiers (filter #(not-any? (partial colliding? %) projectiles) soldiers))
        (assoc :projectiles (filter #(not-any? (partial colliding? %) bodies') projectiles))
        ((fn [{:keys [soldiers players] :as state}]
           (assoc state :players
                  (map (partial maybe-switch-player-from-dead-soldier soldiers) players)))))))

(defn line-of-sight [a b bodies screen-center]
  (take-while (fn [point] (and (on-screen? screen-center point)
                               (not-any? (partial colliding? point) bodies)))
              (rest (bresenham-line a b))))

(defn vector-between
  [start end]
  {:x (- (:x end) (:x start))
   :y (- (:y end) (:y start))})

(defn abs-angle-difference
  "Handles angles <0 and >360"
  [a b]
  (js/Math.abs (- (mod (+ (- b a) 180) 360) 180)))

(defn visible? [looker target bodies]
  (let [bodies-without-target (disj (set bodies) target)
        los-angle (vector->angle (vector-between looker (crosshair-position looker)))
        looker-to-target-angle (vector->angle (vector-between looker target))]
    (and (< (abs-angle-difference los-angle looker-to-target-angle) 90)
         (empty? (filter (fn [point] (some (partial colliding? point) bodies-without-target))
                         (rest (bresenham-line looker target)))))))

(defn unpress-keys
  []
  (dorun (map (fn [key-code] (swap! key-state
                                    assoc
                                    key-code
                                    (assoc (get @key-state key-code) :pressed nil)))
              (keys @key-state))))

(defn soldier-to-key-map
  [soldier players]
  (some (fn [player] (if (= (:player-id player) (:player-id soldier))
                       (:action->key-code player)))
        players))

(defn handle-switching
  [{players :players :as state}]
  (assoc state :players
         (map (fn [{player-id :player-id action->key-code :action->key-code :as player}]
                (if (and (contains? (active-keys (set/map-invert action->key-code) :pressed)
                                    :switch)
                         (other-soldier (:soldiers state) player))
                  (assoc player :current-soldier-id
                         (get-in (other-soldier (:soldiers state) player) [:weapon :type]))
                  player))
              players)))

(defn step-soldiers
  [{players :players :as state}]
  (let [bodies' (bodies state)
        current-soldiers (set (map (partial player-current-soldier (:soldiers state))
                                   (:players state)))]
    (assoc state :soldiers
           (map (fn [soldier]
                  (if (contains? current-soldiers soldier)
                    (let [action->key-code (soldier-to-key-map soldier players)]
                      (-> soldier
                          (move-soldier bodies' action->key-code)
                          (update :weapon (partial update-firing-status action->key-code))
                          (update :weapon (partial move-crosshair action->key-code))))
                    soldier))
                (:soldiers state)))))

(defn step-projectile-generation
  [state]
  (let [projectiles (generate-projectiles (:soldiers state))]
    (-> state
        (update :projectiles (partial concat (vals projectiles)))
        (update :soldiers
                (partial map (fn [soldier]
                               (if (contains? projectiles soldier)
                                 (assoc-in soldier [:weapon :last-shot] (now))
                                 soldier)))))))

(defn step [state]
  (-> state
      (handle-switching)
      (step-soldiers)
      (step-projectile-generation)
      (step-projectile-movement)
      (step-projectile-destruction)))

(defn fill-block [screen color block]
  (set! (.-fillStyle screen) color)
  (.fillRect screen (:x block) (:y block) grid grid))

(defn stroke-block [screen color block]
  (set! (.-strokeStyle screen) color)
  (set! (.-lineWidth screen) 1.5)
  (.strokeRect screen
               (+ (:x block) 0.5) (+ (:y block) 0.5)
               (- grid 1) (- grid 1)))

(defn view-offset
  [obj]
  {:x (- (- (:x obj) (/ (:x screen-size) 2)))
   :y (- (- (:y obj) (/ (:y screen-size) 2)))})

(defmulti draw-crosshair (fn [screen soldier other-bodies] (get-in soldier [:weapon :type])))

(defmethod draw-crosshair :rifle [screen soldier other-bodies]
  (let [los (line-of-sight soldier (crosshair-position soldier) other-bodies soldier)]
    (fill-block screen (:color soldier) soldier)
    (dorun (map (partial fill-block screen (get-in soldier [:weapon :color])) los))))

(defmethod draw-crosshair :mortar [screen soldier other-bodies]
  (let [crosshair-position' (crosshair-position soldier)
        los (rest (bresenham-line soldier crosshair-position'))]
    (fill-block screen (:color soldier) soldier)
    (dorun (map (partial fill-block screen (get-in soldier [:weapon :color])) los))
    (stroke-block screen (:color soldier) crosshair-position')))

(defn draw-local-soldiers
  [screen soldiers player on-screen-bodies]
  (let [player-soldiers' (player-soldiers soldiers player)]
    (dorun (map (fn [{{:keys [firing type]} :weapon :as soldier}]
                  (fill-block screen "red" soldier)
                  (draw-crosshair screen soldier on-screen-bodies)
                  (if firing
                    (stroke-block screen "yellow" soldier)))
                soldiers))))

(defn draw-screen
  [state screen local-player remote-player]
  (let [current-soldier (player-current-soldier (:soldiers state) local-player)
        view-offset' (view-offset current-soldier)
        on-screen-bodies (filter (partial on-screen? current-soldier) (bodies state))]

    ;; center on player
    (.save screen)
    (.translate screen (:x view-offset') (:y view-offset'))

    ;; clear screen
    (set! (.-fillStyle screen) "black")
    (.fillRect screen
               (- (:x current-soldier) (/ (:x screen-size) 2))
               (- (:y current-soldier) (/ (:y screen-size) 2))
               (:x screen-size)
               (:y screen-size))

    ;; draw walls
    (dorun (map (partial fill-block screen "white")
                (filter (partial on-screen? current-soldier) (:walls state))))

    ;; draw local soldiers
    (draw-local-soldiers screen
                         (player-soldiers (:soldiers state) local-player)
                         local-player
                         on-screen-bodies)

    ;; draw remote soldiers
    (dorun (map #(fill-block screen (:color %) %)
                (filter #(and (on-screen? current-soldier %)
                              (visible? current-soldier % on-screen-bodies))
                        (player-soldiers (:soldiers state) remote-player))))

    ;; draw projectiles
    (dorun (map (partial fill-block screen "yellow")
                (filter (partial on-screen? current-soldier) (:projectiles state))))

    ;; center back on origin
    (.restore screen)))

(defn draw
  [state]
  (let [player1 (nth (:players state) 0)
        player2 (nth (:players state) 1)]
    (draw-screen state screen1 player1 player2)
    (draw-screen state screen2 player2 player1)))

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
     [;; top left barrier
      {:x 40 :y 100} {:x 50 :y 100} {:x 60 :y 100} {:x 70 :y 100} {:x 80 :y 100} {:x 90 :y 100}
      {:x 100 :y 40} {:x 100 :y 50} {:x 100 :y 60} {:x 100 :y 70} {:x 100 :y 80} {:x 100 :y 90}
      {:x 100 :y 100}

      ;; bottom right barrier
      {:x 400 :y 400} {:x 410 :y 400} {:x 420 :y 400} {:x 430 :y 400} {:x 440 :y 400}
      {:x 450 :y 400}
      {:x 400 :y 410} {:x 400 :y 420} {:x 400 :y 430} {:x 400 :y 440} {:x 400 :y 450}]

     ;; border walls
     (map #(hash-map :x %          :y 0)          (range 0 (- d grid) grid))
     (map #(hash-map :x (- d grid) :y %)          (range 0 (- d grid) grid))
     (map #(hash-map :x %          :y (- d grid)) (range grid d grid))
     (map #(hash-map :x 0          :y %)          (range grid d grid)))))

(defn make-player
  [player-id action->key-code]
  {:player-id player-id
   :action->key-code action->key-code
   :current-soldier-id :rifle})

(defn make-soldier
  [player-id pos color weapon]
  (merge pos {:player-id player-id
              :last-move 0
              :move-every 200
              :color color
              :weapon weapon}))

(defmulti make-weapon (fn [type & rest] type))

(defmethod make-weapon :rifle
  [type color]
   {:type type
    :angle 0
    :last-move 0
    :move-every 10
    :last-shot 0
    :shoot-every 1000
    :firing false
    :color color})

(defmethod make-weapon :mortar
  [type pos color]
  (merge pos {:type type
              :last-move 0
              :move-every 100
              :last-shot 0
              :shoot-every 1000
              :firing false
              :color color}))

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
(let [player1-rifle-pos {:x 30 :y 30}
      player1-mortar-pos {:x 50 :y 50}
      player2-rifle-pos {:x (- level-dimensions 30) :y (- level-dimensions 30)}
      player2-mortar-pos {:x (- level-dimensions 50) :y (- level-dimensions 50)}]
  (tick
   (-> {:players [(make-player :red (:player-1-dvorak key-maps))
                  (make-player :blue (:player-2-dvorak key-maps))]
        :soldiers [
                   (make-soldier :red
                                 player1-rifle-pos
                                 "red"
                                 (make-weapon :rifle
                                              "rgba(255, 0, 0, 0.5)"))
                   (make-soldier :red
                                 player1-mortar-pos
                                 "red"
                                 (make-weapon :mortar
                                              {:x (+ (:x player1-mortar-pos) (* grid 2))
                                               :y (+ (:y player1-mortar-pos) (* grid 2))}
                                              "rgba(255, 0, 0, 0.5)"))
                   (make-soldier :blue
                                 player2-rifle-pos
                                 "blue"
                                 (make-weapon :rifle "rgba(0, 0, 255, 0.5)"))
                   (make-soldier :blue
                                 player2-mortar-pos
                                 "blue"
                                 (make-weapon :mortar
                                              {:x (- (:x player2-mortar-pos) (* grid 2))
                                               :y (- (:y player2-mortar-pos) (* grid 2))}
                                              "rgba(0, 0, 255, 0.5)"))]
        :projectiles []}
       (assoc :walls (make-walls)))))