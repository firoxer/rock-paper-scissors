(local repl (require "lib.repl"))
(local lume (require "lib.lume"))

(local config
  {:entities-per-type 200
   :speed 0.05})

(local state {:entities []})

(fn reset-state! []
  (set state.entities [])
  (for [_ 1 config.entities-per-type]
    (each [_ t (ipairs [:rock :paper :scissors])]
      (table.insert state.entities {:x (love.math.random) :y (love.math.random) :t t}))))

(fn all-types-are-same? []
  (var first-type-found (. state.entities 1 :t))
  (var reset? true)
  (each [_ {: t} (ipairs state.entities) &until (not reset?)]
    (when (not= t first-type-found)
      (set reset? false)))
  reset?)

(fn love.load []
  (repl.start)
  (reset-state!)
  (love.window.setFullscreen true))

(local lunch-map
  {:rock :scissors
   :scissors :paper
   :paper :rock})

(local enemy-map
  {:rock :paper
   :paper :scissors
   :scissors :rock})

(fn can-eat? [a b]
  (and a
       (= a.t (. enemy-map b.t))
       (< (lume.distance a.x a.y b.x b.y) 0.02)))

(fn find-nearest-of-type [t x y]
  (var nearest-distance math.huge)
  (var nearest-entity nil)
  (each [_ e (ipairs state.entities)]
    (when (and (= t e.t)
               (not= x e.x)
               (not= y e.y))
      (let [distance (lume.distance x y e.x e.y true)]
        (when (< distance nearest-distance)
          (set nearest-distance distance)
          (set nearest-entity e)))))
  nearest-entity)

(fn love.update [dt]
  (when (all-types-are-same?)
    (reset-state!))
  (each [_ {: x : y : t &as entity} (ipairs state.entities)]
    (let [enemy (find-nearest-of-type (. enemy-map t) x y)]
      (when (can-eat? enemy entity)
        (set entity.t enemy.t)))
    (var dx 0)
    (var dy 0)
    (let [enemy (find-nearest-of-type (. enemy-map entity.t) x y)]
      (when enemy
        (let [(vx vy) (lume.vector (lume.angle x y enemy.x enemy.y)
                                   (* -1 dt))]
          (set dx (+ dx vx))
          (set dy (+ dy vy)))))
    (let [mate (find-nearest-of-type entity.t x y)]
      (when mate
        (let [(vx vy) (lume.vector (lume.angle x y mate.x mate.y)
                                   (* -1 0.2 dt))]
          (set dx (+ dx vx))
          (set dy (+ dy vy)))))
    (let [lunch (find-nearest-of-type (. lunch-map entity.t) x y)]
      (when lunch
        (let [(vx vy) (lume.vector (lume.angle x y lunch.x lunch.y)
                                   (* dt))]
          (set dx (+ dx vx))
          (set dy (+ dy vy)))))
    ; Pull to middle
    (let [(vx vy) (lume.vector (lume.angle x y 0.5 0.5)
                               (* (lume.distance x y 0.5 0.5 true) dt))]
      (set dx (+ dx vx))
      (set dy (+ dy vy)))
    (set entity.x (lume.clamp (+ entity.x (* dx config.speed)) 0 1))
    (set entity.y (lume.clamp (+ entity.y (* dy config.speed)) 0 1))))

(fn love.keypressed [key]
  (when (= "escape" key)
    (love.event.quit)))

(fn screen-x [x]
  (lume.lerp 5 (- (love.graphics.getWidth) 5) x)) ; Some margin to show entities at edges

(fn screen-y [y]
  (lume.lerp 5 (- (love.graphics.getHeight) 5) y)) ; Some margin to show entities at edges

(love.graphics.setLineWidth 3)

(local rock-color [(lume.color "#4FC47F")])
(local paper-color [(lume.color "#009CFF")])
(local scissors-color [(lume.color "#F0330F")])

(fn love.draw []
  (love.graphics.clear [1 1 1])
  (each [_ {: t : x : y} (ipairs state.entities)]
    (match t
      :rock     (do (love.graphics.setColor rock-color)
                    (love.graphics.circle :fill (screen-x x) (screen-y y) 5))
      :paper    (do (love.graphics.setColor paper-color)
                    (love.graphics.rectangle :fill (- (screen-x x) 5) (- (screen-y y) 5) 10 10))
      :scissors (do (love.graphics.setColor scissors-color)
                    (love.graphics.line (- (screen-x x) 5) (- (screen-y y) 5)
                                        (+ (screen-x x) 5) (+ (screen-y y) 5))
                    (love.graphics.line (- (screen-x x) 5) (+ (screen-y y) 5)
                                        (+ (screen-x x) 5) (- (screen-y y) 5)))))
  (love.graphics.setColor [0 0 0])
  (love.graphics.print (love.timer.getFPS) 16 16))
