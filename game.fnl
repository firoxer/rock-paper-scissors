(local repl (require "lib.repl"))
(local lume (require "lib.lume"))
(local profile (require :lib.profile))

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
  (love.window.setMode 800 600 {:resizable true})
  (love.window.maximize))

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

(local profiling? false)
(when profiling?
  (var frames-since-last-report 0)
  (let [update love.update
        update* (fn [...]
                  (if (> frames-since-last-report 30)
                    (do (print (profile.report 20))
                        (set frames-since-last-report 0))
                    (set frames-since-last-report (+ 1 frames-since-last-report)))
                  (update ...))]
    (profile.start)
    (set love.update update*)))

(fn love.keypressed [key]
  (when (= "escape" key)
    (love.event.quit)))

(local rock-color [(lume.color "#4FC47F")])
(local paper-color [(lume.color "#009CFF")])
(local scissors-color [(lume.color "#F0330F")])

(fn love.draw []
  (love.graphics.clear [1 1 1])
  (let [screen-width (love.graphics.getWidth)
        screen-height (love.graphics.getHeight)
        scale-x #(lume.lerp 5 (- screen-width 5) $) ; Some margin to show entities at edges
        scale-y #(lume.lerp 5 (- screen-height 5) $) ; Some margin to show entities at edges
        entity-size (scale-x 0.005)
        entity-half-size (/ entity-size 2)] 
    (love.graphics.setLineWidth (* entity-size 0.3))
    (each [_ {: t : x : y} (ipairs state.entities)]
      (match t
        :rock     (do (love.graphics.setColor rock-color)
                      (love.graphics.circle :fill (scale-x x) (scale-y y) entity-half-size))
        :paper    (do (love.graphics.setColor paper-color)
                      (love.graphics.rectangle :fill (- (scale-x x) entity-half-size)
                                                     (- (scale-y y) entity-half-size)
                                                     entity-size
                                                     entity-size))
        :scissors (do (love.graphics.setColor scissors-color)
                      (love.graphics.line (- (scale-x x) entity-half-size) (- (scale-y y) entity-half-size)
                                          (+ (scale-x x) entity-half-size) (+ (scale-y y) entity-half-size))
                      (love.graphics.line (- (scale-x x) entity-half-size) (+ (scale-y y) entity-half-size)
                                          (+ (scale-x x) entity-half-size) (- (scale-y y) entity-half-size)))))
    (love.graphics.setColor [0 0 0])
    (love.graphics.print (love.timer.getFPS) 16 16)))
