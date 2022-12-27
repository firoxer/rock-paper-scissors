(local lume (require :lib.lume))
(local profile (require :lib.profile))

(local quadtree (require :quadtree))

(local entity-n 100)
(local minimum-eat-distance (math.pow 0.02 2)) ; precalculated for performance

(fn generate-entities [n]
  (local entities (quadtree.new))
  (for [_ 1 n]
    (quadtree.insert entities {:x (love.math.random)
                               :y (love.math.random)
                               :t (lume.randomchoice [:rock :paper :scissors])}))
  entities)

(fn count-type-n [entities]
  (local counts {:rock 0 :paper 0 :scissors 0})
  (each [entity (quadtree.walk entities)]
    (tset counts entity.t (+ (. counts entity.t) 1)))
  counts)

(fn eats? [a b]
  (or (and (= a.t :rock) (= b.t :scissors))
      (and (= a.t :paper) (= b.t :rock))
      (and (= a.t :scissors) (= b.t :paper))))

(fn eaten-by? [a b]
  (or (and (= a.t :rock) (= b.t :paper))
      (and (= a.t :paper) (= b.t :scissors))
      (and (= a.t :scissors) (= b.t :rock))))

(fn friends? [a b]
  (= a.t b.t))

(fn can-eat? [a b]
  (and (eats? a b)
       (let [squared-distance (lume.distance a.x a.y b.x b.y true)]
         (< 0 squared-distance minimum-eat-distance))))

(fn find-winner [entities]
  (let [{: rock : paper : scissors} (count-type-n entities)]
    (if (= rock 0) :paper
        (= paper 0) :scissors
        (= scissors 0) :rock)))

(fn next-entities [entities]
  (let [speed 0.001
        new-entities (quadtree.new)]
    (each [{: x : y : t &as entity} (quadtree.walk entities)]
      (local new-entity (lume.clone entity))
      (var dx 0)
      (var dy 0)
      (each [neighbor (quadtree.nearest-neighbors 3 entities x y {:ignore-exact? true})]
        (when (can-eat? neighbor entity)
          (set new-entity.t neighbor.t))
        (let [magnitude (if (eaten-by? entity neighbor) -1
                            (eats? entity neighbor) 1
                            (friends? entity neighbor) -0.1)
              (vx vy) (lume.vector (lume.angle x y neighbor.x neighbor.y) magnitude)]
          (set dx (+ dx vx))
          (set dy (+ dy vy))))
      ; Pull to middle
      (let [(vx vy) (lume.vector (lume.angle x y 0.5 0.5)
                                 (lume.distance x y 0.5 0.5 true))]
        (set dx (+ dx vx))
        (set dy (+ dy vy)))
      (set new-entity.x (lume.clamp (+ x (* dx speed)) 0 1))
      (set new-entity.y (lume.clamp (+ y (* dy speed)) 0 1))
      (or (quadtree.insert new-entities new-entity)
          (quadtree.insert new-entities entity)))
    new-entities))

(var _entities nil)

(fn love.load []
  (set _entities (generate-entities entity-n))
  (love.window.setMode 800 600 {:resizable true})
  (love.window.maximize))

(fn love.update [dt]
  (let [winner (find-winner _entities)]
    (when winner
      (print (.. winner " won"))
      (set _entities (generate-entities entity-n))))
  (set _entities (next-entities _entities)))

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
    (each [{: x : y : t} (quadtree.walk _entities)]
      (match t
        :rock (do (love.graphics.setColor rock-color)
                  (love.graphics.circle :fill (scale-x x)
                                              (scale-y y)
                                              entity-half-size))
        :paper (do (love.graphics.setColor paper-color)
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

(fn love.keypressed [key]
  (when (= "escape" key)
    (love.event.quit)))
