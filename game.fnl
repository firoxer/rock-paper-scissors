(local lume (require :lib.lume))
(local profile (require :lib.profile))

(local quadtree (require :quadtree))

(local entity-n 100)
(local minimum-eat-distance (math.pow 0.02 2)) ; precalculated for performance

(fn close-enough-to-eat? [a b]
  (let [squared-distance (lume.distance a.x a.y b.x b.y true)]
    (< 0 squared-distance minimum-eat-distance)))

(fn next-entities [entities eaten-by eats]
  (local new-entities (quadtree.new))
  (each [{: x : y : t &as entity} (quadtree.walk entities)]
    (var changes-tree-to nil)
    (var dx 0)
    (var dy 0)
    (each [neighbor (quadtree.nearest-neighbors 1 eaten-by x y)]
      (when (close-enough-to-eat? neighbor entity)
        (set changes-tree-to eaten-by))
      (let [magnitude -1 
            (vx vy) (lume.vector (lume.angle x y neighbor.x neighbor.y) magnitude)]
        (set dx (+ dx vx))
        (set dy (+ dy vy))))
    (each [neighbor (quadtree.nearest-neighbors 1 eats x y)]
      (let [magnitude 1
            (vx vy) (lume.vector (lume.angle x y neighbor.x neighbor.y) magnitude)]
        (set dx (+ dx vx))
        (set dy (+ dy vy))))
    (each [neighbor (quadtree.nearest-neighbors 1 entities x y {:ignore-exact? true})]
      (let [magnitude -0.1
            (vx vy) (lume.vector (lume.angle x y neighbor.x neighbor.y) magnitude)]
        (set dx (+ dx vx))
        (set dy (+ dy vy))))
    ; Pull to middle
    (let [(vx vy) (lume.vector (lume.angle x y 0.5 0.5)
                               (lume.distance x y 0.5 0.5 true))]
      (set dx (+ dx vx))
      (set dy (+ dy vy)))
    (let [speed 0.001
          new-x (lume.clamp (+ x (* dx speed)) 0 1)
          new-y (lume.clamp (+ y (* dy speed)) 0 1)
          new-tree (or changes-tree-to new-entities)]
      (or (quadtree.insert new-tree {:x new-x :y new-y})
          (quadtree.insert new-tree entity))))
  new-entities)

(fn generate-entities [n t]
  (local entities (quadtree.new))
  (for [_ 1 n]
    (quadtree.insert entities {:x (love.math.random)
                               :y (love.math.random)}))
  entities)

(var _rock nil)
(var _paper nil)
(var _scissors nil)

(fn love.load []
  (set _rock (generate-entities entity-n :rock))
  (set _paper (generate-entities entity-n :paper))
  (set _scissors (generate-entities entity-n :scissors))
  (love.window.setMode 800 600 {:resizable true})
  (love.window.maximize))

(fn find-winner [rock paper scissors]
  (if (quadtree.empty? rock) :paper
      (quadtree.empty? paper) :scissors
      (quadtree.empty? scissors) :rock))

(fn love.update [dt]
  (let [winner (find-winner _rock _paper _scissors)]
    (when winner
      (print (.. winner " won"))
      (set _rock (generate-entities entity-n :rock))
      (set _paper (generate-entities entity-n :paper))
      (set _scissors (generate-entities entity-n :scissors))))
  (set _rock (next-entities _rock _paper _scissors))
  (set _paper (next-entities _paper _scissors _rock))
  (set _scissors (next-entities _scissors _rock _paper)))

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
    (each [{: x : y : t} (quadtree.walk _rock)]
      (love.graphics.setColor rock-color)
      (love.graphics.circle :fill (scale-x x)
                                  (scale-y y)
                                  entity-half-size))
    (each [{: x : y : t} (quadtree.walk _paper)]
      (love.graphics.setColor paper-color)
      (love.graphics.rectangle :fill (- (scale-x x) entity-half-size)
                                     (- (scale-y y) entity-half-size)
                                     entity-size
                                     entity-size))
    (each [{: x : y : t} (quadtree.walk _scissors)]
      (love.graphics.setColor scissors-color)
      (love.graphics.line (- (scale-x x) entity-half-size) (- (scale-y y) entity-half-size)
                             (+ (scale-x x) entity-half-size) (+ (scale-y y) entity-half-size))
      (love.graphics.line (- (scale-x x) entity-half-size) (+ (scale-y y) entity-half-size)
                              (+ (scale-x x) entity-half-size) (- (scale-y y) entity-half-size)))
    (love.graphics.setColor [0 0 0])
    (love.graphics.print (love.timer.getFPS) 16 16)))

(fn love.keypressed [key]
  (when (= "escape" key)
    (love.event.quit)))
