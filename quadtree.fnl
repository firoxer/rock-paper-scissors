(fn leaf? [node]
  (= nil node.nw))

(fn empty? [node]
  (and (= nil node.nw)
       (= nil node.value)))

(fn branch? [node]
  (not= nil node.nw))

(fn create-leaf [x1 y1 size]
  (let [x2 (+ x1 size)
        y2 (+ y1 size)]
    {:value nil
     : x1
     : y1
     : x2
     : y2}))

(fn new []
  (create-leaf 0 0 1))

(fn within-bounds? [node x y]
  (and (if (= node.x2 1)
         (and (<= node.x1 x) (<= x node.x2)) ; Without this, nodes with x=1 cause errors
         (and (<= node.x1 x) (< x node.x2)))
       (if (= node.y2 1)
         (and (<= node.y1 y) (<= y node.y2)) ; Without this, nodes with y=1 cause errors
         (and (<= node.y1 y) (< y node.y2)))))

(fn find-subnode [node x y]
  (if (within-bounds? node.nw x y) node.nw
      (within-bounds? node.ne x y) node.ne
      (within-bounds? node.sw x y) node.sw
      (within-bounds? node.se x y) node.se))

(var insert nil) ; Forward declaration

(fn leaf->branch [node]
  (let [{: x1 : y1 : x2} node
        half-size (/ (- x2 x1) 2)]
    (set node.nw (create-leaf x1 y1 half-size))
    (set node.ne (create-leaf (+ x1 half-size) y1 half-size))
    (set node.sw (create-leaf x1 (+ y1 half-size) half-size))
    (set node.se (create-leaf (+ x1 half-size) (+ y1 half-size) half-size)))
  (insert (find-subnode node node.value.x node.value.y) node.value)
  (set node.value nil))

(fn branch->leaf [node]
  (set node.nw nil)
  (set node.ne nil)
  (set node.sw nil)
  (set node.se nil))

(set insert
  (fn [node value]
    (if (empty? node)
      (do (set node.value value)
          true)
      (if (and node.value
               (= value.x node.value.x)
               (= value.y node.value.y))
        false
        (do (when (leaf? node)
              (leaf->branch node))
            (insert (find-subnode node value.x value.y) value))))))

(fn walk* [node]
  (if (leaf? node)
    (when node.value (coroutine.yield node.value))
    (do (when node.nw (walk* node.nw))
        (when node.ne (walk* node.ne))
        (when node.sw (walk* node.sw))
        (when node.se (walk* node.se)))))

(fn walk [node]
  (coroutine.wrap (fn []
                    (walk* node))))

(fn size [node]
  (if (leaf? node)
    (if node.value 1 0)
    (+ (size node.nw)
       (size node.ne)
       (size node.sw)
       (size node.se))))

(fn remove [node value]
  (when (within-bounds? node value.x value.y)
    (if (leaf? node)
      (set node.value nil)
      (do (remove node.nw value)
          (remove node.ne value)
          (remove node.sw value)
          (remove node.se value)
          (when (and (empty? node.nw)
                     (empty? node.ne)
                     (empty? node.sw)
                     (empty? node.se))
            (branch->leaf node))))))

(fn distance [x1 y1 x2 y2]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (+ (* dx dx) (* dy dy))))

(fn quad-distance [x y x1 y1 x2 y2]
  (let [dx (math.max 0 (- x1 x) (- x x2))
        dy (math.max 0 (- y1 y) (- y y2))]
    (+ (* dx dx) (* dy dy))))

(fn score-nn [x y node]
  (if (leaf? node)
    (distance x y node.value.x node.value.y)
    (quad-distance x y node.x1 node.y1 node.x2 node.y2)))

(fn nearest-neighbors* [k best x y {: ignore-exact? &as opts}]
  (table.sort best #(> $1.nn-score $2.nn-score))
  (var done false)
  (var next-k k)
  (for [i (length best) 1 -1 &until done]
    (let [elem (. best i)]
      (if (leaf? elem)
        (let [to-yield (table.remove best)]
          (when (or (= ignore-exact? false)
                    (not= elem.value.x x) (not= elem.value.y y))
            (coroutine.yield to-yield.value)
            (set next-k (- next-k 1))
            (when (= 0 next-k)
              (lua "return"))))
        (set done true))))
  (let [{: nw : ne : sw : se} (table.remove best)]
    (when (not (empty? nw))
      (set nw.nn-score (score-nn x y nw))
      (table.insert best nw))
    (when (not (empty? ne))
      (set ne.nn-score (score-nn x y ne))
      (table.insert best ne))
    (when (not (empty? sw))
      (set sw.nn-score (score-nn x y sw))
      (table.insert best sw))
    (when (not (empty? se))
      (set se.nn-score (score-nn x y se))
      (table.insert best se)))
  (nearest-neighbors* next-k best x y opts))

(fn nearest-neighbors [k node x y opts]
  (let [opts* (or opts {:ignore-exact? false})]
    (coroutine.wrap (fn []
                      (nearest-neighbors* k [node] x y opts*)))))

(fn nearest-neighbor [node x y opts]
  ((nearest-neighbors 1 node x y opts)))

{: new
 : insert
 : remove
 : empty?
 : walk
 : size
 : nearest-neighbors
 : nearest-neighbor}
