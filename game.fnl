(local repl (require "lib.repl"))

(local state {:color {:r 0 :g 0 :b 0}
              :text "hello world"})
(set _G.state state) ; To make accessible from REPL

(fn love.load []
  (repl.start))

(fn invlerp [a b v]
  (/ (- v a)
     (- b a)))

(fn love.update []
  (let [time (love.timer.getTime)
        time->color #(invlerp -1 1 (math.sin $))]
    (set state.color {:r (time->color time)
                      :g (time->color (+ time (* math.pi 0.667)))
                      :b (time->color (+ time (* math.pi 1.333)))})))

(fn love.keypressed [key]
  (when (= "escape" key)
    (love.event.quit)))

(local font (love.graphics.newFont 32))

(fn love.draw []
  (let [{: color : text} state]
    (love.graphics.clear color.r color.g color.b)
    (love.graphics.print text font 16 16)))
