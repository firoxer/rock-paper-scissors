love-fennel
===

Minimal setup to get up and going with [LÖVE](https://love2d.org/) and [Fennel](https://fennel-lang.org/)

Motivation
---

* Jump start to 2D game development
* With a logical and capable framework
* With a language with the humble semantics of Lua and the clean syntax of a Lisp

Requirements
---

* LÖVE 11.4 installed and available in PATH

Running
---

```sh
love .
```

REPL
---

While your application is running, you can access its state via a REPL in the terminal. For example, to change the text in the example game, you can enter `(set state.text "changed")`.

Library Recommendations
---

Both libraries can be installed by just copying the relevant files to `lib/`.

* [Lume](https://github.com/rxi/lume): Since Lua (and thus Fennel) has a small standard library, you might want to install Lume to access a bunch of functions commonly used in game development.
* [tiny-ecs](https://github.com/bakpakin/tiny-ecs): For managing complexity with game worlds with numerous different kinds of entities, tiny-ecs provides an easy-to-use Entity Component System.
