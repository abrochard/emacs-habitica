# emacs-habitica
Emacs extension for [Habitica](https://habitica.com/), a RPG style habit tracker and todo list.


## Installation
Load the file [habitica.el](../habitica.el) and set your habitica user id and token as:
``` lisp
(setq habitica-uid "123")
(setq habitica-token "456")
```
You can find your uid and token by following the instructions [here](http://habitica.wikia.com/wiki/API_Options).

## Usage
To see your tasks, call
``` lisp
M-x habitica-tasks
```

## Shortcuts
Place your cursor on the task
``` lisp
C-x t n => new task
C-x t t => cycle todo/done
C-x t + => + a habit
C-x t - => - a habit
C-x t d => set deadline
C-x t i => set difficulty
C-x t D => delete the task
C-x t b => buy reward
C-x t g => refresh
```

## Customize
### Highlithing
If you want to try highlighting tasks based on their value
``` lisp
(setq habitica-turn-on-highlighting t)
```
This is very experimental.

### Streak count
If you want the streak count to appear as a tag for your daily tasks
``` lisp
(setq habitica-show-streak t)
```
