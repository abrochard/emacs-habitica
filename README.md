# emacs-habitica
Emacs extension for [Habitica](https://habitica.com/), a RPG style habit tracker and todo list.

[![MELPA](https://melpa.org/packages/habitica-badge.svg)](https://melpa.org/#/habitica)


## Installation
Install from MELPA with
``` lisp
M-x package-install habitica
```
or load the file [habitica.el](../master/habitica.el).

## Usage
To see your tasks, call
``` lisp
M-x habitica-tasks
```

On your first use, the extension will prompt your for your username and password. These are used to query your user id and api token from the service.

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
C-x t a => add a tag to the task
C-x t A => remove a tag from the task
C-x t g => refresh
```

## Customize
### Auto login
If you restart Emacs often, or if you just don't like entering your username or password, it is possible to bypass it by setting your user id and token directly:
``` lisp
(setq habitica-uid "123")
(setq habitica-token "456")
```
You can find your uid and token by following the instructions [here](http://habitica.wikia.com/wiki/API_Options).

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
