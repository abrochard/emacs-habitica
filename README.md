# emacs-habitica
Emacs Extension for Habitica
[[https://habitica.com/]]


## Installation
Get the file.
Set your habitica user id and token as:

``` lisp
(setq habitica-uid "123")
(setq habitica-token "456")
```

## Usage
To see your tasks, call
``` lisp
habitica-tasks
```
### Shortcuts
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
