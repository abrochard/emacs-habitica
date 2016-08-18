# emacs-habitica
Emacs Extension for Habitica
[[https://habitica.com/]]


## Installation
Get the file.
Set your habitica user id and token as:

``` lisp
(defvar habitica-uid "123")
(defvar habitica-token "456")
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
C-x t u => + a habit
C-x t d => - a habit
```

## TODO
- task deletion
- get menu showing
- rewards integration
- pets?
- everything else
