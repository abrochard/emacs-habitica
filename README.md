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

If you want to create a new task, navigate inside the section that you want to create the task under, and press C-x t n.
To mark a task as done, place cursor on line and press C-x t t.

## TODO
- task deletion
- up/down habits
- get menu showing
- get the refresh keybinding to work
- rewards integration
- pets?
- everything else
