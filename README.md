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

If you want to create a new task, navigate inside the section that you want to create the task under, start typing the name of your task on a new line and press C-x t.
