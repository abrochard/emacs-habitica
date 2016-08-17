;;; package --- Summary

;;; Commentary:
;; Soon


;;; Code:

(require 'org)
(require 'json)
(require 'org-element)


(defvar habitica-base "https://habitica.com/api/v3")
(defvar habitica-uid "123") ;; replace with correct user id
(defvar habitica-token "456") ;; replace with correct token


(defvar habitica-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n"         #'habitica-new-task)
    (define-key map "t"         #'habitica-todo-task)
    (define-key map "g"         #'habitica-tasks)
    map)
  "Keymap of habitica interactive commands.")

(defcustom habitica-keymap-prefix (kbd "C-x t")
  "Prefix for key bindings of habitica-mode."
  :group 'habitica
  :type 'string
  :risky t
  :set
  (lambda (variable key)
    (when (and (boundp variable) (boundp 'habitica-mode-map))
      (define-key habitica-mode-map (symbol-value variable) nil)
      (define-key habitica-mode-map key habitica-command-map))
    (set-default variable key)))


;;; Global Habitica menu
(defvar habitica-mode-menu-map
  (easy-menu-create-menu
   "Habitica"
   '(["Create a new task" habitica-new-task habitica-mode]
     ["Mark task as todo/done" habitica-todo-task habitica-mode]
     ["Refresh tasks" habitica-tasks t]))
  "Menu of command `habitica-mode'.")

(defvar habitica-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map habitica-keymap-prefix habitica-command-map)
    (define-key map [menu-bar habitica] habitica-mode-menu-map)
    map)
  "Keymap of command `habitica-mode'.")

;;; autoload

(define-minor-mode habitica-mode
  "Mode to edit and manage your Habitica tasks"
  :lighter " Habitica"
  :keymap habitica-mode-map)

(defun habitica-send-request (endpoint type data)
  "Base function to send request to the Habitica API.

ENDPOINT can be found in the habitica doc.
TYPE is the type of HTTP request (GET, POST, DELETE)
DATA is the form to be sent as x-www-form-urlencoded."
  (let ((url  (concat habitica-base endpoint))
        (url-request-method        type)
        (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded") ("x-api-user" . ,habitica-uid) ("x-api-key" . ,habitica-token)))
        (url-request-data          data))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (delete-region (point-min) (string-match-p "{" (buffer-string)))
      (assoc-default 'data (json-read-from-string (buffer-string))))))

(defun habitica-get-tasks ()
  "Gets all the user's tasks."
  (habitica-send-request "/tasks/user" "GET" ""))

(defun habitica-insert-task (task)
  "Format the task into org mode todo heading.

TASK is the parsed JSON response."
  (if (eq (assoc-default 'completed task) :json-false)
      (insert "** TODO ")
    (insert "** DONE "))
  (insert (assoc-default 'text task))
  (insert "\n")
  (if (and (assoc-default 'date task) (< 1 (length (assoc-default 'date task))))
      (insert (concat "   DEADLINE: <" (assoc-default 'date task) ">\n")))
  (org-set-property "ID" (assoc-default '_id task)))

(defun habitica-parse-tasks (tasks type)
  "Parse the tasks to 'org-mode' format.

TASKS is the list of tasks from the JSON response
TYPE is the type of task that you want to parse (habit, daily, or todo)."
  (dolist (value (append tasks nil))
    (if (equal (assoc-default 'type value) type)
        (habitica-insert-task value))))

(defun habitica-create-task (type name &optional down)
  "Send a post request to create a new user task.

TYPE is the type of task that you want to create (habit, daily, or todo)
NAME is the task name
DOWN is optional, in case of a habit, if you want to be able to downvote the task."
  (if down
      (habitica-send-request "/tasks/user" "POST" (concat "type=" type "&text=" name "&down=" down))
    (habitica-send-request "/tasks/user" "POST" (concat "type=" type "&text=" name))))

(defun habitica-get-current-type ()
  "Get the current type based on the cursor position."
  (save-excursion
    (progn (re-search-backward "^\* " (point-min) t)
           (car (org-get-tags-at)))))

(defun habitica-score-task (id direction)
  "Send a post request to score a task.

ID is the id of the task that you are scoring
DIRECTION is up or down, if the task is a habit."
  (habitica-send-request (concat "/tasks/" id "/score/" direction) "POST" ""))

(defun habitica-get-profile ()
  "Get the user's raw profile data."
  (habitica-send-request "/user" "GET" ""))

(defun habitica-parse-profile ()
  "Formats the user profile as a header."
  (let ((profile (assoc-default 'stats (habitica-get-profile))))
    (insert "* Profile\n")
    (insert (concat "** Level: " (format "%d" (assoc-default 'lvl profile)) "\n"))
    (insert (concat "** Class: " (assoc-default 'class profile) "\n"))
    (insert (concat "** Health: " (format "%s" (assoc-default 'hp profile)) " / " (format "%d" (assoc-default 'maxHealth profile)) "\n"))
    (insert (concat "** Exp: " (format "%s" (assoc-default 'exp profile)) " / " (format "%d" (assoc-default 'toNextLevel profile)) "\n"))
    (let ((gold (format "%d" (assoc-default 'gp profile))))
      (insert (concat "** Gold: " gold "\n"))
      (insert (concat "** Silver: " (format "%d" (* 100 (- (assoc-default 'gp profile) (string-to-number gold)))) "\n"))
      )
    ))

(defun habitica-refresh-profile ()
  "Kill the current profile and parse a new one."
  (save-excursion
    (progn (re-search-backward "^\* Profile" (point-min) t)
           (org-cut-subtree)
           (habitica-parse-profile))))

(defun habitica-todo-task ()
  "Mark the current task as done or todo depending on its current state."
  (interactive)
  (if (not (equal (buffer-name) "*habitica*"))
      (message "You must be inside the habitica buffer")
    (if (equal (format "%s" (org-element-property :todo-type (org-element-at-point))) "todo")
        (progn (habitica-score-task (org-element-property :ID (org-element-at-point)) "up")
               (org-todo "DONE")
               (habitica-refresh-profile))
      (progn (habitica-score-task (org-element-property :ID (org-element-at-point)) "down")
             (org-todo "TODO")
             (habitica-refresh-profile)))
      ))

(defun habitica-new-task (name)
  "Attempt to be smart to create a new task based on context.

NAME is the name of the new task to create."
  (interactive "sEnter the task name: ")
  (if (not (equal (buffer-name) "*habitica*"))
      (message "You must be inside the habitica buffer")
    (progn (end-of-line)
           (newline)
           (habitica-insert-task (habitica-create-task (habitica-get-current-type) name))
           (org-content))))

(defun habitica-tasks ()
  "Main function to summon the habitica buffer."
  (interactive)
  (switch-to-buffer "*habitica*")
  (delete-region (point-min) (point-max))
  (org-mode)
  (habitica-mode)
  (insert "#+TITLE: Habitica Dashboard\n\n")
  (habitica-parse-profile)
  (let ((habitica-data (habitica-get-tasks)))
    (insert "* Habits :habit:\n")
    (habitica-parse-tasks habitica-data "habit")
    (insert "* Daily Tasks :daily:\n")
    (habitica-parse-tasks habitica-data "daily")
    (insert "* To-Dos :todo:\n")
    (habitica-parse-tasks habitica-data "todo")
    )
  (org-align-all-tags)
  (org-content)
  )

(provide 'habitica)
;;; emacs-habitica.el ends here
