;;; package --- Summary

;;; Commentary:
;; Soon


;;; Code:

(require 'cl-lib)
(require 'org)
(require 'json)
(require 'org-element)


(defvar habitica-base "https://habitica.com/api/v3")
(defvar habitica-uid "123") ;; replace with correct user id
(defvar habitica-token "456") ;; replace with correct token

(defvar habitica-tags '())

(defvar habitica-habit-threshold 1)

(defvar habitica-level 0)
(defvar habitica-exp 0)
(defvar habitica-max-exp 0)
(defvar habitica-hp 0)
(defvar habitica-gold 0)
(defvar habitica-silver 0)

(defvar habitica-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n"         #'habitica-new-task)
    (define-key map "t"         #'habitica-todo-task)
    (define-key map "g"         #'habitica-tasks)
    (define-key map "+"         #'habitica-up-task)
    (define-key map "-"         #'habitica-down-task)
    (define-key map "D"         #'habitica-delete-task)
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
  (if (equal (format "%s" (assoc-default 'type task)) "habit")
      (cond ((>= (assoc-default 'value task) habitica-habit-threshold) (insert "** DONE "))
            ((< (assoc-default 'value task) habitica-habit-threshold) (insert "** TODO ")))
    (cond         ((eq (assoc-default 'completed task) :json-false) (insert "** TODO "))
                  ((eq (assoc-default 'completed task) t) (insert "** DONE "))))
  (insert (assoc-default 'text task))
  (insert "\n")
  (if (and (assoc-default 'date task) (< 1 (length (assoc-default 'date task))))
      (insert (concat "   DEADLINE: <" (assoc-default 'date task) ">\n")))
  (if (< 0 (length (assoc-default 'tags task)))
      (dolist (tag (append (assoc-default 'tags task) nil))
        (org-set-tags-to (assoc-default (format "%s" tag) habitica-tags))))
  (org-set-property "ID" (assoc-default '_id task)))

(defun habitica-parse-tasks (tasks type)
  "Parse the tasks to 'org-mode' format.

TASKS is the list of tasks from the JSON response
TYPE is the type of task that you want to parse (habit, daily, or todo)."
  (dolist (value (append tasks nil))
    (if (equal (assoc-default 'type value) type)
        (habitica-insert-task value))))

(defun habitica-parse-rewards (rewards)
  "Parse the rewards to 'org-mode' format.

REWARDS is the list of rewards from the JSON response."
  (dolist (reward (append rewards nil))
    (if (equal (assoc-default 'type reward) "reward")
        (progn  (insert "** ")
                (insert (assoc-default 'text reward))
                (org-set-tags-to (format "%d" (assoc-default 'value reward)))
                (org-set-property "ID" (assoc-default '_id reward))))))

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

(defun habitica-show-notifications (profile)
  "Compare the new profile to the current one and display notifications.

PROFILE the user stats as JSON."
  (cond ((equal habitica-level (assoc-default 'lvl profile)) (message "Exp: %f" (- (assoc-default 'exp profile) habitica-exp)))
        ((< habitica-level (assoc-default 'lvl profile)) (message "Reached level %d! Exp: %f" (assoc-default 'lvl profile) (+ (- habitica-max-exp habitica-exp) (assoc-default 'exp profile))))
        ((> habitica-level (assoc-default 'lvl profile)) (message "Fell to level %d. Exp: %f" (assoc-default 'lvl profile) (* -1 (+ (- (assoc-default 'toNextLevel profile) (assoc-default 'exp profile)) habitica-exp))))))

(defun habitica-parse-profile ()
  "Formats the user profile as a header."
  (let ((profile (assoc-default 'stats (habitica-get-profile))))
    (habitica-show-notifications profile) ;show the difference in exp
    (setq habitica-level (assoc-default 'lvl profile)) ;get level
    (setq habitica-exp (assoc-default 'exp profile)) ;get exp
    (setq habitica-max-exp (assoc-default 'toNextLevel profile)) ;get max experience
    (setq habitica-hp (assoc-default 'hp profile)) ;get hp
    (setq habitica-gold (string-to-number (format "%d" (assoc-default 'gp profile)))) ;get gold
    (setq habitica-silver (string-to-number (format "%d" (* 100 (- (assoc-default 'gp profile) habitica-gold))))) ;get silver
    (insert "* Profile\n")
    (insert (concat "** Level: " (format "%d" habitica-level) "\n"))
    (insert (concat "** Class: " (assoc-default 'class profile) "\n"))
    (insert (concat "** Health: " (format "%s" habitica-hp) " / " (format "%d" (assoc-default 'maxHealth profile)) "\n"))
    (insert (concat "** Exp: " (format "%s" habitica-exp) " / " (format "%d" habitica-max-exp) "\n"))
    (insert (concat "** Gold: " (format "%d" habitica-gold) "\n"))
    (insert (concat "** Silver: " (format "%d" habitica-silver) "\n"))))

(defun habitica-refresh-profile ()
  "Kill the current profile and parse a new one."
  (save-excursion
    (progn (re-search-backward "^\* Profile" (point-min) t)
           (org-cut-subtree)
           (habitica-parse-profile))))

(defun habitica-get-tags ()
  "Get the dictionary id/tags."
  (setq habitica-tags '())
  (dolist (value (append (habitica-send-request "/tags" "GET" "") nil))
    (setq habitica-tags (cl-acons (assoc-default 'id value) (assoc-default 'name value) habitica-tags))))

(defun habitica-up-task ()
  "Up or complete a task."
  (interactive)
  (habitica-score-task (org-element-property :ID (org-element-at-point)) "up")
  (habitica-refresh-profile))

(defun habitica-down-task ()
  "Down or - a task."
  (interactive)
  (habitica-score-task (org-element-property :ID (org-element-at-point)) "down")
  (habitica-refresh-profile))

(defun habitica-todo-task ()
  "Mark the current task as done or todo depending on its current state."
  (interactive)
  (if (not (equal (buffer-name) "*habitica*"))
      (message "You must be inside the habitica buffer")
    (if (equal (format "%s" (org-element-property :todo-type (org-element-at-point))) "todo")
        (progn (habitica-up-task)
               (org-todo "DONE"))
      (progn (habitica-down-task)
             (org-todo "TODO")))))

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

(defun habitica-set-deadline ()
  "Set a deadline for a todo task."
  (interactive)
  (let ((date (replace-regexp-in-string "[a-zA-Z:.<> ]" "" (org-deadline nil))))
    (habitica-send-request (concat "/tasks/" (org-element-property :ID (org-element-at-point))) "PUT" (concat "&date=" date))))

(defun habitica-delete-task ()
  "Delete the task under the cursor."
  (interactive)
  (habitica-send-request (concat "/tasks/" (org-element-property :ID (org-element-at-point))) "DELETE" "")
  (org-cut-subtree))

(defun habitica-tasks ()
  "Main function to summon the habitica buffer."
  (interactive)
  (switch-to-buffer "*habitica*")
  (delete-region (point-min) (point-max))
  (org-mode)
  (habitica-mode)
  (insert "#+TITLE: Habitica Dashboard\n\n")
  (habitica-get-tags)
  (habitica-parse-profile)
  (let ((habitica-data (habitica-get-tasks)))
    (insert "* Habits :habit:\n")
    (habitica-parse-tasks habitica-data "habit")
    (insert "* Daily Tasks :daily:\n")
    (habitica-parse-tasks habitica-data "daily")
    (insert "* To-Dos :todo:\n")
    (habitica-parse-tasks habitica-data "todo")
    (insert "* Rewards :rewards:\n")
    (habitica-parse-rewards habitica-data)
    )
  (org-align-all-tags)
  (org-content))

(provide 'habitica)
;;; emacs-habitica.el ends here
