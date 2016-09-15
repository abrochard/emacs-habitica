;;; habitica.el --- Interface for habitica.com

;; Copyright (C) 2016, Adrien Brochard

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 1.0
;; Author: Adrien Brochard
;; Keywords: habitica todo
;; URL: https://github.com/abrochard/emacs-habitica
;; License: GNU General Public License >= 3
;; Package-Requires: ((org "8.3.5") (emacs "24.3"))

;;; Commentary:

;; Emacs extension for [Habitica](https://habitica.com/), a RPG style habit tracker and todo list.

;;; Install:

;; Install from MELPA with
;;
;; M-x package-install habitica
;;
;; or load the present file.

;;; Usage:

;; To see your tasks, call
;;
;; M-x habitica-tasks
;;
;; On your first use, the extension will prompt your for your username and password.
;; These are used to query your user id and api token from the service.

;;; Shortcuts:

;; Place your cursor on the task
;;
;; C-x t n => new task
;; C-x t t => cycle todo/done
;; C-x t + => + a habit
;; C-x t - => - a habit
;; C-x t d => set deadline
;; C-x t i => set difficulty
;; C-x t D => delete the task
;; C-x t b => buy reward
;; C-x t g => refresh
;;

;;; Customize:

;; Auto login
;; If you restart Emacs often, or if you just don't like entering your username or password, it is possible to bypass it by setting your user id and token directly:
;;
;; (setq habitica-uid "123")
;; (setq habitica-token "456")
;;
;; You can find your uid and token by following the instructions [here](http://habitica.wikia.com/wiki/API_Options).

;; Highlithing
;; If you want to try highlighting tasks based on their value
;;
;; (setq habitica-turn-on-highlighting t)
;;
;; This is very experimental.

;; Streak count
;; If you want the streak count to appear as a tag for your daily tasks
;;
;; (setq habitica-show-streak t)
;;

;;; Code:

;;;; Consts
(defconst habitica-version "1.0" "Habitica version.")

(defgroup habitica nil
  "Interface for habitica.com, a RPG based task management system."
  :group 'extensions
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/abrochard/emacs-habitica"))

;;;; libraries
(require 'cl-lib)
(require 'easymenu)
(require 'json)
(require 'org)
(require 'org-element)


;;;; Variables
(defvar habitica-base "https://habitica.com/api/v3")
(defvar habitica-uid nil)
(defvar habitica-token nil)

(defvar habitica-tags '())

(defvar habitica-habit-threshold 1
  "This is the threshold used to consider a habit as done.")

(defvar habitica-turn-on-highlighting nil)
(defvar habitica-color-threshold
  '(("hi-red-b" . -10) ("hi-yellow" . 0) ("hi-green" . 5) ("hi-blue" . 10)))

(defvar habitica-show-streak nil)

(defvar habitica-level 0)
(defvar habitica-exp 0)
(defvar habitica-max-exp 0)
(defvar habitica-hp 0)
(defvar habitica-max-hp 0)
(defvar habitica-mp 0)
(defvar habitica-max-mp 0)
(defvar habitica-gold 0)
(defvar habitica-silver 0)

(defvar habitica-status-bar-length 20)

(defvar habitica-difficulty '((1 . "easy") (1.5 . "medium") (2 . "hard"))
  "Assoc list of priority/difficulty.")

(defvar habitica-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n"         #'habitica-new-task)
    (define-key map "t"         #'habitica-todo-task)
    (define-key map "g"         #'habitica-tasks)
    (define-key map "+"         #'habitica-up-task)
    (define-key map "-"         #'habitica-down-task)
    (define-key map "D"         #'habitica-delete-task)
    (define-key map "d"         #'habitica-set-deadline)
    (define-key map "b"         #'habitica-buy-reward)
    (define-key map "i"         #'habitica-set-difficulty)
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
     ["+ a habit" habitica-up-task]
     ["- a habit" habitica-down-task]
     ["Set deadline for todo" habitica-set-deadline]
     ["Set difficulty for task" habitica-set-difficulty]
     ["Buy reward" habitica-buy-reward]
     ["Delete a task" habitica-delete-task]
     ["Refresh tasks" habitica-tasks t]))
  "Menu of command `habitica-mode'.")

(easy-menu-add-item nil '("Tools") habitica-mode-menu-map "Habitica")

(defvar habitica-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map habitica-keymap-prefix habitica-command-map)
    (define-key map [menu-bar habitica] habitica-mode-menu-map)
    map)
  "Keymap of command `habitica-mode'.")

;;; Function
;;;; Utilities
(defun habitica--send-request (endpoint type data)
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
      (assoc-default 'data (json-read-from-string (decode-coding-string
                                                   (buffer-string)
                                                   'utf-8))))))

(defun habitica--get-tasks ()
  "Gets all the user's tasks."
  (habitica--send-request "/tasks/user" "GET" ""))

(defun habitica--insert-todo (task)
  "Logic to insert TODO or DONE for a task.

TASK is the parsed JSON response."
  (if (equal (format "%s" (assoc-default 'type task)) "habit")
      (cond ((>= (assoc-default 'value task) habitica-habit-threshold) (insert "** DONE "))
            ((< (assoc-default 'value task) habitica-habit-threshold) (insert "** TODO ")))
    (cond         ((eq (assoc-default 'completed task) :json-false) (insert "** TODO "))
                  ((eq (assoc-default 'completed task) t) (insert "** DONE ")))))

(defun habitica--insert-deadline (task)
  "Insert the deadline for a particular task.

TASK is the parsed JSON response."
  (if (and (assoc-default 'date task) (< 1 (length (assoc-default 'date task))))
      (insert (concat "   DEADLINE: <" (assoc-default 'date task) ">\n"))))

(defun habitica--insert-tags (task)
  "Insert the tags and difficulty for a particular task.

TASK is the parsed JSON reponse."
  (org-set-tags-to (append
                    (mapcar (lambda (arg)
                              (assoc-default (format "%s" arg) habitica-tags))
                            (assoc-default 'tags task))
                    (list (assoc-default (assoc-default 'priority task) habitica-difficulty))
                    (habitica--get-streak-as-list task))))

(defun habitica--get-streak-as-list (task)
  "Get the streak formated as a single element list.

TASK is the parsed JSON reponse."
  (if (and habitica-show-streak (assoc-default 'streak task) (< 0 (assoc-default 'streak task)))
      (list (format "%s" (assoc-default 'streak task)))
    '()))

(defun habitica--update-streak (increment)
  "Update the streak count for a task.

INCREMENT is what to add to the streak count."
  (let ((new-tags '()))
    (dolist (tag (org-get-tags))
      (message "%s" new-tags)
      (if (string-match-p "[0-9]+" tag)
          (setq new-tags (push (format "%s" (+ increment (string-to-number tag))) new-tags))
        (setq new-tags (push tag new-tags))))
    (org-set-tags-to new-tags)))

(defun habitica--highlight-task (task)
  "Highlight the task using its value and user defined thresholds.

TASK is the parsed JSON reponse."
  (dolist (value habitica-color-threshold)
    (if (<= (assoc-default 'value task) (cdr value))
        (progn (highlight-regexp (assoc-default 'text task) (car value))
               (throw 'aaa nil))))
  (highlight-regexp (assoc-default 'text task) (car (car (last habitica-color-threshold)))))

(defun habitica--insert-task (task)
  "Format the task into org mode todo heading.

TASK is the parsed JSON response."
  (habitica--insert-todo task)
  (insert (assoc-default 'text task))
  (insert "\n")
  (habitica--insert-deadline task)
  (habitica--insert-tags task)
  (org-set-property "ID" (assoc-default '_id task))
  (org-set-property "value" (format "%s" (assoc-default 'value task)))
  (if habitica-turn-on-highlighting
      (catch 'aaa
        (habitica--highlight-task task))
      )
  )

(defun habitica--parse-tasks (tasks order)
  "Parse the tasks to 'org-mode' format.

TASKS is the list of tasks from the JSON response
ORDER is the ordered list of ids to print the task in."
  (dolist (id (append order nil))
    (dolist (value (append tasks nil))
      (if (equal (assoc-default 'id value) id)
          (habitica--insert-task value)))))

(defun habitica--parse-rewards (rewards order)
  "Parse the rewards to 'org-mode' format.

REWARDS is the list of rewards from the JSON response
ORDER is the ordered list of ids to print the rewards in."
  (dolist (id (append order nil))
    (dolist (reward (append rewards nil))
      (if (equal (assoc-default 'id reward) id)
          (progn  (insert "** ")
                  (insert (assoc-default 'text reward))
                  (org-set-tags-to (format "%d" (assoc-default 'value reward)))
                  (org-set-property "ID" (assoc-default '_id reward)))))))

(defun habitica--create-task (type name &optional down)
  "Send a post request to create a new user task.

TYPE is the type of task that you want to create (habit, daily, or todo)
NAME is the task name
DOWN is optional, in case of a habit, if you want to be able to downvote the task."
  (if down
      (habitica--send-request "/tasks/user" "POST" (concat "type=" type "&text=" name "&down=" down))
    (habitica--send-request "/tasks/user" "POST" (concat "type=" type "&text=" name))))

(defun habitica--get-current-type ()
  "Get the current type based on the cursor position."
  (save-excursion
    (progn (re-search-backward "^\* " (point-min) t)
           (car (org-get-tags-at)))))

(defun habitica--score-task (id direction)
  "Send a post request to score a task.

ID is the id of the task that you are scoring
DIRECTION is up or down, if the task is a habit."
  (habitica--send-request (concat "/tasks/" id "/score/" direction) "POST" ""))

(defun habitica--get-profile ()
  "Get the user's raw profile data."
  (habitica--send-request "/user" "GET" ""))

(defun habitica--show-notifications (profile)
  "Compare the new profile to the current one and display notifications.

PROFILE the user stats as JSON."
  (cond ((equal habitica-level (assoc-default 'lvl profile))
         (message "Exp: %f" (- (assoc-default 'exp profile) habitica-exp)))
        ((< habitica-level (assoc-default 'lvl profile))
         (message "Reached level %d! Exp: %f" (assoc-default 'lvl profile) (+ (- habitica-max-exp habitica-exp) (assoc-default 'exp profile))))
        ((> habitica-level (assoc-default 'lvl profile))
         (message "Fell to level %d. Exp: %f" (assoc-default 'lvl profile)
                  (* -1 (+ (- (assoc-default 'toNextLevel profile) (assoc-default 'exp profile)) habitica-exp))))))

(defun habitica--set-profile (profile)
  "Set the profile variables.

PROFILE is the JSON formatted response."
    (setq habitica-level (assoc-default 'lvl profile)) ;get level
    (setq habitica-exp (fround (assoc-default 'exp profile))) ;get exp
    (setq habitica-max-exp (assoc-default 'toNextLevel profile)) ;get max experience
    (setq habitica-hp (fround (assoc-default 'hp profile))) ;get hp
    (setq habitica-max-hp (assoc-default 'maxHealth profile)) ;get max hp
    (setq habitica-mp (fround (assoc-default 'mp profile))) ;get mp
    (setq habitica-max-mp (assoc-default 'maxMP profile)) ;get max mp
    (setq habitica-gold (string-to-number (format "%d" (assoc-default 'gp profile)))) ;get gold
    (setq habitica-silver (string-to-number (format "%d" (* 100 (- (assoc-default 'gp profile) habitica-gold))))) ;get silver
    )

(defun habitica--format-status-bar (current max length)
  "Formats the current value as an ASCII progress bar.

CURRENT is the current value
MAX is the max value
LENGTH is the total number of characters in the bar."
  (concat "["
          (make-string (truncate (fround (* (/ current max) length))) ?#)
          (make-string (truncate (fround (* (/ (- max current) max) length))) ?-)
          "]"))

(defun habitica--parse-profile (profile)
  "Formats the user stats as a header.

PROFILE is the JSON profile data"
  (let ((stats (assoc-default 'stats profile)))
    (habitica--show-notifications stats) ;show the difference in exp
    (habitica--set-profile stats)
    (insert "* Stats\n")
    (insert (concat "** Level  : " (format "%d" habitica-level) "\n"))
    (insert (concat "** Class  : " (assoc-default 'class stats) "\n"))
    (insert (concat "** Health : "
                    (habitica--format-status-bar habitica-hp habitica-max-hp habitica-status-bar-length)
                    "  "
                    (format "%d" habitica-hp) " / " (format "%d" habitica-max-hp) "\n"))
    (insert (concat "** Exp    : "
                    (habitica--format-status-bar habitica-exp habitica-max-exp habitica-status-bar-length)
                    "  "
                    (format "%d" habitica-exp) " / " (format "%d" habitica-max-exp) "\n"))
    (if (<= 10 habitica-level)
        (insert (concat "** Mana   : "
                        (habitica--format-status-bar habitica-mp habitica-max-mp habitica-status-bar-length)
                        "  "
                        (format "%d" habitica-mp) " / " (format "%d" habitica-max-mp) "\n")))
    (insert (concat "** Gold   : " (format "%d" habitica-gold) "\n"))
    (insert (concat "** Silver : " (format "%d" habitica-silver) "\n"))))

(defun habitica--refresh-profile ()
  "Kill the current profile and parse a new one."
  (save-excursion
    (progn (re-search-backward "^\* Stats" (point-min) t)
           (org-cut-subtree)
           (habitica--parse-profile (habitica--get-profile)))))

(defun habitica--get-tags ()
  "Get the dictionary id/tags."
  (setq habitica-tags '())
  (dolist (value (append (habitica--send-request "/tags" "GET" "") nil))
    (setq habitica-tags (cl-acons (assoc-default 'id value) (assoc-default 'name value) habitica-tags))))


;;;; Interactive
(defun habitica-up-task ()
  "Up or complete a task."
  (interactive)
  (let ((result (habitica--score-task (org-element-property :ID (org-element-at-point)) "up"))
        (current-value (string-to-number (org-element-property :VALUE (org-element-at-point)))))
    (if (< habitica-habit-threshold (+ current-value (assoc-default 'delta result)))
        (org-todo "DONE"))
    (org-set-property "value" (number-to-string (+ current-value (assoc-default 'delta result)))))
  (habitica--refresh-profile))

(defun habitica-down-task ()
  "Down or - a task."
  (interactive)
  (let ((result (habitica--score-task (org-element-property :ID (org-element-at-point)) "down"))
        (current-value (string-to-number (org-element-property :VALUE (org-element-at-point)))))
    (if (> habitica-habit-threshold (+ current-value (assoc-default 'delta result)))
        (org-todo "TODO"))
    (org-set-property "value" (number-to-string (+ current-value (assoc-default 'delta result)))))
  (habitica--refresh-profile))

(defun habitica-todo-task ()
  "Mark the current task as done or todo depending on its current state."
  (interactive)
  (if (not (equal (buffer-name) "*habitica*"))
      (message "You must be inside the habitica buffer")
    (if (equal (format "%s" (org-element-property :todo-type (org-element-at-point))) "todo")
        (progn (habitica-up-task)
               (if habitica-show-streak
                   (habitica--update-streak 1))
               (org-todo "DONE"))
      (progn (habitica-down-task)
             (if habitica-show-streak
                 (habitica--update-streak -1))
             (org-todo "TODO")))))

(defun habitica-new-task (name)
  "Attempt to be smart to create a new task based on context.

NAME is the name of the new task to create."
  (interactive "sEnter the task name: ")
  (if (not (equal (buffer-name) "*habitica*"))
      (message "You must be inside the habitica buffer")
    (progn (end-of-line)
           (newline)
           (habitica--insert-task (habitica--create-task (habitica--get-current-type) name))
           (org-content))))

(defun habitica-set-deadline ()
  "Set a deadline for a todo task."
  (interactive)
  (let ((date (replace-regexp-in-string "[a-zA-Z:.<> ]" "" (org-deadline nil))))
    (habitica--send-request (concat "/tasks/" (org-element-property :ID (org-element-at-point))) "PUT" (concat "&date=" date))))

(defun habitica-set-difficulty (level)
  "Set a difficulty level for a task.

LEVEL index from 1 to 3."
  (interactive "nEnter the difficulty level, 1 (easy) 2 (medium) 3 (hard): ")
  (let ((task (habitica--send-request (concat "/tasks/" (org-element-property :ID (org-element-at-point))) "PUT"
                                     (concat "&priority="
                                             (format "%s" (car (nth (- level 1) habitica-difficulty)))))))
    (beginning-of-line)
    (kill-line)
    (habitica--insert-task task)
    (org-content)))

(defun habitica-delete-task ()
  "Delete the task under the cursor."
  (interactive)
  (habitica--send-request (concat "/tasks/" (org-element-property :ID (org-element-at-point))) "DELETE" "")
  (org-cut-subtree))

(defun habitica-buy-reward ()
  "Use the up function to buy a reward."
  (interactive)
  (habitica-up-task)
  (message "Bought reward %s" (org-element-property :raw-value (org-element-at-point))))

(defun habitica-login (username)
  "Login and retrives the user id and api token.

USERNAME is the user's username."
  (interactive "sEnter your Habitica username: ")
  (setq habitica-uid nil)
  (setq habitica-token nil)
  (let ((password (read-passwd "Enter your password: ")))
    (let ((url "https://habitica.com/api/v3/user/auth/local/login")
          (url-request-method "POST")
          (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
          (url-request-data (concat "username=" username "&password=" password))
          (data nil))
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (delete-region (point-min) (string-match-p "{" (buffer-string)))
        (setq data (assoc-default 'data
                                  (json-read-from-string (decode-coding-string (buffer-string) 'utf-8))))
        (setq habitica-uid (assoc-default 'id data))
        (setq habitica-token (assoc-default 'apiToken data)))))
  (if (and habitica-uid habitica-token)
      (message "Successfully logged in.")
    (message "Error logging in.")))

;;;###autoload
(defun habitica-tasks ()
  "Main function to summon the habitica buffer."
  (interactive)
  (if (or (not habitica-uid) (not habitica-token))
      (call-interactively 'habitica-login))
  (switch-to-buffer "*habitica*")
  (delete-region (point-min) (point-max))
  (org-mode)
  (habitica-mode)
  (insert "#+TITLE: Habitica Dashboard\n\n")
  (habitica--get-tags)
  (let ((habitica-data (habitica--get-tasks))
        (habitica-profile (habitica--get-profile)))
    (habitica--parse-profile habitica-profile)
    (let ((tasksOrder (assoc-default 'tasksOrder habitica-profile)))
      (insert "* Habits :habit:\n")
      (habitica--parse-tasks habitica-data (assoc-default 'habits tasksOrder))
      (insert "* Daily Tasks :daily:\n")
      (habitica--parse-tasks habitica-data (assoc-default 'dailys tasksOrder))
      (insert "* To-Dos :todo:\n")
      (habitica--parse-tasks habitica-data (assoc-default 'todos tasksOrder))
      (insert "* Rewards :rewards:\n")
      (habitica--parse-rewards habitica-data (assoc-default 'rewards tasksOrder))))
  (org-align-all-tags)
  (org-content))

(define-minor-mode habitica-mode
  "Mode to edit and manage your Habitica tasks"
  :lighter " Habitica"
  :keymap habitica-mode-map)

(provide 'habitica)
;;; habitica.el ends here
