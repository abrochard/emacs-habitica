


(require 'json)


(defvar habitica-base "https://habitica.com/api/v3")
;; (defvar habitica-uid "123") ;; replace with correct user id
;; (defvar habitica-token "456") ;; replace with correct token


(defvar habitica-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n"         #'habitica-new-task)
    (define-key map "t"         #'habitica-todo-task)
    map)
  "Keymap of habitica interactive commands.")

(defcustom habitica-keymap-prefix (kbd "C-x t")
  "Prefix for key bindings of habitica-mode"
  :group 'habitica
  :type 'string
  :risky t
  :set
  (lambda (variable key)
    (when (and (boundp variable) (boundp 'habitica-mode-map))
      (define-key habitica-mode-map (symbol-value variable) nil)
      (define-key habitica-mode-map key habitica-command-map))
    (set-default variable key)))

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
  "Base function to send request to the Habitica API"
  (let ((url  (concat habitica-base endpoint))
        (url-request-method        type)
        (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded") ("x-api-user" . ,habitica-uid) ("x-api-key" . ,habitica-token)))
        (url-request-data          data))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (delete-region (point-min) (string-match-p "{" (buffer-string)))
      (assoc-default 'data (json-read-from-string (buffer-string))))))

(defun habitica-get-tasks ()
  "Gets all the user's tasks"
  (habitica-send-request "/tasks/user" "GET" ""))

(defun habitica-insert-task (task)
  "Formats the task into org mode todo heading"
  (if (eq (assoc-default 'completed task) :json-false)
      (insert "** TODO ")
    (insert "** DONE "))
  (insert (assoc-default 'text task))
  (insert "\n")
  (if (and (assoc-default 'date task) (< 1 (length (assoc-default 'date task))))
      (insert (concat "   DEADLINE: <" (assoc-default 'date value) ">\n")))
  (org-set-property "ID" (assoc-default '_id task)))

(defun habitica-parse-tasks (tasks type)
  "Parses the tasks to org-mode format"
  (dolist (value (append tasks nil))
    (if (equal (assoc-default 'type value) type)
        (habitica-insert-task value))))

(defun habitica-create-task (type text &optional down)
  "Sends a post request to create a new user task"
  (if down
      (habitica-send-request "/tasks/user" "POST" (concat "type=" type "&text=" text "&down=" down))
    (habitica-send-request "/tasks/user" "POST" (concat "type=" type "&text=" text))))

(defun habitica-get-current-type ()
  "Get the current type based on the cursor position"
  (save-excursion
    (progn (re-search-backward "^\* " (point-min) t)
           (car (org-get-tags-at)))))

(defun habitica-score-task (id direction)
  "Sends a post reauest to score a task"
  (habitica-send-request (concat "/tasks/" id "/score/" direction) "POST" ""))

(defun habitica-get-profile ()
  "Get the user's raw profile data"
  (habitica-send-request "/user" "GET" ""))

(defun habitica-parse-profile ()
  "Formats the user profile as a header"
  (let ((profile (assoc-default 'stats (habitica-get-profile))))
    (insert "* Profile\n")
    (insert (concat "** Level: " (format "%d" (assoc-default 'lvl profile)) "\n"))
    (insert (concat "** Class: " (assoc-default 'class profile) "\n"))
    (insert (concat "** Health: " (format "%s" (assoc-default 'hp profile)) " / " (format "%d" (assoc-default 'maxHealth profile)) "\n"))
    (insert (concat "** Exp: " (format "%s" (assoc-default 'exp profile)) " / " (format "%d" (assoc-default 'toNextLevel profile)) "\n"))
    (let ((gold (format "%d" (assoc-default 'gp profile))))
      (insert (concat "** Gold: " gold "\n"))
      (insert (concat "** Silver: " (format "%d" (* 10 (- (assoc-default 'gp profile) (string-to-number gold)))) "\n"))
      )
    ))

(defun habitica-todo-task ()
  "Marks the current task as done or todo depending on its current state"
  (interactive)
  (if (not (equal (buffer-name) "*habitica*"))
      (message "You must be inside the habitica buffer")
    (if (equal (format "%s" (org-element-property :todo-type (org-element-at-point))) "todo")
        (progn (habitica-score-task (org-element-property :ID (org-element-at-point)) "up")
               (org-todo "DONE"))
      (progn (habitica-score-task (org-element-property :ID (org-element-at-point)) "down")
             (org-todo "TODO"))
      )))

(defun habitica-new-task ()
  "Tries to be smart to create a new task based on context"
  (interactive)
  (if (not (equal (buffer-name) "*habitica*"))
      (message "You must be inside the habitica buffer")
    (progn (beginning-of-line)
           (kill-line)
           (habitica-insert-task (habitica-create-task (habitica-get-current-type) (car kill-ring-yank-pointer))))))

(defun habitica-tasks ()
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
