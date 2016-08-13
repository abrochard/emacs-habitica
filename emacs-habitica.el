


(require 'json)


(defvar habitica-base "https://habitica.com/api/v3")
;;(defvar habitica-uid "123")
;;(defvar habitica-token "456")


;;; autoload

(define-minor-mode habitica-mode
  "Mode to edit and manage your Habitica tasks"
  :lighter " Habitica"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x t") 'habitica-new-task)
            map))

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

(defun habitica-parse-tasks (tasks type)
  "Parses the tasks to org-mode format"
  (dolist (value (append tasks nil))
    (if (equal (assoc-default 'type value) type)
        (progn
          (if (eq (assoc-default 'completed value) :json-false)
              (insert "** TODO ")
            (insert "** DONE "))
          (insert (concat (assoc-default 'text value) "\n"))
          (if (and (assoc-default 'date value) (< 1 (length (assoc-default 'date value))))
              (insert (concat "   DEADLINE: <" (assoc-default 'date value) ">\n")))
          ))))

(defun habitica-create-task (type text &optional down)
  "Sends a post request to create a new user task"
  (if down
      (habitica-send-request "/tasks/user" "POST" (concat "type=" type "&text=" text "&down=" down))
    (habitica-send-request "/tasks/user" "POST" (concat "type=" type "&text=" text))))

(defun habitica-get-current-type ()
  "Get the current type based on the cursor position"
  (save-excursion
    (progn (re-search-backward "^\* " (point-min) t)
           (let ((text (org-element-property :raw-value (org-element-at-point))))
             (cond ((equal "Habits" text) "habit")
                   ((equal "Daily Tasks" text) "daily")
                   ((equal "To-Dos" text) "todo"))))))

(defun habitica-new-task ()
  "Tries to be smart to create a new task based on context"
  (interactive)
  (if (not (equal (buffer-name) "*habitica*"))
      (message "You must be inside the habitica buffer")
    (progn (habitica-create-task (habitica-get-current-type) (thing-at-point 'line))
           (beginning-of-line)
           (insert "** TODO "))))

(defun habitica-tasks ()
  (interactive)
  (switch-to-buffer "*habitica*")
  (delete-region (point-min) (point-max))
  (org-mode)
  (habitica-mode)
  (insert "#+TITLE: Habitica Dashboard\n\n")
  (let ((habitica-data (habitica-get-tasks)))
    (insert "* Habits\n")
    (habitica-parse-tasks habitica-data "habit")
    (insert "\n")
    (insert "* Daily Tasks\n")
    (habitica-parse-tasks habitica-data "daily")
    (insert "\n")
    (insert "* To-Dos\n")
    (habitica-parse-tasks habitica-data "todo")
    )
  )

(provide 'habitica)
