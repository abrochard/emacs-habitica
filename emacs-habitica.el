


(require 'json)


(defvar habitica-base "https://habitica.com/api/v3")
;;(defvar habitica-uid "123")
;;(defvar habitica-token "456")


(defun habitica-get-tasks ()
  (let ((url  (concat habitica-base "/tasks/user"))
        (url-request-method        "GET")
        (url-request-extra-headers `(("Content-Type" . "application/json") ("x-api-user" . ,habitica-uid) ("x-api-key" . ,habitica-token)))
        (url-request-data          ""))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (delete-region (point-min) (string-match-p "{" (buffer-string)))
      (assoc-default 'data (json-read-from-string (buffer-string))))))

(defun habitica-parse-tasks (tasks type)
  (dolist (value (append tasks nil))
    (if (equal (assoc-default 'type value) type)
        (progn
          (if (eq (assoc-default 'completed value) :json-false)
              (insert "** TODO ")
            (insert "** DONE "))
          (insert (concat (assoc-default 'text value) "\n"))
         ))))

(defun habitica-tasks ()
  (interactive)
  (habitica-get-dailys)
  (switch-to-buffer "*habitica*")
  (delete-region (point-min) (point-max))
  (org-mode)
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
