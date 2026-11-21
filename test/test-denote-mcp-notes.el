;;; test-denote-mcp-notes.el --- Tests for typed notes -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mcpkit)
(require 'denote-mcp)

(defun test-denote-notes--call (name &rest args)
  "Call the denote tool NAME with ARGS and return its result."
  (funcall (mcpkit-tool-handler (gethash name (mcpkit-service-tools denote-mcp-service)))
           args (lambda (err r) (when err (error "%s" err)) r)))

(defmacro test-denote-notes--with-dir (&rest body)
  "Run BODY with a scratch `denote-directory' and no agentic.org capture."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "denote-notes-" t))
          (denote-directory dir)
          (captured nil))
     (unwind-protect
         (cl-letf (((symbol-function 'denote-mcp-notes--capture)
                    (lambda (id title) (push (cons id title) captured) t)))
           ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b) (string-prefix-p dir (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(ert-deftest test-denote-notes/tools-registered ()
  (dolist (tool '("denote_update_note" "denote_next_tasks" "denote_close_note"))
    (should (gethash tool (mcpkit-service-tools denote-mcp-service)))))

(ert-deftest test-denote-notes/create-plan-tags-and-capture ()
  (test-denote-notes--with-dir
    (let* ((res (test-denote-notes--call "denote_create_note" :title "My plan" :type "plan"
                                         :content "* Tasks\n"))
           (path (plist-get res :path)))
      (should (string-match-p "__agent_plan" path))
      (should (eq (plist-get res :todo_captured) t))
      (should (equal (cdar captured) "My plan"))
      (with-temp-buffer
        (insert-file-contents path)
        (should (search-forward "#+execution_status: proposed" nil t))))))

(ert-deftest test-denote-notes/create-report-no-capture ()
  (test-denote-notes--with-dir
    (let ((res (test-denote-notes--call "denote_create_note" :title "A report" :type "report"
                                        :content "body\n")))
      (should (string-match-p "__agent_report" (plist-get res :path)))
      (should-not (plist-member res :todo_captured))
      (should-not captured))))

(ert-deftest test-denote-notes/tracker-add-tasks-and-results ()
  (test-denote-notes--with-dir
    (let* ((path (plist-get (test-denote-notes--call "denote_create_note" :title "Sweep"
                                                     :type "tracker" :content "* Tasks\n")
                            :path))
           (res (test-denote-notes--call
                 "denote_update_note" :note path
                 :add_tasks (vector (list :category "Bugs" :id_hint "leak in queue" :title "Fix leak"))))
           (id (aref (plist-get res :added_ids) 0)))
      (should (stringp id))
      (test-denote-notes--call "denote_update_note" :note path
                               :results (vector (list :id id :text "fixed in abc123")))
      (with-temp-buffer
        (insert-file-contents path)
        (should (search-forward "Fix leak" nil t))
        (should (search-forward "fixed in abc123" nil t))))))

(ert-deftest test-denote-notes/append-body-child-heading ()
  (test-denote-notes--with-dir
    (let ((path (plist-get (test-denote-notes--call "denote_create_note" :title "P" :type "report"
                                                    :content "* Tasks\n:PROPERTIES:\n:CUSTOM_ID: tasks\n:END:\n* Feedback\n")
                           :path)))
      (test-denote-notes--call "denote_append_body" :file_or_slug path :custom_id "tasks"
                               :headline_title "New task" :todo_state "TODO" :content "Details")
      (with-temp-buffer
        (insert-file-contents path)
        (should (search-forward "** TODO New task" nil t))
        (should (search-forward "Details" nil t))))))

(provide 'test-denote-mcp-notes)
;;; test-denote-mcp-notes.el ends here
