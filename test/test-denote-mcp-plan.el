;;; test-denote-mcp-plan.el --- Tests for batched plan updates -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers `denote-mcp-plan-update', `denote-mcp-plan-next-tasks' and
;; the `denote_update_note' / `denote_next_tasks' / `denote_close_note' MCP
;; tools.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'denote-mcp)

(defconst test-aip--plan
  "#+title: Sample
#+filetags: :agent:plan:
#+TODO: TODO INPROGRESS BLOCKED | DONE CANCELED
#+TODO: QUESTION | ANSWERED
#+execution_status: proposed
#+started:
#+finished:

* Tasks
** TODO First
:PROPERTIES:
:CUSTOM_ID: first
:END:
** TODO Second
:PROPERTIES:
:CUSTOM_ID: second
:END:
** TODO Third
:PROPERTIES:
:CUSTOM_ID: third
:END:

* Execution Plan
| Task              | Dependencies    | Status |
|-------------------+-----------------+--------|
| [[#first][First]]   | —               | TODO   |
| [[#second][Second]] | [[#first][First]]  | TODO   |
| [[#third][Third]]   | [[#second][Second]] | TODO   |

* Assumptions
Body.

* Open Questions
** QUESTION Which backend? :question:
")

(defconst test-aip--tracker
  "#+title: Tracker
#+filetags: :agent:tracker:
#+TODO: TODO INPROGRESS BLOCKED | DONE CANCELLED
#+execution_status: proposed
#+started:
#+finished:

* Tasks
** Cat
*** TODO One
:PROPERTIES:
:ID: one
:END:
")

(defmacro test-aip--with-note (var kind &rest body)
  "Bind VAR to a temp note of KIND (`plan' or `tracker') while running BODY."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "aip-test" t))
          (,var (expand-file-name
                 (format "20261121T120000--sample__agent_%s.org" ,kind) dir))
          (denote-directory dir))
     (unwind-protect
         (progn
           (with-temp-file ,var
             (insert (if (equal ,kind "plan") test-aip--plan test-aip--tracker)))
           ,@body)
       (when-let* ((b (get-file-buffer ,var))) (with-current-buffer b (set-buffer-modified-p nil)) (kill-buffer b))
       (delete-directory dir t))))

(defun test-aip--text (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest denote-mcp-plan-update-batches-and-syncs-table ()
  "One call flips several headings, re-derives the table and saves once."
  (test-aip--with-note f "plan"
    (let ((r (denote-mcp-plan-update
              f :transitions '(("first" . "DONE") ("second" . "INPROGRESS")))))
      (should (= (plist-get r :applied) 2))
      (should (= (plist-get r :total) 3))
      (should (= (plist-get r :open) 2))
      (should-not (plist-get r :all-done))
      (should-not (buffer-modified-p (get-file-buffer f)))
      (let ((text (test-aip--text f)))
        (should (string-match-p "^\\*\\* DONE First" text))
        (should (string-match-p "^\\*\\* INPROGRESS Second" text))
        (should (string-match-p "\\[\\[#first\\]\\[First\\]\\] +| — +| DONE" text))
        (should (string-match-p "\\[\\[#second\\]\\[Second\\]\\] +| \\[\\[#first\\]\\[First\\]\\] +| INPROGRESS" text))
        ;; Third depends on second, which changed: its own status is untouched.
        (should (string-match-p "\\[\\[#third\\]\\[Third\\]\\] +| \\[\\[#second\\]\\[Second\\]\\] +| TODO" text))))))

(ert-deftest denote-mcp-plan-update-unknown-id-changes-nothing ()
  "A bad id aborts before any heading is touched."
  (test-aip--with-note f "plan"
    (should-error (denote-mcp-plan-update
                   f :transitions '(("first" . "DONE") ("nope" . "DONE"))))
    (should (string-match-p "^\\*\\* TODO First" (with-current-buffer (get-file-buffer f) (buffer-string))))))

(ert-deftest denote-mcp-plan-update-marks-finished-plan-executed ()
  "All tasks done on a plan requests the x tag; a tracker never does."
  (test-aip--with-note f "plan"
    (let (marked)
      (cl-letf (((symbol-function 'denote-mcp--mark-executed)
                 (lambda (file) (setq marked file) nil)))
        (let ((r (denote-mcp-plan-update
                  f :transitions '(("first" . "DONE") ("second" . "DONE") ("third" . "CANCELED"))
                  :mark-executed t)))
          (should (plist-get r :all-done))
          (should (plist-get r :marked))
          (should (equal marked f))))))
  (test-aip--with-note f "tracker"
    (let (marked)
      (cl-letf (((symbol-function 'denote-mcp--mark-executed)
                 (lambda (file) (setq marked file) nil)))
        (let ((r (denote-mcp-plan-update
                  f :transitions '(("one" . "DONE")) :mark-executed t)))
          (should (plist-get r :all-done))
          (should-not (plist-get r :marked))
          (should-not marked))))))

(ert-deftest denote-mcp-plan-update-questions-are-not-tasks ()
  "An open QUESTION never blocks the all-done count."
  (test-aip--with-note f "plan"
    (let ((r (denote-mcp-plan-update
              f :transitions '(("first" . "DONE") ("second" . "DONE") ("third" . "DONE")))))
      (should (plist-get r :all-done))
      (should (= (plist-get r :total) 3)))))

(ert-deftest denote-mcp-plan-update-decisions-notes-answers-status ()
  "Decisions, notes, answers and execution status land in a single save."
  (test-aip--with-note f "plan"
    (denote-mcp-plan-update
     f
     :decisions '(("Use sqlite" "Because." nil nil)
                  ("Inline" "Short." nil "^\\* Assumptions"))
     :notes '(("^\\* Assumptions" . "Added later."))
     :answers '(("Which backend?" "sqlite"))
     :execution-status "in-progress")
    (let ((text (test-aip--text f)))
      (should (string-match-p "^\\* Decisions" text))
      (should (string-match-p ":STATUS: ACCEPTED" text))
      (should (string-match-p "\\*Decision \\[[^]]+\\]: Inline\\* Short\\." text))
      (should (string-match-p "Added later\\." text))
      (should (string-match-p "^\\*\\* ANSWERED Which backend\\?" text))
      (should (string-match-p "^#\\+execution_status: in-progress" text))
      (should (string-match-p "^#\\+started: \\[" text)))))

(ert-deftest denote-mcp-plan-next-tasks-rounds-and-ready ()
  "Ready tasks have every dependency done; rounds follow the dependency chain."
  (test-aip--with-note f "plan"
    (let ((r (denote-mcp-plan-next-tasks f)))
      (should (equal (mapcar (lambda (x) (plist-get x :id)) (plist-get r :ready)) '("first")))
      (should (equal (mapcar (lambda (x) (plist-get x :round)) (plist-get r :rounds)) '(1 2 3))))
    (denote-mcp-plan-update f :transitions '(("first" . "DONE")))
    (let ((r (denote-mcp-plan-next-tasks f)))
      (should (equal (mapcar (lambda (x) (plist-get x :id)) (plist-get r :ready)) '("second")))
      (should (= (alist-get "DONE" (plist-get r :counts) 0 nil #'equal) 1)))))

(ert-deftest denote-mcp-notes-update-tool-and-close-guard ()
  "The denote_update_note tool batches work; denote_close_note needs confirmation."
  (test-aip--with-note f "plan"
    (let* ((tools (mcpkit-service-tools (mcpkit-get-service 'denote)))
           (call (lambda (name args)
                   (let (err val)
                     (funcall (mcpkit-tool-handler (map-elt tools name)) args
                              (lambda (e v) (setq err e val v)))
                     (list err val)))))
      (let ((res (funcall call "denote_update_note"
                          `(:note ,f :transitions [(:id "first" :status "DONE")
                                                   (:id "second" :status "INPROGRESS")]))))
        (should-not (car res))
        (should (equal (plist-get (cadr res) :applied) 2))
        (should (equal (plist-get (cadr res) :open) 2))
        (should (eq (plist-get (cadr res) :all_done) :json-false)))
      (let ((res (funcall call "denote_next_tasks" `(:note ,f))))
        (should-not (car res))
        (should (equal (plist-get (aref (plist-get (cadr res) :ready) 0) :id) "second")))
      (should (car (funcall call "denote_close_note" `(:note ,f))))
      (should (car (funcall call "denote_close_note" `(:note ,f :confirmed :json-false)))))))

(provide 'test-denote-mcp-plan)
;;; test-denote-mcp-plan.el ends here
