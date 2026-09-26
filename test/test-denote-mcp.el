;;; test-denote-mcp.el --- Tests for denote-mcp and mcpkit-emacs -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)

(require 'mcpkit)
(require 'denote-mcp)

(defmacro test-mcpkit--with-temp-denote (&rest body)
  "Execute BODY in a sandbox `denote-directory'."
  (declare (indent 0))
  `(let* ((temp-dir (make-temp-file "denote-test-" t))
          (denote-directory temp-dir)
          (mcpkit-registry nil)
          (mcpkit--active-services nil)
          (mcpkit--active-server nil))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t)))))

(ert-deftest test-denote-mcp/top-level-definition ()
  "Test that `denote-mcp-service' is defined and has tools at load time."
  (should (mcpkit-service-p denote-mcp-service))
  (should (eq (mcpkit-service-name denote-mcp-service) 'denote))
  (should (>= (hash-table-count (mcpkit-service-tools denote-mcp-service)) 19)))

(ert-deftest test-denote-mcp/registration ()
  "Test that all denote tools are registered on the denote service."
  (let ((mcpkit-registry nil))
    (let ((svc (denote-mcp-register)))
      (should (mcpkit-service-p svc))
      (should (eq (mcpkit-service-name svc) 'denote))
      (let ((tools (mcpkit-service-tools svc)))
        (should (gethash "denote_find" tools))
        (should (gethash "denote_find_by_slug" tools))
        (should (gethash "denote_find_most_recent" tools))
        (should (gethash "denote_get_metadata" tools))
        (should (gethash "denote_read_note" tools))
        (should (gethash "denote_create_note" tools))
        (should (gethash "denote_append_body" tools))
        (should (gethash "denote_rename" tools))
        (should (gethash "denote_mark_executed" tools))
        (should (gethash "denote_sync_frontmatter" tools))
        (should (gethash "denote_seq_get_next" tools))
        (should (gethash "denote_seq_set" tools))
        (should (gethash "denote_seq_graft" tools))
        (should (gethash "denote_seq_reparent" tools))
        (should (gethash "denote_seq_tree" tools))
        (should (gethash "denote_verify" tools))
        (should (gethash "denote_link_string" tools))
        (should (gethash "denote_insert_dblock" tools))
        (should (gethash "denote_redate" tools))))))

(ert-deftest test-denote-mcp/crud-workflow ()
  "Test creating a note, reading it, appending body, and reading metadata."
  (test-mcpkit--with-temp-denote
    (let ((svc (denote-mcp-register)))
      ;; Create note
      (let* ((create-tool (gethash "denote_create_note" (mcpkit-service-tools svc)))
             (res (funcall (mcpkit-tool-handler create-tool)
                           (list :title "Test Note Alpha"
                                 :keywords '("agent" "plan")
                                 :sequence "3a1"
                                 :content "* Introduction\nFirst section.")
                           (lambda (_status r) r))))
        (should (plist-get res :path))
        (should (equal (plist-get res :title) "Test Note Alpha"))
        (should (file-exists-p (plist-get res :path)))

        ;; Read note
        (let* ((read-tool (gethash "denote_read_note" (mcpkit-service-tools svc)))
               (read-res (funcall (mcpkit-tool-handler read-tool)
                                  (list :file_or_slug "test-note-alpha")
                                  (lambda (_status r) r))))
          (should (string-search "First section." (plist-get read-res :content))))

        ;; Append body
        (let* ((append-tool (gethash "denote_append_body" (mcpkit-service-tools svc)))
               (app-res (funcall (mcpkit-tool-handler append-tool)
                                 (list :file_or_slug "test-note-alpha"
                                       :content "* Tasks\n- [ ] Task 1")
                                 (lambda (_status r) r))))
          (should (> (plist-get app-res :bytes_appended) 0)))

        ;; Verify appended content
        (let* ((read-tool (gethash "denote_read_note" (mcpkit-service-tools svc)))
               (read-res2 (funcall (mcpkit-tool-handler read-tool)
                                   (list :file_or_slug "test-note-alpha")
                                   (lambda (_status r) r))))
          (should (string-search "Task 1" (plist-get read-res2 :content))))

        ;; Check link string
        (let* ((link-tool (gethash "denote_link_string" (mcpkit-service-tools svc)))
               (link-res (funcall (mcpkit-tool-handler link-tool)
                                  (list :target_slug "test-note-alpha")
                                  (lambda (_status r) r))))
          (should (string-prefix-p "[[denote:" (plist-get link-res :link)))
          (should (string-search "Test Note Alpha" (plist-get link-res :link))))

        ;; Mark executed
        (let* ((mark-tool (gethash "denote_mark_executed" (mcpkit-service-tools svc)))
               (mark-res (funcall (mcpkit-tool-handler mark-tool)
                                  (list :file_or_slug "test-note-alpha")
                                  (lambda (_status r) r))))
          (should (equal (plist-get mark-res :status) "marked"))
          (should (member "x" (plist-get mark-res :keywords))))))))

(ert-deftest test-denote-mcp/find-and-filter ()
  "Test `denote_find' and `denote_find_most_recent'."
  (test-mcpkit--with-temp-denote
    (let ((svc (denote-mcp-register)))
      (let ((create-tool (gethash "denote_create_note" (mcpkit-service-tools svc))))
        (funcall (mcpkit-tool-handler create-tool)
                 (list :title "Alpha One" :keywords '("agent" "plan") :date "2026-01-01")
                 (lambda (_status r) r))
        (funcall (mcpkit-tool-handler create-tool)
                 (list :title "Beta Two" :keywords '("agent" "review") :date "2026-01-02")
                 (lambda (_status r) r))
        (funcall (mcpkit-tool-handler create-tool)
                 (list :title "Gamma Three" :keywords '("agent" "plan") :date "2026-01-03")
                 (lambda (_status r) r)))

      ;; Test denote_find
      (let* ((find-tool (gethash "denote_find" (mcpkit-service-tools svc)))
             (find-res (funcall (mcpkit-tool-handler find-tool)
                                (list :query "Beta" :max_results 10)
                                (lambda (_status r) r))))
        (should (= (length find-res) 1))
        (should (equal (plist-get (car find-res) :title) "Beta Two")))

      ;; Test denote_find_most_recent with tags
      (let* ((recent-tool (gethash "denote_find_most_recent" (mcpkit-service-tools svc)))
             (recent-res (funcall (mcpkit-tool-handler recent-tool)
                                  (list :tags '("agent" "plan"))
                                  (lambda (_status r) r))))
        (should (plist-get recent-res :found))
        (should (equal (plist-get recent-res :title) "Gamma Three"))))))

(ert-deftest test-denote-mcp/rename-and-metadata ()
  "Test `denote_rename' and `denote_get_metadata'."
  (test-mcpkit--with-temp-denote
    (let ((svc (denote-mcp-register)))
      (let* ((create-tool (gethash "denote_create_note" (mcpkit-service-tools svc)))
             (meta-tool (gethash "denote_get_metadata" (mcpkit-service-tools svc)))
             (rename-tool (gethash "denote_rename" (mcpkit-service-tools svc)))
             (c-res (funcall (mcpkit-tool-handler create-tool)
                             (list :title "Original Name" :keywords '("draft"))
                             (lambda (_status r) r)))
             (orig-path (plist-get c-res :path)))
        (should (file-exists-p orig-path))

        ;; Rename
        (let ((r-res (funcall (mcpkit-tool-handler rename-tool)
                              (list :file_or_slug "original-name"
                                    :new_title "Updated Title"
                                    :new_keywords '("final" "reviewed"))
                              (lambda (_status r) r))))
          (should (equal (plist-get r-res :title) "Updated Title"))
          (should (member "reviewed" (plist-get r-res :keywords)))
          (should (file-exists-p (plist-get r-res :new_path))))

        ;; Metadata lookup
        (let ((m-res (funcall (mcpkit-tool-handler meta-tool)
                              (list :file_or_slug "updated-title")
                              (lambda (_status r) r))))
          (should (equal (plist-get m-res :title) "Updated Title"))
          (should (member "final" (plist-get m-res :filetags))))))))

(ert-deftest test-denote-mcp/sequence-ops ()
  "Test `denote_seq_get_next', `denote_seq_set', and `denote_seq_tree'."
  (test-mcpkit--with-temp-denote
    (let ((svc (denote-mcp-register)))
      (let ((create-tool (gethash "denote_create_note" (mcpkit-service-tools svc)))
            (seq-tool (gethash "denote_seq_set" (mcpkit-service-tools svc)))
            (tree-tool (gethash "denote_seq_tree" (mcpkit-service-tools svc))))
        (funcall (mcpkit-tool-handler create-tool)
                 (list :title "Root Note" :sequence "1")
                 (lambda (_status r) r))
        (funcall (mcpkit-tool-handler create-tool)
                 (list :title "Child Note" :sequence "1a")
                 (lambda (_status r) r))
        (funcall (mcpkit-tool-handler create-tool)
                 (list :title "Free Note")
                 (lambda (_status r) r))

        ;; Assign sequence to Free Note
        (let ((set-res (funcall (mcpkit-tool-handler seq-tool)
                                (list :file_or_slug "free-note" :sequence "1b")
                                (lambda (_status r) r))))
          (should (equal (plist-get set-res :sequence) "1b"))
          (should (file-exists-p (plist-get set-res :path))))

        ;; Inspect tree
        (let ((tree-res (funcall (mcpkit-tool-handler tree-tool)
                                 (list :root_sequence "1")
                                 (lambda (_status r) r))))
          (should (= (plist-get tree-res :count) 3)))))))

(ert-deftest test-denote-mcp/redate-and-links ()
  "Test `denote_redate' and repo-wide link rewriting."
  (test-mcpkit--with-temp-denote
    (let ((svc (denote-mcp-register)))
      (let* ((create-tool (gethash "denote_create_note" (mcpkit-service-tools svc)))
             (read-tool (gethash "denote_read_note" (mcpkit-service-tools svc)))
             (redate-tool (gethash "denote_redate" (mcpkit-service-tools svc)))
             ;; Note A
             (res-a (funcall (mcpkit-tool-handler create-tool)
                             (list :title "Target Note" :date "2026-01-01")
                             (lambda (_status r) r)))
             (id-a (plist-get res-a :id))
             ;; Note B linking to Note A
             (link-text (format "See [[denote:%s][Target Note]] for details." id-a)))
        (funcall (mcpkit-tool-handler create-tool)
                 (list :title "Source Note" :content link-text :date "2026-01-02")
                 (lambda (_status r) r))

        ;; Redate Note A
        (let* ((new-id "20260925T120000")
               (redate-res (funcall (mcpkit-tool-handler redate-tool)
                                    (list :file_or_slug "target-note"
                                          :new_identifier new-id)
                                    (lambda (_status r) r))))
          (should (equal (plist-get redate-res :new_identifier) new-id))
          (should (> (plist-get redate-res :links_updated) 0))

          ;; Verify Note B now links to new-id
          (let ((read-b (funcall (mcpkit-tool-handler read-tool)
                                 (list :file_or_slug "source-note")
                                 (lambda (_status r) r))))
            (should (string-search (format "[[denote:%s]" new-id)
                                   (plist-get read-b :content)))))))))

(ert-deftest test-denote-mcp/sync-frontmatter ()
  "Test `denote_sync_frontmatter' corrects missing or divergent frontmatter."
  (test-mcpkit--with-temp-denote
    (let ((svc (denote-mcp-register)))
      (let* ((create-tool (gethash "denote_create_note" (mcpkit-service-tools svc)))
             (sync-tool (gethash "denote_sync_frontmatter" (mcpkit-service-tools svc)))
             (c-res (funcall (mcpkit-tool-handler create-tool)
                             (list :title "Sync Target" :sequence "2a1")
                             (lambda (_status r) r)))
             (path (plist-get c-res :path)))
        ;; Artificially corrupt the frontmatter in the file
        (with-temp-file path
          (insert "#+title: Sync Target\n#+identifier: 19990101T000000\n\nBody here."))
        ;; Run sync
        (let ((sync-res (funcall (mcpkit-tool-handler sync-tool)
                                 (list :file_or_slug "sync-target")
                                 (lambda (_status r) r))))
          (should (> (plist-get sync-res :changes_count) 0)))))))



(ert-deftest test-denote-mcp/jsonrpc-payload ()
  "Test JSON-RPC 2.0 dispatch over `mcpkit--handle-request-payload'."
  (test-mcpkit--with-temp-denote
    (let* ((svc (denote-mcp-register))
           (active-services (list (cons svc 'error))))
      ;; Create a note directly first
      (let ((create-tool (gethash "denote_create_note" (mcpkit-service-tools svc))))
        (funcall (mcpkit-tool-handler create-tool)
                 (list :title "Payload Test Note" :keywords '("test"))
                 (lambda (_status r) r)))

      ;; Test initialize request
      (let* ((init-req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}")
             init-resp)
        (mcpkit--handle-request-payload
         init-req
         active-services
         (lambda (_status resp &rest _)
           (setq init-resp resp)))
        (should (equal (plist-get init-resp :id) 1))
        (should (plist-get (plist-get init-resp :result) :capabilities)))

      ;; Test tools/list request
      (let* ((list-req "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\",\"params\":{}}")
             list-resp)
        (mcpkit--handle-request-payload
         list-req
         active-services
         (lambda (_status resp &rest _)
           (setq list-resp resp)))
        (should (equal (plist-get list-resp :id) 2))
        (let ((tools (plist-get (plist-get list-resp :result) :tools)))
          (should (seq-find (lambda (t-desc) (equal (plist-get t-desc :name) "denote_find")) tools))))

      ;; Test tools/call request for denote_find_by_slug
      (let* ((call-req "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"denote_find_by_slug\",\"arguments\":{\"slug\":\"payload-test\"}}}")
             call-resp)
        (mcpkit--handle-request-payload
         call-req
         active-services
         (lambda (_status resp &rest _)
           (setq call-resp resp)))
        (should (equal (plist-get call-resp :id) 3))
        (let* ((res (plist-get call-resp :result))
               (content (plist-get res :content)))
          (should (vectorp content))
          (should (> (length content) 0)))))))

(provide 'test-denote-mcp)
;;; test-denote-mcp.el ends here
