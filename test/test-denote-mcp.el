;;; test-denote-mcp.el --- Tests for denote-mcp and mcpkit-emacs -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

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
        (should (gethash "denote_convert_to_org" tools))
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

(ert-deftest test-denote-mcp/targeted-append-and-vector-keywords ()
  "Append at a custom ID and accept vector keywords in create and rename."
  (test-mcpkit--with-temp-denote
    (let* ((svc (denote-mcp-register))
           (tools (mcpkit-service-tools svc))
           (create (mcpkit-tool-handler (gethash "denote_create_note" tools)))
           (append-body (mcpkit-tool-handler (gethash "denote_append_body" tools)))
           (rename (mcpkit-tool-handler (gethash "denote_rename" tools)))
           (note (funcall create
                          (list :title "Targeted Test" :keywords ["agent" "plan"]
                                :content "* Tasks\n:PROPERTIES:\n:CUSTOM_ID: tasks\n:END:\nOld\n* Other\nOther body\n")
                          (lambda (_status result) result)))
           (file (plist-get note :path)))
      (funcall append-body (list :file_or_slug "targeted-test" :custom_id "tasks"
                                 :content "New task\n") (lambda (_status result) result))
      (with-temp-buffer
        (insert-file-contents file)
        (should (string-match-p "Old\nNew task\n\\* Other" (buffer-string))))
      (let ((result (funcall rename (list :file_or_slug file :new_keywords ["review"])
                             (lambda (_status value) value))))
        (should (member "review" (plist-get result :keywords)))))))

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
          (should (= (plist-get tree-res :count) 3)))

        ;; Next sequence calculations
        (let ((next-tool (gethash "denote_seq_get_next" (mcpkit-service-tools svc))))
          ;; 1. Next child for sequence 1 (1a and 1b already exist -> 1c)
          (let ((n1 (funcall (mcpkit-tool-handler next-tool)
                             (list :target_sequence "1")
                             (lambda (_status r) r))))
            (should (equal (plist-get n1 :next_sequence) (denote-sequence-get-new 'child "1")))
            (should (equal (plist-get n1 :relation) "child")))

          ;; 2. Next sibling for sequence 1
          (let ((n2 (funcall (mcpkit-tool-handler next-tool)
                             (list :target_sequence "1" :type "sibling")
                             (lambda (_status r) r))))
            (should (equal (plist-get n2 :next_sequence) "2"))
            (should (equal (plist-get n2 :relation) "sibling")))

          ;; 3. Next parent when target_sequence is omitted
          (let ((n3 (funcall (mcpkit-tool-handler next-tool)
                             (list)
                             (lambda (_status r) r))))
            (should (equal (plist-get n3 :next_sequence) "2"))
            (should (equal (plist-get n3 :relation) "parent"))))

        ;; Reparent operations
        (let ((rep-tool (gethash "denote_seq_reparent" (mcpkit-service-tools svc))))
          ;; Create branch 2 and 2a
          (funcall (mcpkit-tool-handler create-tool)
                   (list :title "Branch Two" :sequence "2")
                   (lambda (_status r) r))
          (funcall (mcpkit-tool-handler create-tool)
                   (list :title "Branch Two Child" :sequence "2a")
                   (lambda (_status r) r))
          ;; Reparent Branch Two under Root Note (sequence 1) recursively
          (let ((rep-res (funcall (mcpkit-tool-handler rep-tool)
                                  (list :source_slug "branch-two"
                                        :new_parent_slug "root-note"
                                        :recursive t)
                                  (lambda (_status r) r))))
            (should (equal (plist-get rep-res :status) "reparented_recursive"))
            (should (file-exists-p (plist-get rep-res :source)))
            ;; Child 2a must also have been recursively reparented under root sequence 1
            (let ((tree-after (funcall (mcpkit-tool-handler tree-tool)
                                       (list :root_sequence "1")
                                       (lambda (_status r) r))))
              (should (= (plist-get tree-after :count) 5)))))))))

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
          (should (> (length content) 0))))

      ;; Test tools/call request for denote_create_note with JSON array keywords
      (let* ((create-req "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\"name\":\"denote_create_note\",\"arguments\":{\"title\":\"JSONRPC Created\",\"keywords\":[\"agent\",\"test\"],\"content\":\"Hello from JSONRPC\"}}}")
             create-resp)
        (mcpkit--handle-request-payload
         create-req
         active-services
         (lambda (_status resp &rest _)
           (setq create-resp resp)))
        (should (equal (plist-get create-resp :id) 4))
        (let* ((res (plist-get create-resp :result))
               (content (plist-get res :content)))
          (should (vectorp content))
          (should (> (length content) 0)))))))

(ert-deftest test-denote-mcp/convert-markdown-to-org ()
  "Convert metadata and body, preserving the source identifier and signature."
  (test-mcpkit--with-temp-denote
    (let* ((path (expand-file-name "20260101T120000==1a--sample__test.md" denote-directory))
           (new (concat (file-name-sans-extension path) ".org"))
           (tool (gethash "denote_convert_to_org" (mcpkit-service-tools (denote-mcp-register)))))
      (with-temp-file path
        (insert "---\ntitle: Sample\ndate: 2026-01-01\ntags: [test]\nidentifier: 20260101T120000\nsignature: 1a\n---\n\n# Heading\n\n| Name | Value |\n|---|---|\n| A | B |\n\n[Plan](denote:20260101T130000)\n"))
      (let* ((visiting (find-file-noselect path))
             (result (funcall (mcpkit-tool-handler tool) (list :file_or_slug path)
                              (lambda (err value)
                                (when err (error "Conversion failed: %s" err))
                                value))))
        (should (equal (buffer-file-name visiting) new))
        (kill-buffer visiting)
        (should (equal (plist-get result :new_path) new))
        (should (equal (plist-get result :identifier) "20260101T120000"))
        (should-not (file-exists-p path))
        (with-temp-buffer
          (insert-file-contents new)
          (should (search-forward "#+signature:  1a" nil t))
          (should (search-forward "* Heading" nil t))
          (should (search-forward "| A" nil t))
          (should (search-forward "[[denote:20260101T130000][Plan]]" nil t)))))))

(ert-deftest test-denote-mcp/convert-refuses-collision-and-unsaved-buffer ()
  "Keep source and target unchanged if a collision or unsaved edit exists."
  (test-mcpkit--with-temp-denote
    (let* ((path (expand-file-name "20260101T120000--sample.md" denote-directory))
           (new (concat (file-name-sans-extension path) ".org"))
           (tool (gethash "denote_convert_to_org" (mcpkit-service-tools (denote-mcp-register))))
           (handler (mcpkit-tool-handler tool)))
      (with-temp-file path (insert "---\ntitle: Sample\nidentifier: 20260101T120000\n---\n\n# Text\n"))
      (with-temp-file new (insert "existing"))
      (should (stringp (funcall handler (list :file_or_slug path) (lambda (err _r) err))))
      (delete-file new)
      (let ((buffer (find-file-noselect path)))
        (unwind-protect
            (progn
              (with-current-buffer buffer (goto-char (point-max)) (insert "unsaved"))
              (should (stringp (funcall handler (list :file_or_slug path) (lambda (err _r) err))))
              (should (file-exists-p path))
              (should-not (file-exists-p new)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest test-denote-mcp/convert-rejects-extra-metadata ()
  "Do not discard extra YAML metadata or accept malformed front matter."
  (test-mcpkit--with-temp-denote
    (let* ((path (expand-file-name "20260101T120000--sample.md" denote-directory))
           (tool (gethash "denote_convert_to_org" (mcpkit-service-tools (denote-mcp-register))))
           (handler (mcpkit-tool-handler tool)))
      (dolist (content '("---\ntitle: Sample\ngdocs_id: abc\n---\n\n# Heading\n"
                         "---\ntitle: Sample\n\n# Missing delimiter\n"
                         "---\ntitle: Sample\ntitle: Other\n---\n\n# Heading\n"))
        (with-temp-file path (insert content))
        (should (stringp (funcall handler (list :file_or_slug path) (lambda (err _r) err))))
        (should (equal (with-temp-buffer (insert-file-contents path) (buffer-string)) content))
        (should-not (file-exists-p (concat (file-name-sans-extension path) ".org")))))))

(ert-deftest test-denote-mcp/convert-refuses-concurrent-change ()
  "Preserve source changes made during Markdown conversion."
  (test-mcpkit--with-temp-denote
    (require 'mdwn)
    (let* ((path (expand-file-name "20260101T120000--sample.md" denote-directory))
           (tool (gethash "denote_convert_to_org" (mcpkit-service-tools (denote-mcp-register))))
           (original (symbol-function 'mdwn-to-org)))
      (with-temp-file path
        (insert "---\ntitle: Sample\nidentifier: 20260101T120000\n---\n\n# Heading\n"))
      (cl-letf (((symbol-function 'mdwn-to-org)
                 (lambda (body)
                   (prog1 (funcall original body)
                     (with-temp-buffer (insert "New data") (append-to-file (point-min) (point-max) path))))))
        (let ((err (funcall (mcpkit-tool-handler tool) (list :file_or_slug path)
                            (lambda (err _r) err))))
          (should (stringp err))
          (should (string-match-p "Source changed during conversion" err))))
      (should (file-exists-p path))
      (should (with-temp-buffer
                (insert-file-contents path)
                (search-forward "New data" nil t)))
      (should-not (file-exists-p (concat (file-name-sans-extension path) ".org"))))))

(ert-deftest test-denote-mcp/json-dispatch-keywords ()
  "Create a note with a JSON keyword array through the MCP dispatcher."
  (test-mcpkit--with-temp-denote
    (let ((svc (denote-mcp-register))
          (payload (json-serialize
                    (list :jsonrpc "2.0" :id 2 :method "tools/call"
                          :params (list :name "denote_create_note"
                                        :arguments (list :title "JSON Array Review"
                                                         :keywords ["agent" "plan"])))))
          status response)
      (unwind-protect
          (progn
            (mcpkit--handle-request-payload
             payload (list (cons svc 'error))
             (lambda (s r &rest _) (setq status s response r)))
            (should (eq status 'success))
            (should-not (plist-get response :error))
            (let ((files (directory-files temp-dir nil "json-array-review")))
              (should (= (length files) 1))
              (should (string-match-p "__agent_plan\\.org\\'" (car files)))))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (and buffer-file-name (string-prefix-p temp-dir buffer-file-name))
              (set-buffer-modified-p nil)
              (kill-buffer buffer))))))))

(ert-deftest test-denote-mcp/targeted-append-rejects-invalid-targets ()
  "Reject ambiguous, absent and non-Org targets without changing notes."
  (test-mcpkit--with-temp-denote
    (let* ((svc (denote-mcp-register))
           (handler (mcpkit-tool-handler
                     (gethash "denote_append_body" (mcpkit-service-tools svc))))
           (org-file (expand-file-name "20260101T000000--sample.org" temp-dir))
           (text-file (expand-file-name "20260101T000001--sample.txt" temp-dir))
           (body "* Tasks\nFirst\n* Tasks\nSecond\n"))
      (with-temp-file org-file (insert body))
      (with-temp-file text-file (insert "Plain text\n"))
      (seq-do (lambda (args)
                (should (stringp (funcall handler args (lambda (err _value) err)))))
              (list (list :file_or_slug org-file :target_heading "Tasks" :content "x")
                    (list :file_or_slug org-file :target_heading "Missing" :content "x")
                    (list :file_or_slug text-file :target_heading "Tasks" :content "x")))
      (with-temp-buffer
        (insert-file-contents org-file)
        (should (equal (buffer-string) body)))
      (with-temp-buffer
        (insert-file-contents text-file)
        (should (equal (buffer-string) "Plain text\n"))))))

(provide 'test-denote-mcp)
;;; test-denote-mcp.el ends here
