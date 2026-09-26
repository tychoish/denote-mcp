;;; test-helper.el --- ERT test infrastructure -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(defvar test-helper-root nil
  "Repository root directory.")

(let* ((test-file (or load-file-name buffer-file-name))
       (test-dir (file-name-directory (or test-file (expand-file-name "test/test-helper.el"))))
       (root (file-name-directory (directory-file-name test-dir))))
  (setq test-helper-root root)
  (add-to-list 'load-path root)
  ;; Add local ~/.emacs.d/elpa packages if present
  (let ((user-elpa (expand-file-name "~/.emacs.d/elpa")))
    (when (file-directory-p user-elpa)
      (dolist (dir (directory-files user-elpa t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))))

(provide 'test-helper)
;;; test-helper.el ends here
