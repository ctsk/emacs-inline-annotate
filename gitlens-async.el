;;; gitlens-async.el --- Loaded by async instance         -*- lexical-binding: t; -*-
;;; Commentary:
;;  Minimize the amount of stuff that the async instance has to load
;;; Code:

(require 'subr-x)
(require 'gitlens-parser)

(defconst gitlens--blame-command-incremental "git blame --incremental --root %s"
  "Command to run to get git blame info (porcelain), subst file path at %s.")

(defconst gitlens--blame-command-porcelain "git blame --porcelain --root %s"
  "Command to run to get git blame info (incremental), subst file path at %s.")

(defun gitlens-async--get-blame-info (file-name &optional incremental)
  "Run git-blame on FILE-NAME. Use --incremental if INCREMENTAL is non-nil."
  (when file-name
    (let ((command-str (if incremental gitlens--blame-command-incremental
                                       gitlens--blame-command-porcelain)))
      (shell-command-to-string
       (format command-str file-name)))))

(provide 'gitlens-async)
;;; gitlens-async.el ends here
