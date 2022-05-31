;;; gitlens.el --- Show git blame info about the current line         -*- lexical-binding: t; -*-
;;; Commentary:
;; Heavily inspired by the VSCode gitlens project and blamer.el
;;; Code:
;;;
;;;

(require 'parsec)
(require 'subr-x)

(defconst gitlens--blame-command "git blame --incremental --root %s"
  "Command to run to get git blame info, %s is substituted with file-path.")

(defconst gitlens--blame-command-for-lines "git blame --porcelain --root -L %s,%s %s"
  "Command to run to get git blame info for a range of lines.")


;;
;; Hashless hash table
;; Idea: We don't need to rehash the git commit hashes
;;
;; (probably not worth it, we barely beat sxhash )
;;
;; (defun gitlens--hash-test (key1 key2)
;;   (string= key1 key2))
;;
;; (defun gitlens--hash-hash (key)
;;   (string-to-number (substring key 0 8) 16))
;;

(defun gitlens--get-blame-info (file-name)
  "Run git-blame on FILE-NAME."
  (shell-command-to-string
   (format gitlens--blame-command file-name)))
;;; gitlens.el ends here
