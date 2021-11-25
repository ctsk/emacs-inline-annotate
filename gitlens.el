;;; gitlens.el --- Show git blame info about the current line         -*- lexical-binding: t; -*-
;;; Commentary:
;; Heavily inspired by the VSCode gitlens project and blamer.el
;;; Code:
;;;
;;;
(require 'cl-lib)
(require 'parsec)
(require 'subr-x)

(cl-defstruct commit
  hash
  author    author-mail author-time author-tz
  committer committer-mail committer-time committer-tz
  summary   previous       filename       line)

(defconst gitlens--blame-command "git blame --line-porcelain --root %s"
  "Command to run to get git blame info, %s is substituted with file-path.")

(defconst gitlens--blame-command-for-lines "git blame --line-porcelain --root -L %s,%s %s"
  "Command to run to get git blame info for a range of lines.")

(defmacro gitlens--setter (key struct)
  "Create lambda v -> (setf (KEY STRUCT) v)."
  `(lambda (val) (setf (,key ,struct) val)))

(defun gitlens--split-lines (string)
  "Split STRING into lines separated by \n."
  (split-string string "\n"))

(defun gitlens--get-blame-info (file-name)
  "Run git-blame on FILE-NAME."
  (shell-command-to-string
   (format gitlens--blame-command-for-lines 10 15 file-name)))

(defun gitlens--finish-line ()
  "Collect the rest of the line."
  (parsec-until-s (parsec-eol-or-eof)))

(defun gitlens--skip-ws ()
  "Consume one or more whitespace characters."
  (parsec-many (parsec-ch ?\s)))

(defun gitlens--next-word ()
  "Consume the next word, as well as any whitespace before it."
  (gitlens--skip-ws)
  (parsec-many-s
   (parsec-none-of ?\s ?\n ?\r)))

(defun gitlens--parse-key-value-line (key setter)
  "If the next string is KEY call SETTER with the rest of the line as parameter."
  (when (parsec-str key)
    (gitlens--skip-ws)
    (funcall setter (gitlens--finish-line))))

(defun gitlens--porcelain--parse-line (commit-struct)
  "Parse a line and store it in COMMIT-STRUCT."
  (let ((key (parsec-or
                (gitlens--parse-key-value-line "author-mail"    (gitlens--setter commit-author-mail    commit-struct))
                (gitlens--parse-key-value-line "author-time"    (gitlens--setter commit-author-time    commit-struct))
                (gitlens--parse-key-value-line "author-tz"      (gitlens--setter commit-author-tz      commit-struct))
                (gitlens--parse-key-value-line "author"         (gitlens--setter commit-author         commit-struct))
                (gitlens--parse-key-value-line "committer-mail" (gitlens--setter commit-committer-mail commit-struct))
                (gitlens--parse-key-value-line "committer-time" (gitlens--setter commit-committer-time commit-struct))
                (gitlens--parse-key-value-line "committer-tz"   (gitlens--setter commit-committer-tz   commit-struct))
                (gitlens--parse-key-value-line "committer"      (gitlens--setter commit-committer      commit-struct))
                (gitlens--parse-key-value-line "summary"        (gitlens--setter commit-summary        commit-struct))
                (gitlens--parse-key-value-line "previous"       (gitlens--setter commit-previous       commit-struct))
                (gitlens--parse-key-value-line "filename"       (gitlens--setter commit-filename       commit-struct))
                (gitlens--parse-key-value-line "\t"             (gitlens--setter commit-line           commit-struct)))))
    (or (eq key nil)                ;; unknown key - ignore
        (not (string= key "\t")))))

(defun gitlens--porcelain--parse-commit (hash-table)
  "Parse a commit structure and store it in HASH-TABLE."
  (let* ((hash       (gitlens--next-word))
         (old-line   (gitlens--next-word))
         (cur-line   (gitlens--next-word))
         (cur-commit (gethash hash hash-table (make-commit))))

    (unless (string-empty-p hash)
        ;; ignore <count> information for now
        (gitlens--finish-line)

        ;; parse any commit info we find
        (setf (commit-hash cur-commit) hash)
        (parsec-many (gitlens--porcelain--parse-line cur-commit))
        (puthash hash cur-commit hash-table)

        (cons cur-line hash))))

(defun gitlens--porcelain-parse (str)
  "Parse STR where STR is the output of git blame --porcelain."
  (let ((hash-table (make-hash-table :test 'equal)))
    (parsec-with-input str
      (parsec-many-till
        (gitlens--porcelain--parse-commit hash-table)
        (parsec-eof)))
    hash-table))


(defvar test-str
 "bd78fa1d5442e6e023a16d407741ec899d57d3cd 10 10 1
author Chong Yidong
author-mail <cyd@stupidchicken.com>
author-time 1283098633
author-tz -0400
committer Chong Yidong
committer-mail <cyd@stupidchicken.com>
committer-time 1283098633
committer-tz -0400
summary Add \"Package:\" file headers to denote built-in packages.
previous 4520b858c1d64bf61db1ae7b00b1b84e0cc645bd lisp/emacs-lisp/bytecomp.el
filename lisp/emacs-lisp/bytecomp.el
\t;; Package: emacs
1c393159a24ae0c5891c7f6367db53459f76d2e0 4 11 1
author Jim Blandy
author-mail <jimb@redhat.com>
author-time 710806007
author-tz +0000
committer Jim Blandy
committer-mail <jimb@redhat.com>
committer-time 710806007
committer-tz +0000
summary Initial revision
filename lisp/emacs-lisp/bytecomp.el
\t
1c393159a24ae0c5891c7f6367db53459f76d2e0 9 12 2
\t;; This file is part of GNU Emacs.
1c393159a24ae0c5891c7f6367db53459f76d2e0 10 13
\t;;blablaablalbalbalbalab\n\n")


(provide 'gitlens)
;;; gitlens.el ends here
