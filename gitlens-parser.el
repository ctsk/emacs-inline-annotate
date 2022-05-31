;;; gitlens-parser.el --- Parser of GIT Blame output -*- lexical-binding: t; -*-
;;; Commentary:
;; To be able to be loaded with emacs-async
;;
;;; Code:

(require 'cl-lib)
(require 'parsec)

(cl-defstruct commit
;;  hash
  author
  author-mail
  author-time
  author-tz
  ;; committer
  ;; committer-mail
  ;; committer-time
  ;; committer-tz
  ;; previous
  ;; filename
  summary)

(defmacro gitlens-parser--setter (key struct)
  "Create lambda v -> (setf (KEY STRUCT) v)."
  `(lambda (val) (setf (,key ,struct) val)))

;;
;; Parsec Helpers
;;

(defun gitlens-parser--collect-line ()
  "Collect the rest of the line."
  (prog1 (parsec-re "[^\n]*")
    (parsec-eol-or-eof)))

(defun gitlens-parser--skip-ws ()
  "Consume one or more whitespace characters."
  (parsec-many (parsec-ch ?\s)))

(defun gitlens-parser--next-word ()
  "Consume the next word, as well as any whitespace before it."
  (gitlens-parser--skip-ws)
  (parsec-re "[[:alnum:]]*"))

(defun gitlens-parser--next-num ()
  "Consume the next number, as well as any whitespace before it."
  (string-to-number (gitlens-parser--next-word)))

;;
;; Parser helpers
;;

(defun gitlens-parser--parse-key-value-line (key setter)
  "If the next string is KEY call SETTER with the rest of the line as parameter."
  (when (parsec-str key)
    (gitlens-parser--skip-ws)
    (funcall setter (gitlens-parser--collect-line))))

(defun gitlens-parser--parse-header-line (commit-struct)
  "Parse a line and store it in COMMIT-STRUCT."
  (parsec-or
    (gitlens-parser--parse-key-value-line "author-mail"    (gitlens-parser--setter commit-author-mail    commit-struct))
    (gitlens-parser--parse-key-value-line "author-time"    (gitlens-parser--setter commit-author-time    commit-struct))
    (gitlens-parser--parse-key-value-line "author-tz"      (gitlens-parser--setter commit-author-tz      commit-struct))
    (gitlens-parser--parse-key-value-line "author"         (gitlens-parser--setter commit-author         commit-struct))
    (gitlens-parser--parse-key-value-line "summary"        (gitlens-parser--setter commit-summary        commit-struct))
    (gitlens-parser--collect-line)))

(defun gitlens-parser--parse-commit-section (cur-commit separator)
  "Parse git blame header, store in CUR-COMMIT, stop at SEPARATOR."
  ;; parse headers
  (parsec-many-till
   (gitlens-parser--parse-header-line cur-commit)
   (funcall separator))

  ;; discard line contents
  (gitlens-parser--collect-line))

(defun gitlens-parser--abstract-parse(str num-lines commit-parser)
  "Parse git blame STR, with NUM-LINES the number of lines in the buffer. delegate parsing of individual commits to COMMIT-PARSER."
  (let* ((hash-table  (make-hash-table :test 'equal :size (/ num-lines 4)))
         (line-lookup (make-vector num-lines nil)))
    (parsec-with-input str
      (parsec-many-till
        (funcall commit-parser hash-table line-lookup)
        (parsec-eof)))
    (cons hash-table line-lookup)))

;;
;; Porcelain Parser
;;

(defun gitlens-parser--porcelain-parse-commit (hash-table line-lookup)
  "Parse git blame --porcelain, Store information in HASH-TABLE and LINE-LOOKUP."
  (let* ((hash        (gitlens-parser--next-word))
         (_old-line   (gitlens-parser--next-word))
         (cur-line    (gitlens-parser--next-num))
         (lines       (gitlens-parser--next-num))
         (cur-commit  (gethash hash hash-table (make-commit))))

    (dotimes (n lines)
      (aset line-lookup (+ cur-line n -1) hash)
      (gitlens-parser--collect-line)  ;; drop the other headers, we don't need them
      (gitlens-parser--parse-commit-section cur-commit #'(lambda () (parsec-ch ?\t))))

    (puthash hash cur-commit hash-table)))


(defun gitlens-parser--porcelain-parse (str num-lines)
  "Parse output of git blame --porcelain as STR and NUM-LINES the number of lines in the buffer."
  (gitlens-parser--abstract-parse str num-lines 'gitlens-parser--porcelain-parse-commit))


;;
;; Incremental Parser
;;


(defun gitlens-parser--incremental-parse-commit (hash-table line-lookup)
  "Parse git blame --incremental, Store information in HASH-TABLE and LINE-LOOKUP."
  (let* ((hash        (gitlens-parser--next-word))
         (_old-line   (gitlens-parser--next-word))
         (cur-line    (gitlens-parser--next-num))
         (lines       (gitlens-parser--next-num))
         (cur-commit  (gethash hash hash-table (make-commit))))

    (dotimes (n lines)
      (aset line-lookup (+ cur-line n -1) hash))

    (gitlens-parser--collect-line)
    (gitlens-parser--parse-commit-section cur-commit #'(lambda () (parsec-str "filename")))

    (puthash hash cur-commit hash-table)))


(defun gitlens-parser--incremental-parse (str num-lines)
  "Parse output of git blame --incremental as STR and NUM-LINES the number of lines in the buffer."
  (gitlens-parser--abstract-parse str num-lines 'gitlens-parser--incremental-parse-commit))

(defun gitlens-parser--parse (str num-lines incremental)
  "Parse STR. NUM-LINES upper bound for number of lines in str, Use INCREMENTAL strategy if set."
  (if incremental
      (gitlens-parser--incremental-parse str num-lines)
      (gitlens-parser--porcelain-parse str num-lines)))

(provide 'gitlens-parser)
;;; gitlens-parser.el ends here
