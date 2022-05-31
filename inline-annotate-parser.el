;;; inline-annotate-parser.el --- Parser of GIT Blame output -*- lexical-binding: t; -*-
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

(defmacro inline-annotate-parser--setter (key struct)
  "Create lambda v -> (setf (KEY STRUCT) v)."
  `(lambda (val) (setf (,key ,struct) val)))

;;
;; Parsec Helpers
;;

(defun inline-annotate-parser--collect-line ()
  "Collect the rest of the line."
  (prog1 (parsec-re "[^\n]*")
    (parsec-eol-or-eof)))

(defun inline-annotate-parser--skip-ws ()
  "Consume one or more whitespace characters."
  (parsec-many (parsec-ch ?\s)))

(defun inline-annotate-parser--next-word ()
  "Consume the next word, as well as any whitespace before it."
  (inline-annotate-parser--skip-ws)
  (parsec-re "[[:alnum:]]*"))

(defun inline-annotate-parser--next-num ()
  "Consume the next number, as well as any whitespace before it."
  (string-to-number (inline-annotate-parser--next-word)))

;;
;; Parser helpers
;;

(defun inline-annotate-parser--parse-key-value-line (key setter &optional cast-to-num)
  "If the next string is KEY call SETTER with the rest of the line as parameter. CAST-TO-NUM when set."
  (when (parsec-str key)
    (inline-annotate-parser--skip-ws)
    (funcall setter (if cast-to-num
                      (string-to-number (inline-annotate-parser--collect-line))
                      (inline-annotate-parser--collect-line)))))


(defun inline-annotate-parser--parse-header-line (commit-struct)
  "Parse a line and store it in COMMIT-STRUCT."
  (parsec-or
    (inline-annotate-parser--parse-key-value-line "author-mail"    (inline-annotate-parser--setter commit-author-mail    commit-struct))
    (inline-annotate-parser--parse-key-value-line "author-time"    (inline-annotate-parser--setter commit-author-time    commit-struct) t)
    (inline-annotate-parser--parse-key-value-line "author-tz"      (inline-annotate-parser--setter commit-author-tz      commit-struct))
    (inline-annotate-parser--parse-key-value-line "author"         (inline-annotate-parser--setter commit-author         commit-struct))
    (inline-annotate-parser--parse-key-value-line "summary"        (inline-annotate-parser--setter commit-summary        commit-struct))
    (inline-annotate-parser--collect-line)))

(defun inline-annotate-parser--parse-commit-section (cur-commit separator)
  "Parse git blame header, store in CUR-COMMIT, stop at SEPARATOR."
  ;; parse headers
  (parsec-many-till
   (inline-annotate-parser--parse-header-line cur-commit)
   (funcall separator))

  ;; discard line contents
  (inline-annotate-parser--collect-line))

(defun inline-annotate-parser--abstract-parse(str num-lines commit-parser)
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

(defun inline-annotate-parser--porcelain-parse-commit (hash-table line-lookup)
  "Parse git blame --porcelain, Store information in HASH-TABLE and LINE-LOOKUP."
  (let* ((hash        (inline-annotate-parser--next-word))
         (_old-line   (inline-annotate-parser--next-word))
         (cur-line    (inline-annotate-parser--next-num))
         (lines       (inline-annotate-parser--next-num))
         (cur-commit  (gethash hash hash-table (make-commit))))

    (dotimes (n lines)
      (aset line-lookup (+ cur-line n -1) hash)
      (inline-annotate-parser--collect-line)  ;; drop the other headers, we don't need them
      (inline-annotate-parser--parse-commit-section cur-commit #'(lambda () (parsec-ch ?\t))))

    (puthash hash cur-commit hash-table)))


(defun inline-annotate-parser--porcelain-parse (str num-lines)
  "Parse output of git blame --porcelain as STR and NUM-LINES the number of lines in the buffer."
  (inline-annotate-parser--abstract-parse str num-lines 'inline-annotate-parser--porcelain-parse-commit))


;;
;; Incremental Parser
;;


(defun inline-annotate-parser--incremental-parse-commit (hash-table line-lookup)
  "Parse git blame --incremental, Store information in HASH-TABLE and LINE-LOOKUP."
  (let* ((hash        (inline-annotate-parser--next-word))
         (_old-line   (inline-annotate-parser--next-word))
         (cur-line    (inline-annotate-parser--next-num))
         (lines       (inline-annotate-parser--next-num))
         (cur-commit  (gethash hash hash-table (make-commit))))

    (dotimes (n lines)
      (aset line-lookup (+ cur-line n -1) hash))

    (inline-annotate-parser--collect-line)
    (inline-annotate-parser--parse-commit-section cur-commit #'(lambda () (parsec-str "filename")))

    (puthash hash cur-commit hash-table)))


(defun inline-annotate-parser--incremental-parse (str num-lines)
  "Parse output of git blame --incremental as STR and NUM-LINES the number of lines in the buffer."
  (inline-annotate-parser--abstract-parse str num-lines 'inline-annotate-parser--incremental-parse-commit))

(defun inline-annotate-parser--parse (str num-lines incremental)
  "Parse STR. NUM-LINES upper bound for number of lines in str, Use INCREMENTAL strategy if set."
  (if incremental
      (inline-annotate-parser--incremental-parse str num-lines)
      (inline-annotate-parser--porcelain-parse str num-lines)))

(provide 'inline-annotate-parser)
;;; inline-annotate-parser.el ends here
