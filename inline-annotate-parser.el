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

(defmacro ia-parser--setter (key struct)
  "Create lambda v -> (setf (KEY STRUCT) v)."
  `(lambda (val) (setf (,key ,struct) val)))

;;
;; Parsec Helpers
;;

(defun ia-parser--collect-line ()
  "Collect the rest of the line."
  (prog1 (parsec-re "[^\n]*")
    (parsec-eol-or-eof)))

(defun ia-parser--skip-ws ()
  "Consume one or more whitespace characters."
  (parsec-many (parsec-ch ?\s)))

(defun ia-parser--next-word ()
  "Consume the next word, as well as any whitespace before it."
  (ia-parser--skip-ws)
  (parsec-re "[[:alnum:]]*"))

(defun ia-parser--next-num ()
  "Consume the next number, as well as any whitespace before it."
  (string-to-number (ia-parser--next-word)))

;;
;; Parser helpers
;;

(defun ia-parser--parse-key-value-line (key setter &optional cast-to-num)
  "If the next string is KEY call SETTER with the rest of the line as parameter. CAST-TO-NUM when set."
  (when (parsec-str key)
    (ia-parser--skip-ws)
    (funcall setter (if cast-to-num
                      (string-to-number (ia-parser--collect-line))
                      (ia-parser--collect-line)))))


(defun ia-parser--parse-header-line (commit-struct)
  "Parse a line and store it in COMMIT-STRUCT."
  (parsec-or
    (ia-parser--parse-key-value-line "author-mail"    (ia-parser--setter commit-author-mail    commit-struct))
    (ia-parser--parse-key-value-line "author-time"    (ia-parser--setter commit-author-time    commit-struct) t)
    (ia-parser--parse-key-value-line "author-tz"      (ia-parser--setter commit-author-tz      commit-struct))
    (ia-parser--parse-key-value-line "author"         (ia-parser--setter commit-author         commit-struct))
    (ia-parser--parse-key-value-line "summary"        (ia-parser--setter commit-summary        commit-struct))
    (ia-parser--collect-line)))

(defun ia-parser--parse-commit-section (cur-commit separator)
  "Parse git blame header, store in CUR-COMMIT, stop at SEPARATOR."
  ;; parse headers
  (parsec-many-till
   (ia-parser--parse-header-line cur-commit)
   (funcall separator))

  ;; discard line contents
  (ia-parser--collect-line))

(defun ia-parser--abstract-parse(str num-lines commit-parser)
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

(defun ia-parser--porcelain-parse-commit (hash-table line-lookup)
  "Parse git blame --porcelain, Store information in HASH-TABLE and LINE-LOOKUP."
  (let* ((hash        (ia-parser--next-word))
         (_old-line   (ia-parser--next-word))
         (cur-line    (ia-parser--next-num))
         (lines       (ia-parser--next-num))
         (cur-commit  (gethash hash hash-table (make-commit))))

    (dotimes (n lines)
      (aset line-lookup (+ cur-line n -1) hash)
      (ia-parser--collect-line)  ;; drop the other headers, we don't need them
      (ia-parser--parse-commit-section cur-commit #'(lambda () (parsec-ch ?\t))))

    (puthash hash cur-commit hash-table)))


(defun ia-parser--porcelain-parse (str num-lines)
  "Parse output of git blame --porcelain as STR and NUM-LINES the number of lines in the buffer."
  (ia-parser--abstract-parse str num-lines 'ia-parser--porcelain-parse-commit))


;;
;; Incremental Parser
;;


(defun ia-parser--incremental-parse-commit (hash-table line-lookup)
  "Parse git blame --incremental, Store information in HASH-TABLE and LINE-LOOKUP."
  (let* ((hash        (ia-parser--next-word))
         (_old-line   (ia-parser--next-word))
         (cur-line    (ia-parser--next-num))
         (lines       (ia-parser--next-num))
         (cur-commit  (gethash hash hash-table (make-commit))))

    (dotimes (n lines)
      (aset line-lookup (+ cur-line n -1) hash))

    (ia-parser--collect-line)
    (ia-parser--parse-commit-section cur-commit #'(lambda () (parsec-str "filename")))

    (puthash hash cur-commit hash-table)))


(defun ia-parser--incremental-parse (str num-lines)
  "Parse output of git blame --incremental as STR and NUM-LINES the number of lines in the buffer."
  (ia-parser--abstract-parse str num-lines 'ia-parser--incremental-parse-commit))

(defun ia-parser--parse (str num-lines incremental)
  "Parse STR. NUM-LINES upper bound for number of lines in str, Use INCREMENTAL strategy if set."
  (if incremental
      (ia-parser--incremental-parse str num-lines)
      (ia-parser--porcelain-parse str num-lines)))

(provide 'inline-annotate-parser)
;;; inline-annotate-parser.el ends here
