;;; gitlens.el --- Show git blame info about the current line         -*- lexical-binding: t; -*-
;;; Commentary:
;; Heavily inspired by the VSCode gitlens project and blamer.el
;;; Code:

(require 'subr-x)
(require 'async)
(require 'gitlens-parser)


(defconst gitlens--blame-command-incremental "git blame --incremental --root %s"
  "Command to run to get git blame info (porcelain), %s is substituted with file-path.")

(defconst gitlens--blame-command-porcelain "git blame --porcelain --root %s"
  "Command to run to get git blame info (incremental), %s is substituted with file-path.")

(defconst gitlens--blame-command-for-lines "git blame --porcelain --root -L %s,%s %s"
  "Command to run to get git blame info for a range of lines.")

(defconst gitlens--min-indent 50)
(defconst gitlens--eol-offset 10)
(defconst gitlens--eob-offset 5)

(defvar gitlens--line-lookup nil)
(defvar gitlens--hash-table nil)
(defvar gitlens--line-active-lookup nil)
(defvar gitlens--is-fetching nil)
(defvar gitlens--overlays nil)

;;
;; Hashless hash table
;; Idea: We don't need to rehash the git commit hashes
;;
;; (probably not worth it, we barely beat sxhash performance)
;;
;; (defun gitlens--hash-test (key1 key2)
;;   (string= key1 key2))
;;
;; (defun gitlens--hash-hash (key)
;;   (string-to-number (substring key 0 8) 16))
;;

(defun gitlens--get-blame-info (file-name &optional incremental)
  "Run git-blame on FILE-NAME. Use --incremental if INCREMENTAL is non-nil."
  (when file-name
    (let ((command-str (if incremental (prog1 gitlens--blame-command-incremental)
                          (prog1 gitlens--blame-command-porcelain))))
      (shell-command-to-string
       (format command-str file-name)))))

;;; gitlens.el ends here
;;;

(defun gitlens--clear-overlays ()
  "Delete all overlays in the current buffer."
  (dolist (ov gitlens--overlays)
    (delete-overlay ov)))

(defun gitlens--format-commit (commit)
  "Format the COMMIT."
  (commit-summary commit))


(gitlens--clear-overlays)
(defun gitlens--annotate (overlay)
  "Fill overlay with commit data."
  (let* ((line-beg       (line-beginning-position))
         (line-end       (line-end-position))
         (indentation    (max (+ gitlens--eol-offset line-end)
                              (+ gitlens--min-indent line-beg)))
         (left-pad       (make-string (- indentation line-end) ?\s))
         (line-idx       (string-to-number (format-mode-line "%l")))
         (commit-id      (aref gitlens--line-lookup line-idx))
         (commit         (gethash commit-id gitlens--hash-table)))

     (overlay-put overlay 'after-string
                     (concat left-pad (gitlens--format-commit commit)))))

(defun gitlens--annotate-multiple ()
  "Add overlays for a line or a region."
  (if (region-active-p)
    (dolist (bounds (region-bounds))
      (let ((start  (car bounds))
            (end    (cdr bounds))
            (cur-line)
            (next-overlay gitlens--overlays))
        (save-excursion
          (goto-char start)
          (goto-char (line-beginning-position))
          (setq cur-line (- string-to-number (format-mode-line "%l")))
          (while (and (<= (point) end) next-overlay) ;; reuse overlays
            (unless nil ;;(aref gitlens--line-active-lookup cur-line)
              (when t   ;;(invisible-p (car next-overlay))   ;; don't touch visible overlays
                ;; TODO Consult bitmap on whether line (overlay-start ov) has active overlay
                (let ((ov       (car next-overlay))
                      (line-end (line-end-position)))
                  (progn
                    (overlay-put ov 'invisible nil)
                    (delete-overlay ov)
                    (move-overlay   ov line-end line-end)
                    (gitlens--annotate ov)
                    (setq next-overlay (cdr next-overlay)))))
              (next-logical-line)
              (cl-incf cur-line)))
          (while (and (<= (point) end))
            (unless nil ;; (aref gitlens--active-lines-bv cur-line)
              (setq gitlens--overlays
                    (cons (make-overlay (line-end-position) (line-end-position))
                          gitlens--overlays))
              (gitlens--annotate (car gitlens--overlays)))
            (next-logical-line)))))
            ;;(cl-incf cur-line)))))
    (if gitlens--overlays
      (let ((ov       (car gitlens--overlays))
            (line-end (line-end-position)))
        (delete-overlay ov)
        (move-overlay   ov (line-end-position) (line-end-position))
        (gitlens--annotate (car gitlens--overlays)))
      (gitlens--annotate (setq gitlens--overlays
                               (cons (make-overlay (line-end-position) (line-end-position))
                                     gitlens--overlays))))))

(defun gitlens--display (result)
  "Display the RESULT."
  (unless (buffer-modified-p)
    (message "Received result 2")
    (setq gitlens--hash-table (car result))
    (setq gitlens--line-lookup (cdr result))
    (setq gitlens--line-active-lookup (make-vector (length (cdr result)) nil))
    (gitlens--annotate-multiple)))

(defun gitlens--fetch ()
  "Async-fetch git blame info for FILE-NAME call CALLBACK when done."
  (unless gitlens--is-fetching
    (message "Gitlens is fetching")
    (setq gitlens--is-fetching t)
    (when-let ((file-name  (buffer-file-name))
               (line-count (count-lines (point-min) (point-max))))
      (setq gitlens--is-fetching t)
      (async-start
        (lambda ()
          (progn
            (load-file "~/.emacs.d/.local/straight/repos/parsec.el/parsec.el")
            (load-file "~/Projects/gitlens/gitlens-parser.el")
            (load-file "~/Projects/gitlens/gitlens.el")
            (when-let ((blame-info (gitlens--get-blame-info file-name t)))
              (gitlens-parser--incremental-parse blame-info line-count))))
        (lambda (result)
          (setq gitlens--is-fetching nil)
          (message "Received result 0")
          (when result
            (message "Received result 1")
            (gitlens--display result)))))))

(defun gitlens--init ()
  "Start the idle timer."
  (unless (buffer-modified-p)
     (run-with-idle-timer 3 t
                          (lambda () (if (and gitlens--hash-table gitlens--line-lookup)
                                         (gitlens--annotate-multiple)
                                         (gitlens--fetch))))))
