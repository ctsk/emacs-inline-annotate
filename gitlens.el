;;; gitlens.el --- Show git blame info about the current line         -*- lexical-binding: t; -*-
;;; Commentary:
;; Heavily inspired by the VSCode gitlens project and blamer.el
;;; Code:

(require 'subr-x)
(require 'async)
(require 'vc)
(require 'gitlens-async)

(defconst gitlens--min-indent 50)
(defconst gitlens--eol-offset 10)
(defconst gitlens--eob-offset 5)

(defvar-local gitlens--line-lookup        nil)
(defvar-local gitlens--hash-table         nil)
(defvar-local gitlens--line-active-lookup nil)
(defvar-local gitlens--is-fetching        nil)
(defvar-local gitlens--overlays           nil)
(defvar-local gitlens--generation         0)
(defvar-local gitlens--active             nil);; Track a generation number to detect and ignore out-of-date async fetches

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



(defun gitlens--clear-overlays ()
  "Delete all overlays in the current buffer."
  (dolist (ov gitlens--overlays)
    (delete-overlay ov)))

(defun gitlens--format-commit (commit)
  "Format the COMMIT."
  (commit-summary commit))

(defun gitlens--invalidate ()
  "Invalidate caches."
  (setq gitlens--line-lookup        nil
        gitlens--hash-table         nil
        gitlens--line-active-lookup nil)
  (cl-incf gitlens--generation))

(defun gitlens--should-display ()
  "Decide whether gitlens should annotate the current buffer."
  (and (eq (vc-backend (buffer-file-name)) 'Git) ;; must be in git repo and backed by a file
       (not (buffer-modified-p))))               ;; disk version must match buffer version


(defun gitlens--annotate (overlay)
  "Fill OVERLAY with commit data."
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
          (setq cur-line (- (string-to-number (format-mode-line "%l"))))
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
        (move-overlay   ov line-end line-end)
        (gitlens--annotate (car gitlens--overlays)))
      (gitlens--annotate (car (setq gitlens--overlays
                                   (cons (make-overlay (line-end-position) (line-end-position))
                                         gitlens--overlays)))))))

(defun gitlens--display (result)
  "Display the RESULT."
  (unless (buffer-modified-p)
    (message "Gitlens: Caching result.")
    (setq gitlens--hash-table         (car result)
          gitlens--line-lookup        (cdr result)
          gitlens--line-active-lookup (make-vector (length (cdr result)) nil))
    (gitlens--annotate-multiple)))

(defun gitlens--fetch-callback (result gen)
  "What to do when done fetching and receiving result RESULT. GEN represents the age of the result."
  (when (= gen gitlens--generation) ;;fetch is still up to date
    (message "Gitlens: Result is up-to-date.")
    (gitlens--display result)))


(defun gitlens--fetch (callback)
  "Async-fetch git blame info for the current buffer, call CALLBACK when done.
When buffer is nil, use the current buffer."
  (unless gitlens--is-fetching
    (message "Gitlens: Started fetching.")
    (setq gitlens--is-fetching t)
    (when-let ((file-name    (buffer-file-name))
               (line-count   (count-lines (point-min) (point-max)))
               (my-gen       gitlens--generation)
               (buffer       (buffer-name)))
      (setq gitlens--is-fetching t)
      (async-start
        (lambda ()
          (load-file "~/.emacs.d/.local/straight/repos/parsec.el/parsec.el")
          (load-file "~/Projects/gitlens/gitlens-parser.el")
          (load-file "~/Projects/gitlens/gitlens-async.el")
          (let ((is-incremental t))
            (when-let ((blame-info (gitlens-async--get-blame-info file-name is-incremental)))
              (gitlens-parser--parse blame-info line-count is-incremental))))
        (lambda (result)
          (message "Gitlens: Received result.")
          (with-current-buffer buffer
            (setq gitlens--is-fetching nil)
            (when result
              (message "Gitlens: Result not empty.")
              (funcall callback result my-gen))))))))

(defun gitlens--change-callback (_start _end _pre)
  "Reset gitlens on file change."
  (when gitlens--active
    (message "Gitlens: Cleaning up and invalidating cache.")
    (gitlens--clear-overlays)
    (gitlens--invalidate)))

(defun gitlens--idle-callback ()
  "Annotates the current buffer when idle. Fetches data when necessary."
  (when (gitlens--should-display)
    (setq gitlens--active t)
    (if (and gitlens--hash-table gitlens--line-lookup)
        (gitlens--annotate-multiple)
        (gitlens--fetch 'gitlens--fetch-callback))))

(defun gitlens--init ()
  "Start the idle timer."
  (interactive)
  (unless (member 'gitlens--change-callback after-change-functions)
     (add-hook 'after-change-functions 'gitlens--change-callback))
  (run-with-idle-timer .05 t 'gitlens--idle-callback))

(provide 'gitlens)
;;; gitlens.el ends here
