;;; gitlens.el --- Show git blame info about the current line         -*- lexical-binding: t; -*-
;;; Commentary:
;; Heavily inspired by the VSCode gitlens project and blamer.el
;;; Code:

(require 'subr-x)
(require 'async)
(require 'vc)
(require 'gitlens-async)

(defconst gitlens--min-indent 85)
(defconst gitlens--eol-offset 5)
(defconst gitlens--eob-offset 5)
(defconst gitlens--idle-delay 1)
(defconst gitlens--not-committed-hash "0000000000000000000000000000000000000000")

(defvar-local gitlens--line-lookup        nil)
(defvar-local gitlens--hash-table         nil)
(defvar-local gitlens--user-name          nil)
(defvar-local gitlens--line-active-lookup nil)
(defvar-local gitlens--is-fetching        nil)
(defvar-local gitlens--overlays           nil)
(defvar-local gitlens--generation         0)
;; Track a generation number to detect and ignore out-of-date async fetches
(defvar-local gitlens--active             nil)
(defvar-local gitlens--hooked             nil)

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

(defface gitlens-face
  '((t :foreground "#7a88cf"))
  "Face for blamer info.")

(defun gitlens--clear-overlays ()
  "Delete all overlays in the current buffer."
  (overlay-recenter (point-max))
  (dolist (ov gitlens--overlays)
    (delete-overlay ov)))

(defun gitlens--make-overlay ()
  "Create a new overlay."
  (let* ((line-end (line-end-position))
         (new-ov   (make-overlay line-end line-end nil t t)))
    (overlay-put new-ov 'intangible t)
    (setq gitlens--overlays (cons new-ov gitlens--overlays))))

(defun gitlens--prettify-time (commit-ts)
  "Pretty print the COMMIT-TS."
  (let* ((current-decoded  (decode-time (current-time)))
         (commit-decoded   (decode-time commit-ts))
         (sec-diff    (- (time-convert (current-time) 'integer) commit-ts))
         (year-diff   (- (decoded-time-year  current-decoded)
                         (decoded-time-year  commit-decoded)))
         (month-diff  (- (decoded-time-month current-decoded)
                         (decoded-time-month commit-decoded))))
    (cond ((< sec-diff 60)        "Seconds ago")
          ((< sec-diff 120)       "One minute ago")
          ((< sec-diff 3600)      (format "%s minutes ago" (/ sec-diff 60)))
          ((< sec-diff 7200)      "One hour ago")
          ((< sec-diff 86400)     (format "%s hours ago"   (/ sec-diff 3600)))
          ((< sec-diff 172800)    "Yesterday")
          ((< sec-diff 604800)    (format "%s days ago"    (/ sec-diff 86400)))
          ((< sec-diff 1209600)   "Last week")
          ((< sec-diff 2592000)   (format "%s weeks ago"   (/ sec-diff 604800)))
          ((> year-diff  1)       (format "%s years ago"   year-diff))
          ((= year-diff  1)       "Last year")
          ((> month-diff 1)       (format "%s months ago"  month-diff))
          (t                      "Last month"))))

(defun gitlens--format-uncommitted ()
  "What to display when the change hasn't been commited yet."
  (propertize "You ⟐ Not Committed Yet"
              'face `(:inherit (gitlens-face))
              'intangible t
              'cursor t))

(defun gitlens--format-commit (commit)
  "Format the COMMIT."
  (let ((summary (commit-summary commit))
        (author  (commit-author  commit))
        (commit-ts (gitlens--prettify-time (commit-author-time commit))))
    (when (string= author gitlens--user-name)
      (setq author "You"))
    (propertize (concat author " ⟐ " commit-ts " ⟐ " summary)
                'face `(:inherit (gitlens-face))
                'intangible t
                'cursor t)))

(defun gitlens--invalidate ()
  "Invalidate caches."
  (setq gitlens--line-lookup        nil
        gitlens--hash-table         nil
        gitlens--line-active-lookup nil)
  (cl-incf gitlens--generation))

(defun gitlens--has-cache ()
  "Return t if data is cached."
  (when (and gitlens--hash-table gitlens--line-lookup) t))

(defun gitlens--current-line-empty-p ()
  "Is the current line empty?"
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun gitlens--should-fetch ()
  "Decide wither gitlens should fetch blame data."
  (and (eq (vc-backend (buffer-file-name)) 'Git)
       (not (buffer-modified-p))
       (not (gitlens--has-cache))))

(defun gitlens--should-display ()
  "Decide whether gitlens should annotate the current buffer."
  ;; must be in git repo and backed by a file
  ;; disk version must match buffer version
  (and (eq (vc-backend (buffer-file-name)) 'Git)
       (gitlens--has-cache)
       (not (buffer-modified-p))))
       ;; (not (gitlens--current-line-empty-p))))

(defun gitlens--annotate (overlay)
  "Fill OVERLAY with commit data."
  (let* ((line-beg       (line-beginning-position))
         (line-end       (line-end-position))
         (indentation    (max (+ gitlens--eol-offset line-end)
                              (+ gitlens--min-indent line-beg)))
         (left-pad       (make-string (- indentation line-end) ?\s))
         (line-idx       (- (string-to-number (format-mode-line "%l")) 1)))

     (when (< line-idx (length gitlens--line-lookup))
       (let* ((commit-id      (aref gitlens--line-lookup line-idx))
              (commit         (gethash commit-id gitlens--hash-table))
              (str
               (concat left-pad
                       (if (string= commit-id gitlens--not-committed-hash)
                          (gitlens--format-uncommitted)
                          (gitlens--format-commit commit)))))
         (overlay-put overlay
                      'after-string
                      (propertize str
                                 'face `(:inherit (gitlens-face))
                                 'intangible t
                                 'cursor t))))))

(defun gitlens--annotate-multiple ()
  "Add overlays for a line or a region."
  (gitlens--clear-overlays)
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
               ;; don't touch visible overlays
              (when t ;;(invisible-p (car next-overlay))
                ;; TODO Consult bitmap on whether line
                ;; (overlay-start ov) has active overlay
                (let ((ov       (car next-overlay))
                      (line-end (line-end-position)))
                  (overlay-put ov 'invisible nil)
                  (delete-overlay ov)
                  (move-overlay   ov line-end line-end)
                  (gitlens--annotate ov)
                  (setq next-overlay (cdr next-overlay))))
              (next-logical-line)
              (cl-incf cur-line)))
          (while (and (<= (point) end))
            (unless nil ;; (aref gitlens--active-lines-bv cur-line)
              (gitlens--make-overlay)
              (gitlens--annotate (car gitlens--overlays)))
            (next-logical-line)))))
            ;;(cl-incf cur-line)))))
    (if gitlens--overlays
      (let ((ov       (car gitlens--overlays))
            (line-end (line-end-position)))
        (delete-overlay ov)
        (move-overlay   ov line-end line-end)
        (gitlens--annotate (car gitlens--overlays)))
      (gitlens--annotate
       (car (gitlens--make-overlay))))))

(defun gitlens--store (result)
  "Store the RESULT if it's still valid."
  (unless (buffer-modified-p)
    (message "Gitlens: Caching result.")
    (setq gitlens--hash-table         (car result)
          gitlens--line-lookup        (cdr result)
          gitlens--line-active-lookup (make-vector (length (cdr result)) nil))))

(defun gitlens--fetch-callback (result gen)
  "Draw Overlays with  RESULT data. GEN represents the age of the result."
  (when (= gen gitlens--generation) ;;fetch is still up to date
    (message "Gitlens: Result is up-to-date.")
    (gitlens--store result)
    (gitlens--post-command-callback)))

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
            (when-let  ((blame-info (gitlens-async--get-blame-info
                                             file-name is-incremental)))
              (cons (gitlens-async--get-user-name)
               (gitlens-parser--parse blame-info line-count is-incremental)))))
        (lambda (result)
          (message "Gitlens: Received result.")
          (with-current-buffer buffer
            (setq gitlens--is-fetching nil)
            (when result
              (message "Gitlens: Result not empty.")
              (setq gitlens--user-name (string-trim (car result)))
              (funcall callback (cdr result) my-gen))))))))

(defun gitlens--change-callback (_start _end _pre)
  "Reset gitlens on file change."
  (when gitlens--active
    (message "Gitlens: Cleaning up and invalidating cache.")
    (gitlens--clear-overlays)
    (gitlens--invalidate)))

(defun gitlens--post-command-callback ()
  "Display GitLens information."
  (if (gitlens--should-display)
    (gitlens--annotate-multiple)
    (gitlens--clear-overlays)))

(defun gitlens--idle-callback ()
  "Fetch annotations for the current buffer while idle."
  (when (gitlens--should-fetch)
    (setq gitlens--active t)
    (unless gitlens--hooked
      (add-hook 'after-change-functions 'gitlens--change-callback)
      (add-hook 'post-command-hook 'gitlens--post-command-callback)
      (setq gitlens--hooked t))
    (unless (gitlens--has-cache)
      (gitlens--fetch 'gitlens--fetch-callback))))

(defun gitlens--init ()
  "Start the idle timer."
  (interactive)
  (run-with-idle-timer gitlens--idle-delay t 'gitlens--idle-callback))

(provide 'gitlens)
;;; gitlens.el ends here
