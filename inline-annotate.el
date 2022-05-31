;;; inline-annotate.el --- Show git blame info about the current line         -*- lexical-binding: t; -*-
;;; Commentary:
;; Heavily inspired by the VSCode's gitlens plygin and blamer.el
;;; Code:

(require 'vc)
(require 'async)
(require 'subr-x)
(require 'time-date)
(require 'inline-annotate-async)

(defconst inline-annotate--min-indent 85)
(defconst inline-annotate--eol-offset 5)
(defconst inline-annotate--eob-offset 5)
(defconst inline-annotate--idle-delay 1)
(defconst inline-annotate--not-committed-hash
  "0000000000000000000000000000000000000000")

(defvar-local inline-annotate--line-lookup        nil)
(defvar-local inline-annotate--hash-table         nil)
(defvar-local inline-annotate--user-name          nil)
(defvar-local inline-annotate--line-active-lookup nil)
(defvar-local inline-annotate--is-fetching        nil)
(defvar-local inline-annotate--overlays           nil)
(defvar-local inline-annotate--generation         0)
;; Track a generation number to detect and ignore out-of-date async fetches
(defvar-local inline-annotate--hooked             nil)

(defvar inline-annotate--idle-timer nil)

;; Hashless hash table
;; Idea: We don't need to rehash the git commit hashes
;;
;; (probably not worth it, we barely beat sxhash performance)
;;
;; (defun inline-annotate--hash-test (key1 key2)
;;   (string= key1 key2))
;;
;; (defun inline-annotate--hash-hash (key)
;;   (string-to-number (substring key 0 8) 16))


(defgroup inline-annotate nil
  "Show commit info at the end of a current line."
  :group 'tools)

(defface inline-annotate-face
  '((t :foreground "#7a88cf"))
  "Face for blamer info.")

(defun inline-annotate--clear-overlays ()
  "Delete all overlays in the current buffer."
  (overlay-recenter (point-max))
  (dolist (ov inline-annotate--overlays)
    (delete-overlay ov)))

(defun inline-annotate--make-overlay ()
  "Create a new overlay."
  (let* ((line-end (line-end-position))
         (new-ov   (make-overlay line-end line-end nil t t)))
    (overlay-put new-ov 'intangible t)
    (setq inline-annotate--overlays (cons new-ov inline-annotate--overlays))))

(defun inline-annotate--prettify-time (commit-ts)
  "Pretty print the COMMIT-TS."
    ;; seconds to time:
    ;; 60     -> 1 min
    ;; 120    -> 2 mins
    ;; 3600   -> 1 hr
    ;; 7200   -> 2 hrs
    ;; 21600  -> 6 hrs
    ;; 86400  -> 24 hrs
    ;; 259200 -> 3 days
    ;; 604800 -> 7 days = 1 week
    ;; 1209600 -> 14 days = 2 weeks
  (let* ((current-decoded  (decode-time (current-time)))
         (commit-decoded   (decode-time commit-ts))
         (sec-diff    (- (time-convert (current-time) 'integer) commit-ts))
         (year-diff    (- (decoded-time-year current-decoded)
                          (decoded-time-year commit-decoded)))
         (month-diff   (mod (- (decoded-time-month current-decoded)
                               (decoded-time-month commit-decoded))
                            12))
         (day-diff     (mod (- (decoded-time-day current-decoded)
                               (decoded-time-day commit-decoded))
                            (date-days-in-month
                             (decoded-time-year commit-decoded)
                             (decoded-time-month commit-decoded))))
         (week-day     (decoded-time-weekday commit-decoded))
         (start-of-week-diff     (+ day-diff week-day)))

    (cond ((> year-diff 1)              (format "%s years ago" year-diff))
          ((and (= year-diff 1)
                (or (= month-diff 0)
                    (>= month-diff 6))  "Last year"))
          ((> month-diff 1)             (format "%s months ago" month-diff))
          ((and (= month-diff 1)
                (or (= day-diff 0)
                    (>= day-diff  14))) "Last month")
          ((>= start-of-week-diff 14)   (format "%s weeks ago"
                                                (/ start-of-week-diff 7)))
          ((and (>= start-of-week-diff  7)
                (>= day-diff 3))        "Last week")
          ((and (>= day-diff 1)
                (>= sec-diff 21600))    "Yesterday")
          ((>= sec-diff 7200)           (format "%s hours ago"
                                                (/ sec-diff 3600)))
          ((>= sec-diff 3600)           "An hour ago")
          ((>= sec-diff 120)            (format "%s minutes ago"
                                                (/ sec-diff 60)))
          ((>= sec-diff 60)             "A minute ago")
          (t                            "Seconds ago"))))

(defun inline-annotate--format-uncommitted ()
  "What to display when the change hasn't been commited yet."
  (propertize "You ⟐ Not Committed Yet"
              'face `(:inherit (inline-annotate-face))
              'intangible t
              'cursor t))

(defun inline-annotate--format-commit (commit)
  "Format the COMMIT."
  (let ((summary (commit-summary commit))
        (author  (commit-author  commit))
        (commit-ts (inline-annotate--prettify-time (commit-author-time commit))))
    (when (string= author inline-annotate--user-name)
      (setq author "You"))
    (propertize (concat author " ⟐ " commit-ts " ⟐ " summary)
                'face `(:inherit (inline-annotate-face))
                'intangible t
                'cursor t)))

(defun inline-annotate--invalidate ()
  "Invalidate caches."
  (setq inline-annotate--line-lookup        nil
        inline-annotate--hash-table         nil
        inline-annotate--line-active-lookup nil)
  (cl-incf inline-annotate--generation))

(defun inline-annotate--has-cache ()
  "Return t if data is cached."
  (and inline-annotate--hash-table inline-annotate--line-lookup))

(defun inline-annotate--current-line-empty-p ()
  "Is the current line empty?"
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun inline-annotate--should-fetch ()
  "Decide wither inline-annotate should fetch blame data."
  (and (eq (vc-backend (buffer-file-name)) 'Git)
       (not (buffer-modified-p))
       (not (inline-annotate--has-cache))
       inline-annotate-mode))

(defun inline-annotate--should-display ()
  "Decide whether inline-annotate should annotate the current buffer."
  ;; must be in git repo and backed by a file
  ;; disk version must match buffer version
  (and (eq (vc-backend (buffer-file-name)) 'Git)
       (inline-annotate--has-cache)
       (not (buffer-modified-p))
       (not (= (point) (point-max)))
       inline-annotate-mode))

(defun inline-annotate--annotate (overlay)
  "Fill OVERLAY with commit data."
  (let* ((line-beg       (line-beginning-position))
         (line-end       (line-end-position))
         (indentation    (max (+ inline-annotate--eol-offset line-end)
                              (+ inline-annotate--min-indent line-beg)))
         (left-pad       (make-string (- indentation line-end) ?\s))
         (line-idx       (string-to-number (format-mode-line "%l"))))

     (when (< line-idx (length inline-annotate--line-lookup))
       (let* ((commit-id      (aref inline-annotate--line-lookup (- line-idx 1)))
              (commit         (gethash commit-id inline-annotate--hash-table))
              (str
               (concat left-pad
                       (if (string= commit-id inline-annotate--not-committed-hash)
                          (inline-annotate--format-uncommitted)
                          (inline-annotate--format-commit commit)))))
         (overlay-put overlay
                      'after-string
                      (propertize str
                                 'face `(:inherit (inline-annotate-face))
                                 'intangible t
                                 'cursor t))))))

(defun inline-annotate--annotate-multiple ()
  "Add overlays for a line or a region."
  (inline-annotate--clear-overlays)
  (if (region-active-p)
    (dolist (bounds (region-bounds))
      (let ((start  (car bounds))
            (end    (cdr bounds))
            (cur-line)
            (next-overlay inline-annotate--overlays))
        (save-excursion
          (goto-char start)
          (goto-char (line-beginning-position))
          (setq cur-line (- (string-to-number (format-mode-line "%l"))))
          (while (and (<= (point) end) next-overlay) ;; reuse overlays
            (unless nil ;;(aref inline-annotate--line-active-lookup cur-line)
               ;; don't touch visible overlays
              (when t ;;(invisible-p (car next-overlay))
                ;; TODO Consult bitmap on whether line
                ;; (overlay-start ov) has active overlay
                (let ((ov       (car next-overlay))
                      (line-end (line-end-position)))
                  (overlay-put ov 'invisible nil)
                  (delete-overlay ov)
                  (move-overlay   ov line-end line-end)
                  (inline-annotate--annotate ov)
                  (setq next-overlay (cdr next-overlay))))
              (next-logical-line)
              (cl-incf cur-line)))
          (while (and (<= (point) end))
            (unless nil ;; (aref inline-annotate--active-lines-bv cur-line)
              (inline-annotate--make-overlay)
              (inline-annotate--annotate (car inline-annotate--overlays)))
            (next-logical-line)))))
            ;;(cl-incf cur-line)))))
    (if inline-annotate--overlays
      (let ((ov       (car inline-annotate--overlays))
            (line-end (line-end-position)))
        (delete-overlay ov)
        (move-overlay   ov line-end line-end)
        (inline-annotate--annotate (car inline-annotate--overlays)))
      (inline-annotate--annotate
       (car (inline-annotate--make-overlay))))))

(defun inline-annotate--store (result)
  "Store the RESULT if it's still valid."
  (unless (buffer-modified-p)
    (message "Inline-Annotate: Caching result.")
    (setq inline-annotate--hash-table         (car result)
          inline-annotate--line-lookup        (cdr result)
          inline-annotate--line-active-lookup
          (make-vector (length (cdr result)) nil))))

(defun inline-annotate--fetch-callback (result gen)
  "Draw Overlays with  RESULT data. GEN represents the age of the result."
  (when (= gen inline-annotate--generation) ;;fetch is still up to date
    (inline-annotate--store result)
    (inline-annotate--display-cached-data)))

(defun inline-annotate--fetch (callback)
  "Async-fetch git blame info for the current buffer, call CALLBACK when done.
When buffer is nil, use the current buffer."
  (unless inline-annotate--is-fetching
    (message "Inline-Annotate: Started fetching.")
    (setq inline-annotate--is-fetching t)
    (when-let ((file-name    (buffer-file-name))
               (line-count   (count-lines (point-min) (point-max)))
               (my-gen       inline-annotate--generation)
               (buffer       (buffer-name))
               (paths        (list (locate-library "parsec")
                                   (locate-library "inline-annotate-parser")
                                   (locate-library "inline-annotate-async"))))
      (message (number-to-string (length paths)))
      (setq inline-annotate--is-fetching t)
      (unwind-protect
        (async-start
          (lambda ()
            (dolist (path paths)
              (load-file path))
            (let ((is-incremental t))
              (when-let  ((blame-info (inline-annotate-async--get-blame-info
                                               file-name is-incremental)))
                (cons (inline-annotate-async--get-user-name)
                 (inline-annotate-parser--parse blame-info
                                                line-count is-incremental)))))
          (lambda (result)
            (message "Inline-Annotate: Received result.")
            (with-current-buffer buffer
              (setq inline-annotate--is-fetching nil)
              (when result
                (setq inline-annotate--user-name (string-trim (car result)))
                (funcall callback (cdr result) my-gen)))))
        (setq inline-annotate--is-fetching nil)))))

(defun inline-annotate--clear-display-and-invalidate-cache (_start _end _pre)
  "Reset inline-annotate on file change."
  (inline-annotate--clear-overlays)
  (inline-annotate--invalidate))

(defun inline-annotate--display-cached-data ()
  "Display Inline-Annotate information."
  (if (inline-annotate--should-display)
    (inline-annotate--annotate-multiple)
    (inline-annotate--clear-overlays)))

(defun inline-annotate--fetch-if-cache-empty ()
  "Fetch annotations for the current buffer while idle."
  (when (and (inline-annotate--should-fetch)
             (not (inline-annotate--has-cache)))
    (inline-annotate--fetch 'inline-annotate--fetch-callback)))

;;;###autoload
(define-minor-mode inline-annotate-mode
  "Inline-Annotate mode."
  :init-value nil
  :global nil
  :lighter nil
  (inline-annotate--clear-overlays)
  (setq inline-annotate--line-lookup        nil
        inline-annotate--hash-table         nil
        inline-annotate--user-name          nil
        inline-annotate--line-active-lookup nil
        inline-annotate--is-fetching        nil
        inline-annotate--overlays           nil
        inline-annotate--generation         0
        inline-annotate--hooked             nil)
  (cond
   (inline-annotate-mode
    (add-hook 'after-change-functions 'inline-annotate--clear-display-and-invalidate-cache)
    (add-hook 'post-command-hook 'inline-annotate--display-cached-data)
    (add-hook 'after-save-hook 'inline-annotate--fetch-if-cache-empty))
   (t
    (remove-hook 'after-change-functions 'inline-annotate--clear-display-and-invalidate-cache)
    (remove-hook 'post-command-hook 'inline-annotate--display-cached-data)
    (remove-hook 'after-save-hook 'inline-annotate--fetch-if-cache-empty))))

(provide 'inline-annotate)
;;; inline-annotate.el ends here
