;;; inline-annotate.el --- Show git blame info about the current line         -*- lexical-binding: t; -*-
;;; Commentary:
;; Display the last change that touched the current line at the end of the line
;;
;; -- Heavily inspired by the VSCode's gitlens plygin and blamer.el
;;; Code:

(require 'vc)
(require 'async)
(require 'subr-x)
(require 'time-date)
(require 'inline-annotate-async)

(defconst ia--min-indent 85)
(defconst ia--eol-offset 5)
(defconst ia--eob-offset 5)
(defconst ia--idle-delay 1)
(defconst ia--not-committed-hash
  "0000000000000000000000000000000000000000")
(defconst ia--uncommitted-format-string "You · Not Committed Yet")
(defconst ia--committed-format-string "%s · %s · %s")

(defvar-local ia--line-lookup        nil)
(defvar-local ia--hash-table         nil)
(defvar-local ia--cache-age          nil)
(defvar-local ia--user-name          nil)
(defvar-local ia--line-active-lookup nil)
(defvar-local ia--is-fetching        nil)
(defvar-local ia--overlays           nil)
;; Track a generation number to detect and ignore out-of-date async fetches
(defvar-local ia--generation         0)

(defgroup inline-annotate nil
  "Show commit info at the end of a current line."
  :group 'tools)

(defface inline-annotate-face
  '((t :foreground "#7a88cf"))
  "Face for blamer info.")

(defun ia--clear-overlays ()
  "Delete all overlays in the current buffer."
  (overlay-recenter (point-max))
  (dolist (ov ia--overlays)
    (delete-overlay ov)))

(defun ia--make-overlay ()
  "Create a new overlay."
  (let* ((line-end (line-end-position))
         (new-ov   (make-overlay line-end line-end nil t t)))
    (overlay-put new-ov 'intangible t)
    (setq ia--overlays (cons new-ov ia--overlays))))

(defun ia--prettify-time (commit-ts)
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

(defun ia--format-uncommitted ()
  "What to display when the change hasn't been commited yet."
  (propertize ia--uncommitted-format-string
              'face `(:inherit (inline-annotate-face))
              'intangible t
              'cursor t))

(defun ia--format-commit (commit)
  "Format the COMMIT."
  (let ((summary (commit-summary commit))
        (author  (commit-author  commit))
        (commit-ts (ia--prettify-time (commit-author-time commit))))
    (when (string= author ia--user-name)
      (setq author "You"))
    (propertize (format ia--committed-format-string author commit-ts summary)
                'face `(:inherit (inline-annotate-face))
                'intangible t
                'cursor t)))

(defun ia--invalidate ()
  "Invalidate caches."
  (setq ia--line-lookup        nil
        ia--hash-table         nil
        ia--line-active-lookup nil)
  (cl-incf ia--generation))

(defun ia--has-cache ()
  "Return t if data is cached."
  (and ia--hash-table ia--line-lookup))

(defun ia--current-line-empty-p ()
  "Is the current line empty?"
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun ia--should-fetch ()
  "Decide wither inline-annotate should fetch blame data."
  (and (eq (vc-backend (buffer-file-name)) 'Git)
       (not (buffer-modified-p))
       (not (ia--has-cache))
       inline-annotate-mode))

(defun ia--should-display ()
  "Decide whether inline-annotate should annotate the current buffer."
  ;; must be in git repo and backed by a file
  ;; disk version must match buffer version
  (and (eq (vc-backend (buffer-file-name)) 'Git)
       (ia--has-cache)
       (not (buffer-modified-p))
       (not (= (point) (point-max)))
       inline-annotate-mode))

(defun ia--fill-overlay (overlay)
  "Fill OVERLAY with commit data."
  (let* ((line-beg       (line-beginning-position))
         (line-end       (line-end-position))
         (indentation    (max (+ ia--eol-offset line-end)
                              (+ ia--min-indent line-beg)))
         (left-pad       (make-string (- indentation line-end) ?\s))
         (line-idx       (string-to-number (format-mode-line "%l"))))

     (when (< line-idx (length ia--line-lookup))
       (let* ((commit-id      (aref ia--line-lookup (- line-idx 1)))
              (commit         (gethash commit-id ia--hash-table))
              (str
               (concat left-pad
                       (if (string= commit-id ia--not-committed-hash)
                          (ia--format-uncommitted)
                          (ia--format-commit commit)))))
         (overlay-put overlay
                      'after-string
                      (propertize str
                                 'face `(:inherit (inline-annotate-face))
                                 'intangible t
                                 'cursor t))))))

(defun ia--annotate ()
  "Add overlays for a line or a region."
  (ia--clear-overlays)
  (if ia--overlays
     (let ((ov       (car ia--overlays))
           (line-end (line-end-position)))
       (delete-overlay ov)
       (move-overlay   ov line-end line-end)
       (ia--fill-overlay (car ia--overlays)))
     (ia--fill-overlay
      (car (ia--make-overlay)))))

(defun ia--store (result)
  "Store the RESULT if it's still valid."
  (unless (buffer-modified-p)
    (setq ia--hash-table         (car result)
          ia--line-lookup        (cdr result)
          ia--line-active-lookup
          (make-vector (length (cdr result)) nil)
          ia--cache-age          (current-time))
    (message "Inline-Annotate: Done fetching.")))

(defun ia--fetch-callback (result gen)
  "Draw Overlays with  RESULT data. GEN represents the age of the result."
  (when (= gen ia--generation) ;;fetch is still up to date
    (ia--store result)
    (ia--display-cached-data)))

(defun ia--fetch (callback)
  "Async-fetch git blame info for the current buffer, call CALLBACK when done.
When buffer is nil, use the current buffer."
  (unless ia--is-fetching
    (message "Inline-Annotate: Started fetching.")
    (setq ia--is-fetching t)
    (when-let ((file-name    (buffer-file-name))
               (line-count   (count-lines (point-min) (point-max)))
               (my-gen       ia--generation)
               (buffer       (buffer-name))
               (paths        (list (locate-library "parsec")
                                   (locate-library "inline-annotate-parser")
                                   (locate-library "inline-annotate-async"))))
      (setq ia--is-fetching t)
      (unwind-protect
        (async-start
          (lambda ()
            (dolist (path paths)
              (load-file path))
            (let ((is-incremental t))
              (when-let  ((blame-info (ia-async--get-blame-info
                                               file-name is-incremental)))
                (cons (ia-async--get-user-name)
                 (ia-parser--parse blame-info
                                   line-count is-incremental)))))
          (lambda (result)
            (with-current-buffer buffer
              (setq ia--is-fetching nil)
              (when result
                (setq ia--user-name (string-trim (car result)))
                (funcall callback (cdr result) my-gen)))))
        (setq ia--is-fetching nil)))))

(defun ia--clear-display-and-invalidate-cache (&rest _trail)
  "Reset inline-annotate on file change."
  (ia--clear-overlays)
  (ia--invalidate))

(defun ia--display-cached-data ()
  "Display Inline-Annotate information."
  (if (ia--should-display)
    (ia--annotate)
    (ia--clear-overlays)))

(defun ia--detect-external-change ()
  "Return t if an external change to the file or repo happend."
  (let ((last-commit-time (thread-last "git log -1 --format=%ct"
                            (shell-command-to-string)
                            (string-to-number)
                            (seconds-to-time))))

    (or (time-less-p ia--cache-age last-commit-time)
        (not (verify-visited-file-modtime)))))


(defun ia--refresh-on-external-change (&rest _trail)
  "Refresh cache and display if an external change occured."
  (when (ia--detect-external-change)
    (ia--clear-display-and-invalidate-cache)
    (ia--fetch-if-cache-empty)))

(defun ia--fetch-if-cache-empty ()
  "Fetch annotations for the current buffer while idle."
  (when (and (ia--should-fetch))
    (ia--fetch 'ia--fetch-callback)))



;;;###autoload
(define-minor-mode inline-annotate-mode
  "Inline-Annotate mode."
  :init-value nil
  :global nil
  :lighter nil
  (ia--clear-overlays)
  (setq ia--line-lookup        nil
        ia--hash-table         nil
        ia--user-name          nil
        ia--line-active-lookup nil
        ia--is-fetching        nil
        ia--overlays           nil
        ia--generation         0)

  (cond
   (inline-annotate-mode
    (add-hook 'after-change-functions 'ia--clear-display-and-invalidate-cache)
    (add-hook 'post-command-hook 'ia--display-cached-data)
    (add-hook 'after-save-hook 'ia--fetch-if-cache-empty)
    (advice-add 'select-window :after #'ia--refresh-on-external-change)
    (ia--fetch-if-cache-empty))
   (t
    (remove-hook 'after-change-functions 'ia--clear-display-and-invalidate-cache)
    (remove-hook 'post-command-hook 'ia--display-cached-data)
    (remove-hook 'after-save-hook 'ia--fetch-if-cache-empty)
    (advice-remove 'select-window #'ia--refresh-on-external-change))))

;;;###autoload
(define-globalized-minor-mode
  global-inline-annotate-mode
  inline-annotate-mode
  (lambda ()
    (unless inline-annotate-mode
      (inline-annotate-mode))))

(provide 'inline-annotate)
;;; inline-annotate.el ends here
