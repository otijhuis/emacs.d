;; Examples

(defun other-window-backward (n)
  "Select Nth previous window."
  (interactive "p") ; if prefix interpret as nr, if no prefix interpret as nr 1
  (other-window (- n)))

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "p") ; if prefix interpret as nr, if no prefix interpret as nr 1
  (if n
      (other-window (- n)) ; if not nil
    (other-window -1)))

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "p") ; if prefix interpret as nr, if no prefix interpret as nr 1
  (other-window (- (or n 1))))

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P") ; note the capital P
  (other-window (- (prefix-numeric-value n))))

(defalias 'scroll-ahead 'scroll) ; alias under different name

;; () is the same as nil
;; t is truth
(symbolp ()) ; t
(symbolp '(a b c)) ; nil
(listp nil) ; t
(listp '(a b c)) ; t
(listp 'help-command) ; nil

(setq n t)
(or n 1)

(defun line-to-top ()
  ￼"Move current line to top of window."
  ￼(interactive) (recenter O))

;; post-command-hook
;; write-file-hooks
;; find-file-hooks

(defun read-only-if-symlink ()
  ￼(if (file-symlink-p buffer-file-name)
       ￼￼￼(progn
            (setq buffer-read-only t)
            (message "File is a symlink"))))
￼
(defun visit-target-instead ()
  "Replace this buffer with a buffer visiting the link target."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
        ￼
        (if target
            (find-alternate-file target)
          (error "Not visiting a symlink")))
    (error "Not visiting a file")))

(defun clobber-symlink ()
  "Replace symlink with a copy of the file."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
        ￼
        (if target
            (if (yes-or-no-p (format "Replace %s with %s?"
                                     buffer-file-name
                                     target))
                (progn
                  (delete-file buffer-file-name)
                  (write-file buffer-file-name)))
          (error "Not visiting a symlink")))
    (error "Not visiting a file")))

(defadvice switch-to-buffer (before existing-buffer
                                    ￼activate compile)
  ￼"When interactive, switch to existing buffers only,
￼unless given a prefix argument."
  ￼(interactive
    ￼(list (read-buffer "Switch to buffer:"
                        ￼(other-buffer)
                        (null current-prefix-arg)))))

;; last-command
;; symbol properties (put 'a-symbol 'some-property 1)
;; markers (defvar unscroll-point (make-marker) "desc") (set-marker some-marker (point))
;; (set-marker some-marker nil) before discarding it

;; docstring starting with asterisk means it's a user option
;; \[command] is replaced with keybinding or M-x command
;; in later Emacs version use the customize system with defgroup / defcustom
(defvar insert-time-format "%X"
  ￼"*Format for \\[insert-time] (c.f. format-time-string').")

;; (interactive "*") means abort function if current buffer is read-only

;; save-excursion
;; save-restriction (saves things like narrowing / widen)
;; save-match-data (saves things like result of latest search)

;; &rest parameter

;;;;;;;;;;;;;;;;;;;;;;;
;; Custom extensions ;;
;;;;;;;;;;;;;;;;;;;;;;;
