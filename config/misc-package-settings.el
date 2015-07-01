;; Multiple Cursors
(require 'multiple-cursors)
(setq mc/unsupported-minor-modes '(company-mode auto-complete-mode flyspell-mode jedi-mode))

;; Volatile Highlights
(require 'volatile-highlights)

(volatile-highlights-mode t)

;; Indent Guide
(require 'indent-guide)

(indent-guide-global-mode)

(setq indent-guide-recursive t)
(setq indent-guide-delay 0.1)

;; Key chords
(require 'key-chord)

(setq key-chord-two-keys-delay 0.1)

(key-chord-mode +1)

;; Redefine key-chord function so it only works when keypresses are in order. For instance ',x' works, 'x,' doesn't
;; Normally both work because it was meant for simultaneous key presses.
(defun key-chord-define (keymap keys command)
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (if (eq key1 key2)
        (define-key keymap (vector 'key-chord key1 key2) command)
      (define-key keymap (vector 'key-chord key1 key2) command))))

;; If no region is selected then work on current line
(require 'whole-line-or-region)

(whole-line-or-region-mode 1)

;; Undo tree
;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; Guide-key
(require 'guide-key)
(setq guide-key/idle-delay 1.0)
;; (setq guide-key/recursive-key-sequence-flag t)
;; (guide-key/key-chord-hack-on) ; to make it work with guide-key
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c p" "C-x n"))
(setq guide-key/text-scale-amount -1)
(guide-key-mode 1)  ; Enable guide-key-mode

(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

;; Guide-key tip
(require 'guide-key-tip)
(setq guide-key-tip/enabled t)

;; Highlight symbol
(setq highlight-symbol-idle-delay 0.5)

(provide 'misc-package-settings)
