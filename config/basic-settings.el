;; go to beginning of match after search, not the end (not needed with evil)
;;(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

;; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

;; Remove all backup files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; allow undo/redo of window layout
(with-eval-after-load "winner"
  (winner-mode 1))

;; Whitespace-style
;; (setq whitespace-style '(trailing lines space-before-tab
;;                                   indentation space-after-tab
;;                                   empty)
;;       whitespace-line-column 100)

;; Remove useless whitespace before saving a file (only if the file was clean)
;;(global-whitespace-cleanup-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; don't show trailing whitespace, is already fixed on save
(setq-default show-trailing-whitespace nil)

;; General programming hooks
(add-hook 'prog-mode-hook 'ot/esk-add-watchwords)

;; Prettify symbols
(global-prettify-symbols-mode 1)

;; Turn off auto-fill
(auto-fill-mode -1)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(remove-hook 'org-mode-hook 'turn-on-auto-fill)

;; default to utf8
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; re-builder
(setq reb-re-syntax 'string)

;; Turn on column numbering in modeline
(setq column-number-mode t)

;; Default to org-mode for txt files as well
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; smex
(with-eval-after-load "smex"
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (setq smex-prompt-string "Commands: ")
  (smex-initialize))

;; Truncate lines instead of wrapping
(set-default 'truncate-lines t)

;; Smooth scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq scroll-margin 2)

;; Highlight selected/marked text
(transient-mark-mode t)

;; Delete selected/marked text when you start typing (makes it work like other editors)
(delete-selection-mode t)

;; Go back to the cursor location where you were the last time you opened the file
(require 'saveplace)
(setq-default
 save-place-file (concat user-emacs-directory "places")
 save-place t
 save-place-forget-unreadable-files nil)

;; minibuffer history
(with-eval-after-load "savehist"
  (setq savehist-file (concat user-emacs-directory "savehist")
        enable-recursive-minibuffers t  ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(kill-ring
                                        mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60))

(savehist-mode +1)

;; bookmarks
(setq bookmark-default-file (concat user-emacs-directory "bookmarks")
      ;; save after every change
      bookmark-save-flag 1
      url-configuration-directory (concat user-emacs-directory "url")
      eshell-directory-name (concat user-emacs-directory "eshell" )
      tramp-persistency-file-name (concat user-emacs-directory "tramp"))

(mouse-wheel-mode t)
(blink-cursor-mode -1) ; no blinking cursor

(setq
 inhibit-startup-message   t   ; Don't want any startup message
 echo-keystrokes 0.1
 use-dialog-box nil           ; use no dialog boxes, just use the echo area / mini-buffer
 gc-cons-threshold 20000000
 redisplay-dont-pause t
 ns-pop-up-frames nil         ; don't open a new frame when using Open with... for instance
 search-highlight           t ; Highlight search object
 query-replace-highlight    t ; Highlight query object
 mouse-sel-retain-highlight t ; Keep mouse high-lightening
 read-file-name-completion-ignore-case t
 x-select-enable-clipboard t
 x-select-enable-primary t
 next-line-add-newlines t ; When at end of file moving to the next line adds a new line automatically
 apropos-do-all t
 apropos-sort-by-scores t
 scroll-error-top-bottom t ; move to farthest point when not able to move up or down enough lines
 read-buffer-completion-ignore-case t
 completion-auto-help 'lazy
 isearch-resume-in-command-history t
 kill-read-only-ok t
 isearch-allow-scroll t
 visible-bell nil
 color-theme-is-global t
 sentence-end-double-space nil
 shift-select-mode nil
 mouse-yank-at-point t
 uniquify-buffer-name-style 'post-forward
 uniquify-ignore-buffers-re "^\\*"
 uniquify-strip-common-suffix t
 ediff-diff-options "-w"
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally
 diff-switches "-u"
 frame-title-format '("Emacs       %b %+%+ %f"))

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

(setq browse-url-browser-function 'browse-url-default-browser)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; turn off the toolbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ; turn off the menubar
(size-indication-mode 1) ; show the size of the buffer

;; Easily navigate sillycased words
;;(global-subword-mode 1)
(global-superword-mode 1)

(set-default 'indicate-empty-lines nil) ; don't indicate empty lines

(fset 'yes-or-no-p 'y-or-n-p)

;; seems pointless to warn. There's always undo.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(put 'set-goal-column 'disabled nil) ; handy for moving down a column (always goes to the same position when set)

;; Recent files
(with-eval-after-load "recentf"
  (recentf-mode 1)
  (setq recentf-max-saved-items 150)
  (setq recentf-max-menu-items 100)
  (add-to-list 'recentf-exclude "/elpa"))

;; Mac settings, switch meta and command
(setq-default mac-function-modifier 'hyper)
(setq-default mac-command-modifier 'meta)
(setq-default mac-option-modifier 'super)
;; Don't pass CMD to system so I can use CMD-h for instance
(setq mac-pass-command-to-system nil)

;; Allow replacement of selected region or deletion of selected region by typing or using DEL
(delete-selection-mode 1)

;; Always allow narrowing, don't ask questions
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(set-default 'indent-tabs-mode nil) ; use spaces for indenting, not tabs

(set-default 'imenu-auto-rescan t) ; automatically rescan for changes for imenu

(defalias 'auto-tail-revert-mode 'tail-mode)

(random t) ;; Seed the random-number generator

;; Auto refresh buffers when file is changed externally
(global-auto-revert-mode t)

;; Also auto refresh dired (the directory editor), but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Disable highlighting the current line
(global-hl-line-mode 0)

;; automatically reload changed TAGS file
(setq tags-revert-without-query 1)

;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; draw underline lower
(setq x-underline-at-descent-line t)

;; scratch buffer empty
(setq initial-scratch-message nil)

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Text
(setq longlines-show-hard-newlines t)

;; use only spaces and no tabs
(setq-default indent-tabs-mode nil
  default-tab-width 2)

(setq tab-always-indent 'complete)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

(setq set-mark-command-repeat-pop t)
;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
  (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around
      #'modi/multi-pop-to-mark)

;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
  "if RUN-TOGETHER is true, spell check the CamelCase words"
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if RUN-TOGETHER
    (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
     ((string-match "hunspell$" ispell-program-name)
      (setq args nil)))
    args
    ))

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  ;; just reset dictionary to the safe one "en_US" for hunspell.
  ;; if we need use different dictionary, we specify it in command line arguments
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
  '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
 (t (setq ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
(setq ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

;; abbrev
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; symbol overlays
(require 'symbol-overlay)
(symbol-overlay-mode +1)

(provide 'basic-settings)
