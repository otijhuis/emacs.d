;; making paredit work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)

;; open clojurescript files in clojure mode
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
;; open boot build tool files in clojure mode
(add-to-list 'auto-mode-alist '("\.boot$" . clojure-mode))

;; ENHANCE lISP MODES
(require 'paxedit)

;; Indentation
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'ot/esk-remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'ot/esk-prog-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
(add-hook 'clojure-mode-hook 'highlight-symbol-mode)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paxedit-mode)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paxedit-mode)
(add-hook 'clojure-mode-hook 'hl-sexp-mode)

;; make colon part of word (for example :keyword)
(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?: "w")))
(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?- "w")))

;; prettify fn in clojure/clojurescript
(add-hook 'clojure-mode-hook 'ot/pretty-fn)
(add-hook 'clojurescript-mode-hook 'ot/pretty-fn)

;;---------------------------------------------------------

;; Highlight matching parentheses when the point is on them.
;; (show-paren-mode)
;; (setq show-paren-delay 0)

;; highlight surrounding parentheses
;; (add-hook 'prog-mode-hook 'highlight-parentheses-mode)

;;; CIDER CONFIG
(require 'cider)

;; Show what was eval'd
;;(require 'cider-eval-sexp-fu)

(defun hide-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'cider-repl-mode-hook 'hide-eol)
(add-hook 'cider-popup-buffer-mode-hook 'hide-eol)

(setq nrepl-hide-special-buffers t)
(setq cider-prompt-save-file-on-load nil)

(setq cider-repl-print-length 100) ; Limit the number of items of each collection the printer will print to 100

(eval-after-load "cider"
  '(progn
     (setq cider-repl-pop-to-buffer-on-connect nil) ; Prevent the auto-display of the REPL buffer in a separate window after connection is established
     (setq cider-repl-use-clojure-font-lock t)
     (setq cider-show-error-buffer nil)
     (setq cider-jump-to-compilation-error nil)
     (setq cider-auto-jump-to-error nil)
     (add-to-list 'same-window-buffer-names "*cider*") ;Make C-c C-z switch to the *nrepl* buffer in the current window
     ))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode) ; Enable eldoc in clojure buffers
(add-hook 'cider-repl-mode-hook #'subword-mode) ;Enabling CamelCase support for editing commands(like forward-word, backward-word, etc) in nREPL
(add-hook 'cider-repl-mode-hook #'paredit-mode) ;Enable paredit in nRepl buffer
;;(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode) ; rainbow delimiters
(add-hook 'cider-repl-mode-hook '(lambda () (linum-mode 0)))

;; Clojure mode
(add-hook 'clojure-mode-hook 'cider-mode)

;; clj-refactor
(require 'clj-refactor)

(dolist (mapping '(("reagent" . "reagent.core")
                   ("xml" . "clojure.xml")))
  (add-to-list 'cljr-magic-require-namespaces mapping t))

(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-=")))

;;; RAINBOW DELIMITERS
;; (require 'rainbow-delimiters)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; all programming modes
;;(global-rainbow-delimiters-mode) ; globally

(provide 'lisp-settings)
