;; Paredit
(with-eval-after-load "paredit"
  ;; making paredit work with delete-selection-mode
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-open-round 'delete-selection t)
  (put 'paredit-open-square 'delete-selection t)
  (put 'paredit-doublequote 'delete-selection t)
  (put 'paredit-newline 'delete-selection t)
  )

;; prettify symbols
(defun clj-pretty-symbols ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("fn" . 402)
          ("=>" . 8658)
          )))

(with-eval-after-load "clojure-mode"
  ;; open clojurescript files in clojure mode
  (add-to-list 'auto-mode-alist '("\.cljs$" . clojurescript-mode))
  (add-to-list 'auto-mode-alist '("\.cljc$" . clojure-mode))
  ;; open boot build tool files in clojure mode
  (add-to-list 'auto-mode-alist '("\.boot$" . clojure-mode))

  (add-hook 'clojure-mode-hook 'symbol-overlay-mode)

  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paxedit-mode)
  ;; (add-hook 'clojure-mode-hook 'hl-sexp-mode)

  (add-hook 'clojure-mode-hook
            (lambda ()
              (add-hook 'activate-mark-hook (lambda ()
                                              (hl-sexp-mode -1)
                                              (symbol-overlay-mode -1)) nil 'make-it-local)))

  (add-hook 'clojure-mode-hook
            (lambda ()
              (add-hook 'deactivate-mark-hook (lambda ()
                                                (hl-sexp-mode 1)
                                                (symbol-overlay-mode 1)) nil 'make-it-local)))

  (add-hook 'clojure-mode-hook 'clj-pretty-symbols)
  (add-hook 'clojurescript-mode-hook 'clj-pretty-symbols)

  (put-clojure-indent 'match 'defun)
  (setq clojure-align-forms-automatically t)
  )

;; ENHANCE lISP MODES

;; Indentation
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'ot/esk-remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'ot/esk-prog-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook 'symbol-overlay-mode)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paxedit-mode)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

;; Enable `paredit-mode' in the minibuffer, during `eval-expression'.
(defun conditionally-enable-paredit-mode ()
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

;; make colon part of word (for example :keyword)
;;(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?: "w")))
;;(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
;;(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?- "w")))

;;---------------------------------------------------------

;;; CIDER CONFIG

;; Show what was eval'd
;;(require 'cider-eval-sexp-fu)

(defun hide-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(with-eval-after-load "cider"
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-pop-to-buffer-on-connect nil) ; Prevent the auto-display of the REPL buffer in a separate window after connection is established
  ;;(setq cider-repl-use-clojure-font-lock t)
  (setq cider-show-error-buffer 'except-in-repl)
  (setq cider-jump-to-compilation-error nil)
  (setq cider-auto-jump-to-error nil)
  (setq cider-prefer-local-resources t)
  (add-to-list 'same-window-buffer-names "*cider*") ;Make C-c C-z switch to the *nrepl* buffer in the current window

  (add-hook 'cider-repl-mode-hook 'hide-eol)
  (add-hook 'cider-popup-buffer-mode-hook 'hide-eol)

  (setq nrepl-hide-special-buffers nil)
  (setq cider-prompt-save-file-on-load 'always-save)

  ;; Enable error buffer popping also in the REPL:
  (setq cider-repl-popup-stacktraces t)

  ;; auto-select the error buffer when it's displayed
  (setq cider-auto-select-error-buffer nil)

  ;; Pretty print results in repl
  (setq cider-repl-use-pretty-printing t)

  ;; Don't prompt for project when connecting
  (setq cider-prompt-for-project-on-connect nil)

  ;; Don't prompt for symbols
  (setq cider-prompt-for-symbol nil)

  (setq cider-font-lock-dynamically '(macro core function var))

  (setq cider-repl-print-length 100) ; Limit the number of items of each collection the printer will print to 100

  ;; Clojure mode
  (add-hook 'clojure-mode-hook 'cider-mode)

  ;; use syntax highlighting for evaluated overlay
  (setq cider-overlays-use-font-lock t)

  (add-hook 'cider-mode-hook 'turn-on-eldoc-mode) ; Enable eldoc in clojure buffers
  (add-hook 'cider-repl-mode-hook #'subword-mode) ;Enabling CamelCase support for editing commands(like forward-word, backward-word, etc) in nREPL
  (add-hook 'cider-repl-mode-hook #'paredit-mode) ;Enable paredit in nRepl buffer
  ;;(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode) ; rainbow delimiters

  (defun figwheel-repl ()
    (interactive)
    (run-clojure "lein figwheel"))

  ;; Auto-save when loading buffers or running tests
  (defadvice clojure-test-run-tests (before save-first activate)
    (save-buffer))

  (defadvice nrepl-load-current-buffer (before save-first activate)
    (save-buffer))

  )


;; clj-refactor
(with-eval-after-load "clj-refactor"
  (setq cljr-favor-prefix-notation nil)
  (setq cljr-favor-private-functions nil)
  ;; (dolist (mapping '(("reagent" . "reagent.core")
  ;;                    ("xml" . "clojure.xml")))
  ;;   (add-to-list 'cljr-magic-require-namespaces mapping t))
  (setq cljr-magic-require-namespaces
        '(("io"   . "clojure.java.io")
          ("set"  . "clojure.set")
          ("str"  . "clojure.string")
          ("walk" . "clojure.walk")
          ("zip"  . "clojure.zip")
          ("time" . "clj-time.core")
          ("log"  . "taoensso.timbre")
          ("reagent" . "reagent.core")
          ("xml" . "clojure.xml")
          ("json" . "cheshire.core")))

  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 ;;(core-async-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-=")))

  (add-to-list 'cljr-project-clean-functions 'cleanup-buffer)
  (setq cljr-project-clean-sorts-project-dependencies t)
  (setq cljr-project-clean-prompt t)
  (setq cljr-auto-sort-ns t)

  ;; Make q quit out of find-usages to previous window config

  (defadvice cljr-find-usages (before setup-grep activate)
    (window-configuration-to-register ?$))
  )

;; First time loading takes long so require it in advance but don't slow down emacs startup
;;(run-with-idle-timer 5 nil (lambda ()(require 'clj-refactor)))

;; Highlight matching parentheses when the point is on them.
;; (show-paren-mode)
;; (setq show-paren-delay 0)

;; highlight surrounding parentheses
;; (add-hook 'prog-mode-hook 'highlight-parentheses-mode)

;;(require 'symbol-focus)

;; Indent and highlight more commands

(provide 'lisp-settings)
