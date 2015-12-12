(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)

;; making paredit work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)

;; open clojurescript files in clojure mode
(add-to-list 'auto-mode-alist '("\.cljs$" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\.cljc$" . clojure-mode))
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

(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-hook 'inf-clojure-mode-hook #'eldoc-mode)

(add-hook 'clojure-mode-hook
          (lambda ()
            (add-hook 'activate-mark-hook (lambda ()
                                            (hl-sexp-mode -1)
                                            (highlight-symbol-mode -1)) nil 'make-it-local)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (add-hook 'deactivate-mark-hook (lambda ()
                                              (hl-sexp-mode 1)
                                              (highlight-symbol-mode 1)) nil 'make-it-local)))


;; Enable `paredit-mode' in the minibuffer, during `eval-expression'.
(defun conditionally-enable-paredit-mode ()
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

;; make colon part of word (for example :keyword)
;;(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?: "w")))
;;(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
;;(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?- "w")))

;; prettify symbols
(defun clj-pretty-symbols ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("fn" . 402)
          ("=>" . 8658)
          )))

(add-hook 'clojure-mode-hook 'clj-pretty-symbols)
(add-hook 'clojurescript-mode-hook 'clj-pretty-symbols)

;;---------------------------------------------------------

;; Highlight matching parentheses when the point is on them.
;; (show-paren-mode)
;; (setq show-paren-delay 0)

;; highlight surrounding parentheses
;; (add-hook 'prog-mode-hook 'highlight-parentheses-mode)

(require 'symbol-focus)

;;(require 'yesql-ghosts)

;; Indent and highlight more commands
(put-clojure-indent 'match 'defun)

;; Inf-Clojure
(require 'inf-clojure)
(setq inf-clojure-prompt-read-only nil)
(add-hook 'inf-clojure-minor-mode-hook   ;; prevent company-mode from freezing Emacs when the REPL is busy
          (lambda () (setq completion-at-point-functions nil)))
(add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)

;; align-cljlet
(require 'align-cljlet)

;; Figwheel

(defun figwheel-repl ()
  (interactive)
  (run-clojure "lein figwheel"))

(provide 'lisp-settings)

