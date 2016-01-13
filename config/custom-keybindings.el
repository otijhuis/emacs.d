(require 'bind-key)

;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

;; Key chords to use:
;; yy jj '; zx ., \] /. ?? ^^ '/ ;. ;, .; /' =- -=
;; jq qg qk qy qz wq xz fq wx qx jx kq vq qj qh hx qp xk
;; sx

(key-seq-define-global ",p" 'projectile-command-map)
(key-seq-define-global ",a" 'hydra-ag/body)
(key-chord-define-global "';" 'counsel-M-x)

(key-seq-define-global ",l" 'ivy-switch-buffer)
(key-seq-define-global ",f" 'counsel-find-file)

(key-seq-define emacs-lisp-mode-map ",e" 'hydra-lisp-eval/body)

(bind-key "M-x" 'counsel-M-x)
(bind-key "C-x C-i" 'idomenu)
(bind-key "C-x C-b" 'ibuffer)

(key-seq-define-global ",u" 'undo-tree-visualize)
(key-seq-define-global "/." 'hydra-mark/body)
(key-seq-define-global "zx" 'hydra-mark/body)
(key-seq-define-global "][" 'hydra-transpose/body)
;;(symbol-function 'hydra-transpose/transpose-sexps-and-exit)
(key-seq-define-global ",m" 'multiple-cursors-hydra/body)
(bind-key "C-c m" 'multiple-cursors-hydra/body)

;;;;;;;;;;;;;;;;;;;;;;;
;; Other keybindings ;;
;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "C-m" 'ivy-alt-done ivy-minibuffer-map)
(bind-key "C-j" 'ivy-done ivy-minibuffer-map)

(bind-key "C-M-y" 'counsel-yank-pop)
(bind-key "C-s" 'swiper)
(bind-key "C-r" 'ivy-resume)

(bind-key "M-;" 'comment-dwim-2)

(bind-key "M-\\" 'shrink-whitespace)

(bind-key "M-k" 'kill-word)
(bind-key "s-w" 'hydra-copy/body)

(bind-key "M-l" #'fix-word-downcase)
(bind-key "M-u" #'fix-word-upcase)
(bind-key "M-c" #'fix-word-capitalize)

;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(bind-key "<f9>" 'quick-switch-buffer)

;; NeoTree
(require 'neotree)
(bind-key "<f8>" 'neotree-toggle)

(bind-key "s-s" 'save-buffer)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

(require 'misc)
(global-set-key (kbd "s-/") 'copy-from-above-command)

;;(bind-key "C-y" #'hydra-yank-pop/yank)
;;(bind-key "M-y" #'hydra-yank-pop/yank-pop)

;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "s-SPC") 'set-rectangular-region-anchor)

;; Keybindings to use: M-o (other-window maybe???)

(bind-key "C-;" 'iedit-mode)

(bind-key "C-S-<mouse-1>" 'mc/add-cursor-on-click)

(bind-key "M-i" 'helm-imenu)

(bind-key "C-M-;" 'indent-new-comment-line)
(bind-key "C-M-j" 'join-line)
(bind-key "M-j" 'ot/join-line)

(bind-key "s-d" 'ot/duplicate-current-line-or-region)

;;;;;;;;;;;;;;
;; Movement ;;
;;;;;;;;;;;;;;

(bind-key "M-f" 'forward-to-word)
(bind-key "M-F" 'forward-symbol)
(bind-key "M-B" (lambda ()
                  (interactive)
                  (forward-symbol -1)))
(bind-key "s-f" 'forward-whitespace-&-newlines)
(bind-key "s-b" (lambda ()
                  (interactive)
                  (forward-whitespace-&-newlines -1)))

;;;;;;;;;
;; Avy ;;
;;;;;;;;;
(bind-key "H-z" 'avy-zap-to-char-dwim)
(bind-key "H-Z" 'avy-zap-up-to-char-dwim)
(key-seq-define-global ";l" 'avy-goto-char)
(key-seq-define-global "zc" 'ot/avy-goto-word-0)
(key-seq-define-global "zv" 'avy-goto-word-or-subword-1)
(key-seq-define-global "z," 'avy-zap-up-to-char)
(key-seq-define-global "z." 'avy-zap-to-char)
(key-seq-define-global ",z" 'ot/avy-zap-up-to-char-save)
(key-seq-define-global ".z" 'ot/avy-zap-to-char-save)
(bind-key "C-'" 'avy-isearch isearch-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smarter move to beginning/end of line ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "C-S-a" 'beginning-of-line+)
(bind-key "C-a" 'ot/back-to-indentation-or-beginning)
(bind-key "C-S-e" 'end-of-line+)
(bind-key "C-e" 'ot/end-of-code-or-line+)

;;;;;;;;;;;;;;;;;;;;;
;; Web development ;;
;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;
;; Tagedit ;;
;;;;;;;;;;;;;

(bind-key "M-]" 'tagedit-forward-slurp-tag sgml-mode-map)
(bind-key "M-[" 'tagedit-forward-barf-tag sgml-mode-map)
(bind-key "M-r" 'tagedit-raise-tag sgml-mode-map)
(bind-key "M-s" 'tagedit-splice-tag sgml-mode-map)
(bind-key "M-J" 'tagedit-join-tags sgml-mode-map)
(bind-key "M-S" 'tagedit-split-tag sgml-mode-map)
(bind-key "M-C" 'tagedit-convolute-tags sgml-mode-map)
(bind-key "C-k" 'tagedit-kill sgml-mode-map)
(bind-key "s-k" 'tagedit-kill-attribute sgml-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;
;; Paredit / Paxedit ;;
;;;;;;;;;;;;;;;;;;;;;;;

(key-seq-define paredit-mode-map "\\]" 'hydra-paredit/body)
(bind-key "s-j" 'paredit-backward-down paredit-mode-map)
(bind-key "s-i" 'paxedit-backward-up paredit-mode-map)
(bind-key "s-l" 'paxedit-backward-end paredit-mode-map)
(bind-key "s-k" 'paredit-forward-down paredit-mode-map)
(bind-key "C-S-k" 'paxedit-kill paredit-mode-map)
(bind-key "M-k" 'paxedit-symbol-kill paredit-mode-map)
(bind-key [H-backspace] 'paredit-forward-delete paredit-mode-map)
(bind-key [H-M-backspace] 'backward-kill-sexp paredit-mode-map)
(bind-key "s-d" 'ot/paredit-duplicate-after-point paredit-mode-map)
(bind-key "M-D" 'ot/paredit-duplicate-closest-sexp paredit-mode-map)
(bind-key [M-backspace] 'ot/paredit-kill-region-or-backward-word paredit-mode-map)
(bind-key "M-]" 'paredit-forward-slurp-sexp paredit-mode-map)
(bind-key "M-[" 'paredit-forward-barf-sexp paredit-mode-map)
(bind-key "M-s-[" 'paredit-backward-slurp-sexp paredit-mode-map)
(bind-key "M-s-]" 'paredit-backward-barf-sexp paredit-mode-map)
(bind-key "M-9" 'paredit-wrap-round paredit-mode-map)
(bind-key "M-0" 'ot/paredit-wrap-round-from-behind paredit-mode-map)
(bind-key ")" 'ot/step-out-forward paredit-mode-map)

(bind-key [H-backspace] 'delete-char)

(bind-key "M-g M-g" 'avy-goto-line)

;; Activate occur easily inside isearch
(bind-key "C-o" (lambda () (interactive)
                  (let ((case-fold-search isearch-case-fold-search))
                    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
          isearch-mode-map)

(bind-key "<M-return>" 'ot/open-line-below)

;; use hippie-expand instead of abbrev
(bind-key "M-/" 'hippie-expand)
(bind-key "C-M-/" 'hippie-expand-lines)

;; Version control
(bind-key "<f10>" 'magit-status)
(bind-key "<f11>" 'diff-hl-mode)

;; iMenu
(bind-key "<f7>" #'imenu-list-minor-mode)

;; yasnippet
(bind-key "<tab>" nil yas-minor-mode-map)
(bind-key "TAB" nil yas-minor-mode-map)
(bind-key "M-o" 'yas-expand yas-minor-mode-map)

;; popup for yasnippet
(bind-key "M-n" 'popup-next popup-menu-keymap)
(bind-key "TAB" 'popup-next popup-menu-keymap)
(bind-key "<tab>" 'popup-next popup-menu-keymap)
(bind-key "<backtab>" 'popup-previous popup-menu-keymap)
(bind-key "M-p" 'popup-previous popup-menu-keymap)

;;;;;;;;;;;;;;;;;;
;; company-mode ;;
;;;;;;;;;;;;;;;;;;

(bind-key "TAB" #'company-indent-or-complete-common)
(bind-key "<s-tab>" #'company-yasnippet)

(with-eval-after-load 'company
  (bind-key "C-j" 'company-select-next company-active-map)
  (bind-key "C-k" 'company-select-previous company-active-map)
  (bind-key "C-/" 'company-search-candidates company-active-map)
  (bind-key "C-M-/" 'company-filter-candidates company-active-map)
  (bind-key "C-d" 'company-show-doc-buffer company-active-map))

;;;;;;;;;;
;; Helm ;;
;;;;;;;;;;

;;(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Move up and down like isearch
(bind-key "C-k" 'helm-previous-line helm-swoop-map)
(bind-key "C-j" 'helm-next-line helm-swoop-map)
(bind-key "C-k" 'helm-previous-line helm-multi-swoop-map)
(bind-key "C-j" 'helm-next-line helm-multi-swoop-map)

;;;;;;;;;;
;; Help ;;
;;;;;;;;;;

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

;; Lisp
(bind-key "TAB" 'completion-at-point read-expression-map)
(bind-key "RET" 'reindent-then-newline-and-indent lisp-mode-shared-map)

;;; esc quits

(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-ns-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-completion-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-must-match-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-isearch-map)

;;;;;;;;;;;;;;;;;;;;;;
;; Windows / Frames ;;
;;;;;;;;;;;;;;;;;;;;;;

(bind-key "C-x o" 'ido-select-window)
(global-set-key (kbd "C-x 3") 'ot/split-window-right-and-move-there-dammit)
(bind-key "<f12>" 'hydra-frame/body)

;; Commands to map
;; reposition-window, paredit-focus-on-defun, paredit-duplicate-after-point, paredit-reindent-defun

;;;;;;;;;;
;; Lisp ;;
;;;;;;;;;;

(bind-key "<C-return>" 'eval-last-sexp lisp-mode-shared-map)

;;;;;;;;;;;;;
;; Clojure ;;
;;;;;;;;;;;;;

;; (bind-key ",ch" 'ot/helm-clojure-headlines clojure-mode-map)
(bind-key "<C-return>" 'ot/cider-eval-defun-or-region clojure-mode-map)
;;(bind-key "<C-return>" 'inf-clojure-eval-defun clojure-mode-map)

(bind-key (kbd "M-.") 'cider-jump-to-var clojure-mode-map)
(bind-key (kbd "M-,") 'cider-jump-back clojure-mode-map)

;;(bind-key "M-." 'ot/find-tag-without-ns clojure-mode-map)

(key-seq-define clojure-mode-map ",e" 'hydra-clj-eval/body)
(key-seq-define clojure-mode-map ",c" 'hydra-clj-cider/body)

;;(define-key clojure-mode-map "\C-c\C-k" 'ot/reload-current-clj-ns)
;;(define-key clojure-mode-map "\C-cl" 'ot/erase-inf-buffer)
;;(define-key clojure-mode-map "\C-c\C-t" 'clojure-toggle-keyword-string)
;;(define-key inf-clojure-mode-map "\C-cl" 'ot/erase-inf-buffer)

;; (bind-key "s-a" 'hydra-clj-refactor-a/body clojure-mode-map)
;; (key-seq-define clojure-mode-map "]a" 'hydra-clj-refactor-a/body)
;; (key-seq-define clojure-mode-map "]c" 'hydra-clj-refactor-c/body)
;; (key-seq-define clojure-mode-map "]d" 'hydra-clj-refactor-d/body)
;; (key-seq-define clojure-mode-map "]e" 'hydra-clj-refactor-e/body)
;; (key-seq-define clojure-mode-map "]f" 'hydra-clj-refactor-f/body)
;; (key-seq-define clojure-mode-map "]h" 'hydra-clj-refactor-h/body)
;; (key-seq-define clojure-mode-map "]i" 'hydra-clj-refactor-i/body)
;; (key-seq-define clojure-mode-map "]m" 'hydra-clj-refactor-m/body)
;; (key-seq-define clojure-mode-map "]p" 'hydra-clj-refactor-p/body)
;; (key-seq-define clojure-mode-map "]r" 'hydra-clj-refactor-r/body)
;; (key-seq-define clojure-mode-map "]s" 'hydra-clj-refactor-s/body)
;; (key-seq-define clojure-mode-map "]t" 'hydra-clj-refactor-t/body)
;; (key-seq-define clojure-mode-map "]u" 'hydra-clj-refactor-u/body)

(provide 'custom-keybindings)
















