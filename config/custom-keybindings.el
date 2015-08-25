(require 'bind-key)

;;;;;;;;;;;;;
;; Hydra's ;;
;;;;;;;;;;;;;

;; Hydra - Marking
(defhydra hydra-mark (:color blue
                             :hint nil
                             :idle 1.0)
  "Mark"
  ("d" er/mark-defun "Defun / Function")
  ("w" er/mark-word "Word")
  ("u" er/mark-url "Url")
  ("e" er/mark-email "Email")
  ("b" mark-whole-buffer "Buffer")
  ("p" er/mark-paragraph "Paragraph")
  ("s" er/mark-symbol "Symbol")
  ("S" er/mark-symbol-with-prefix "Prefixed symbol")
  ("q" er/mark-inside-quotes "Inside quotes")
  ("Q" er/mark-outside-quotes "Outside quotes")
  ("(" er/mark-inside-pairs "Inside pairs")
  (")" er/mark-outside-pairs "Outside pairs")
  ("t" er/mark-inner-tag "Inner tag")
  ("T" er/mark-outer-tag "Outer tag")
  ("c" er/mark-comment "Comment")
  ("a" er/mark-html-attribute "HTML attribute")
  )

;; Hydra - Yank
(defhydra hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev")
  ("l" helm-show-kill-ring "list" :color blue))   ; or browse-kill-ring

(bind-key "C-y" 'hydra-yank-pop/yank)

;; Hydra - Goto line
(defhydra hydra-goto-line (goto-map ""
                                    :pre (linum-mode 1)
                                    :post (linum-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))

;; Hydra - Projectile
(defhydra hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))

;; Hydra - Multiple cursors
(defhydra multiple-cursors-hydra (:hint nil)
  "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil))

(defhydra hydra-paredit (:color blue
                                :idle 1.0)
  "Paredit"
  ("(" paredit-wrap-round "Wrap round")
  ("[" paredit-wrap-square "Wrap square")
  ("]" paredit-wrap-square "Wrap square")
  ("{" paredit-wrap-curly "Wrap curly")
  ("c" paredit-wrap-curly "Wrap curly")
  ("s" paredit-splice-sexp "Splice")
  ("bs" paredit-splice-sexp-killing-backward "Splice kill backward")
  ("fs" paredit-splice-sexp-killing-forward "Splice kill forward")
  ("S" paredit-split-sexp "Split")
  ("j" paredit-join-sexps "Join")
  ("J" paredit-join-with-next-list "Join next list")
  ("M-J" paredit-join-with-previous-list "Join prev list")
  ("C" paredit-convolute-sexp "Convolute")
  ("M-c" paredit-copy-as-kill "Copy as kill")
  ("r" cljr-raise-sexp "Raise"))

;;;;;;;;;;;;;;;;
;; Key chords ;;
;;;;;;;;;;;;;;;;

;; Key chords to use:
;; yy jj '; zx ., \] /. ?? ^^ '/ ;. ;, .; /' =- -=
;; jq qg qk qy qz wq xz fq wx qx jx kq vq qj qh hx qp xk
;; sx

(key-seq-define-global ",x" 'smex)
(key-seq-define-global "x," 'smex)
(key-seq-define-global "';" 'smex)
(key-seq-define-global ",l" 'ido-switch-buffer)
(key-seq-define-global ",f" 'ido-find-file)
(key-seq-define-global ",u" 'undo-tree-visualize)
;;(key-seq-define-global ",p" 'hydra-projectile/body)
;;(key-seq-define-global ",d" 'ot/duplicate-current-line-or-region)
(key-seq-define-global ",w" 'save-buffer)
(key-seq-define-global "/." 'hydra-mark/body)
(key-seq-define-global "\\]" 'hydra-paredit/body)

(key-seq-define-global "z," 'avy-zap-up-to-char)
(key-seq-define-global "z." 'avy-zap-to-char)

;;;;;;;;;;;;;;;;;;;;;;;
;; Other keybindings ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Keybindings to use: M-o (other-window maybe???)

(bind-key "C-;" 'iedit-mode)

(bind-key "C-c m" 'multiple-cursors-hydra/body)

(bind-key "C-S-<mouse-1>" 'mc/add-cursor-on-click)

(bind-key "M-i" 'helm-imenu)

(bind-key "M-z" 'avy-zap-to-char-dwim)
(bind-key "M-Z" 'avy-zap-up-to-char-dwim)
(bind-key "C-z" 'avy-goto-char)

;; Smarter move to beginning/end of line
(bind-key "C-S-a" 'beginning-of-line+)
(bind-key "C-a" 'ot/back-to-indentation-or-beginning)
(bind-key "C-S-e" 'end-of-line+)
(bind-key "C-e" 'ot/end-of-code-or-line+)

(bind-key "M-j" 'ot/join-line)

(bind-key "C-S-d" 'ot/duplicate-current-line-or-region)

;; Paredit
(bind-key [H-backspace] 'paredit-forward-delete paredit-mode-map)
(bind-key "C-S-d" 'ot/paredit-duplicate-after-point paredit-mode-map)

(bind-key [H-backspace] 'delete-char)

(bind-key "M-g M-g" 'avy-goto-line)

;; Activate occur easily inside isearch
(bind-key "C-o" (lambda () (interactive)
                  (let ((case-fold-search isearch-case-fold-search))
                    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
          isearch-mode-map)

(bind-key "<M-return>" 'ot/open-line-below)

;; use company-mode instead of abbrev
(bind-key "M-/" 'company-complete)

;; use swiper instead of isearch
(bind-key "C-s" 'swiper)
(bind-key "C-r" 'swiper)

;; Magit
(bind-key "<f10>" 'magit-status)

;; popup for yasnippet
(bind-key "M-n" 'popup-next popup-menu-keymap)
(bind-key "TAB" 'popup-next popup-menu-keymap)
(bind-key "<tab>" 'popup-next popup-menu-keymap)
(bind-key "<backtab>" 'popup-previous popup-menu-keymap)
(bind-key "M-p" 'popup-previous popup-menu-keymap)
(bind-key "C-'" 'yas-expand yas-minor-mode-map)

;; company-mode
(with-eval-after-load 'company
  (bind-key "C-j" 'company-select-next company-active-map)
  (bind-key "C-k" 'company-select-previous company-active-map)
  (bind-key "C-/" 'company-search-candidates company-active-map)
  (bind-key "C-M-/" 'company-filter-candidates company-active-map)
  (bind-key "C-d" 'company-show-doc-buffer company-active-map))

;; Helm
;;(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Move up and down like isearch
(bind-key "C-k" 'helm-previous-line helm-swoop-map)
(bind-key "C-j" 'helm-next-line helm-swoop-map)
(bind-key "C-k" 'helm-previous-line helm-multi-swoop-map)
(bind-key "C-j" 'helm-next-line helm-multi-swoop-map)

;; From helm-swoop to helm-multi-swoop-all
(bind-key "M-i" 'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)

(bind-key "M-x" 'smex)
(bind-key "C-x C-i" 'idomenu)
(bind-key "C-x C-b" 'ibuffer)

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

;; Lisp
(bind-key "TAB" 'lisp-complete-symbol read-expression-map)
(bind-key "RET" 'reindent-then-newline-and-indent lisp-mode-shared-map)

;;; esc quits

(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-ns-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-completion-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-must-match-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-isearch-map)

(bind-key "C-x o" 'ido-select-window)

;; Commands to map
;; reposition-window, paredit-focus-on-defun, paredit-duplicate-after-point, paredit-reindent-defun

;; Clojure / Cider
;; (evil-define-key 'normal clojure-mode-map ",ch" 'ot/helm-clojure-headlines)
(bind-key "<C-return>" 'ot/cider-eval-defun-or-region clojure-mode-map)
;; (evil-define-key 'normal clojure-mode-map "\\es" 'cider-eval-last-sexp)
;; (evil-define-key 'normal clojure-mode-map "\\en" 'cider-eval-ns-form)
;; (evil-define-key 'normal clojure-mode-map "\\eb" 'cider-load-buffer)
;; (evil-define-key 'normal clojure-mode-map ",cb" 'cider-repl-clear-buffer)
;; (evil-define-key 'normal clojure-mode-map ",cc" 'cider-connect)
;; (evil-define-key 'normal clojure-mode-map ",cj" 'cider-jack-in)
;; (evil-define-key 'normal clojure-mode-map (kbd "M-.") 'cider-jump-to-var)
;; (evil-define-key 'normal clojure-mode-map (kbd "M-,") 'cider-jump-back)
;; (evil-define-key 'normal clojure-mode-map "\\i" 'cider-inspect)
;; (evil-define-key 'normal clojure-mode-map "\\t" 'cider-toggle-trace)
;; (evil-define-key 'normal clojure-mode-map "\\D" 'cider-doc)
;; (evil-define-key 'normal clojure-mode-map "\\a" 'cider-apropos)
;; (evil-define-key 'normal clojure-mode-map "\\A" 'cider-apropos-documentation)
;; (evil-define-key 'normal clojure-mode-map ",je" 'cider-jump-to-compilation-error)
;; (evil-define-key 'normal clojure-mode-map ",jr" 'cider-jump-to-resource)

;; neotree
;;(add-hook 'neotree-mode-hook
;;          (lambda ()
;;            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
;;            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
;;            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
;;            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(provide 'custom-keybindings)
