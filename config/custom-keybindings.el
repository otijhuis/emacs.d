(require 'bind-key)

;;;;;;;;;;;;;
;; Hydra's ;;
;;;;;;;;;;;;;

;; Hydra - Marking
(defhydra hydra-mark (:color blue
                             :columns 3
                             :idle 1.0)
  "Mark"
  ("d" er/mark-defun "Defun / Function")
  ("w" er/mark-word "Word")
  ("u" er/mark-url "Url")
  ("e" mark-sexp "S-Expression")
  ("E" er/mark-email "Email")
  ("b" hydra-mark-buffer/body "Buffer")
  ("l" mark-line "Line")
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

(defhydra hydra-mark-buffer (:exit t
                                   :idle 1.0)
  "Mark buffer"
  ("w" mark-whole-buffer "Whole buffer")
  ("a" mark-buffer-after-point "Buffer after point")
  ("b" mark-buffer-before-point "Buffer before point"))

;; Hydra - Yank
(defhydra hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev")
  ("l" helm-show-kill-ring "list" :exit t))   ; or browse-kill-ring

;; Hydra - Goto line
(defhydra hydra-goto-line (goto-map ""
                                    :pre (linum-mode 1)
                                    :post (linum-mode -1))
  "goto-line"
  ("g" goto-line "go")
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))

;; Hydra - Multiple cursors
(defhydra multiple-cursors-hydra (:columns 3
                                           :idle 1.0)
  "Multiple cursors"
  ("l" mc/edit-lines "Edit lines in region" :exit t)
  ("b" mc/edit-beginnings-of-lines "Edit beginnings of lines in region" :exit t)
  ("e" mc/edit-ends-of-lines "Edit ends of lines in region" :exit t)
  ("a" mc/mark-all-dwim "Mark all dwim" :exit t)
  ("s" mc/mark-all-symbols-like-this "Mark all symbols likes this" :exit t)
  ("w" mc/mark-all-words-like-this "Mark all words like this" :exit t)
  ("r" mc/mark-all-in-region "Mark all in region" :exit t)
  ("R" mc/mark-all-in-region-regexp "Mark all in region (regexp)" :exit t)
  ("d" mc/mark-all-like-this-in-defun "Mark all like this in defun" :exit t)
  ("S" mc/mark-all-symbols-like-this-in-defun "Mark all symbols like this in defun" :exit t)
  ("W" mc/mark-all-words-like-this-in-defun "Mark all words like this in defun" :exit t)
  ("i" mc/insert-numbers "Insert numbers" :exit t)
  ("n" mc/mark-next-like-this "Mark next like this")
  ("N" mc/skip-to-next-like-this "Skip to next like this")
  ("M-n" mc/unmark-next-like-this "Unmark next like this")
  ("p" mc/mark-previous-like-this "Mark previous like this")
  ("P" mc/skip-to-previous-like-this "Skip to previous like this")
  ("M-p" mc/unmark-previous-like-this "Unmark previous like this")
  ("q" nil "Quit" :exit t))

(defhydra hydra-paredit (:color blue
                                :columns 3
                                :idle 1.0)
  "Paredit"
  ("(" paredit-wrap-round "Wrap round")
  ("[" paredit-wrap-square "Wrap square")
  ("]" paredit-wrap-square "Wrap square")
  ("{" paredit-wrap-curly "Wrap curly")
  ("c" paredit-wrap-curly "Wrap curly")
  ("s" paredit-splice-sexp "Splice")
  ("bs" cljr-splice-sexp-killing-backward "Splice kill backward")
  ("fs" cljr-splice-sexp-killing-forward "Splice kill forward")
  ("S" paredit-split-sexp "Split")
  ("j" paredit-join-sexps "Join")
  ("J" paredit-join-with-next-list "Join next list")
  ("M-J" paredit-join-with-previous-list "Join prev list")
  ("C" paredit-convolute-sexp "Convolute")
  ("M-c" paredit-copy-as-kill "Copy as kill")
  ("r" cljr-raise-sexp "Raise"))

(defhydra hydra-transpose (:columns 3
                                    :idle 1.0)
  "Transpose"
  ("w" transpose-words "Words")
  ("l" transpose-lines "Lines")
  ("e" transpose-sexps "S-expressions")
  ("s" transpose-sexps "S-expressions")
  ("p" transpose-paragraphs "Paragraphs")
  ("q" nil "cancel" :exit t))

(defhydra hydra-frame (:exit t
                             :idle 1.0)
  "Frame"
  ("f" toggle-frame-fullscreen "Toggle fullscreen")
  ("m" toggle-frame-maximized "Toggle maximize")
  ("d" delete-frame "Delete")
  ("n" new-frame "New"))

(defhydra hydra-eval (:exit t
                            :idle 1.0)
  "Eval"
  ("r" eval-region "Region")
  ("b" eval-buffer "Buffer")
  ("e" eval-sexp "S-expression")
  ("d" eval-defun "Defun / Function"))

(defhydra hydra-clj-refactor-a (:exit t
                                      :columns 2
                                      :idle 1.0)
  "Clojure Refactor (a)"
  ("i" cljr-add-import-to-ns "Add import")
  ("r" cljr-add-require-to-ns "Add require")
  ("u" cljr-add-use-to-ns "Add use")
  ("m" cljr-add-missing-libspec "Add missing libspec")
  ("p" cljr-add-project-dependency "Add project dependency")
  ("d" cljr-add-declaration "Add declaration for current top-level form")
  ("s" cljr-add-stubs "Add stubs for interface at point"))

(defhydra hydra-clj-refactor-c (:exit t
                                      :columns 2
                                      :idle 1.0)
  "Clojure Refactor (c)"
  ("c" cljr-cycle-coll "Cycle surrounding collection type" :color red)
  ("i" cljr-cycle-if "Cycle between if and if-not")
  ("p" cljr-cycle-privacy "Cycle privacy of defn or def")
  ("n" cljr-clean-ns "Clean namespace form")
  ("t" cljr-cycle-thread "Cycle threading between -> and ->>. Also applies to versions like cond->"))

(defhydra hydra-clj-refactor-d (:exit t
                                      :columns 1
                                      :idle 1.0)
  "Clojure Refactor (d)"
  ("k" cljr-destructure-keys "Destructure keys"))

(defhydra hydra-clj-refactor-e (:exit t
                                      :columns 2
                                      :idle 1.0)
  "Clojure Refactor (e)"
  ("c" cljr-extract-constant "Extract constant")
  ("d" cljr-extract-def "Extract def")
  ("f" cljr-extract-function "Extract function")
  ("l" cljr-expand-let "Expand let"))

(defhydra hydra-clj-refactor-f (:exit t
                                      :columns 1
                                      :idle 1.0)
  "Clojure Refactor (f)"
  ("e" cljr-create-fn-from-example "Create fn from example stub")
  ("u" cljr-find-usages "Find usages"))

(defhydra hydra-clj-refactor-h (:exit t
                                      :columns 1
                                      :idle 1.0)
  "Clojure Refactor (h)"
  ("d" cljr-hotload-dependency "Hotload dependency"))

(defhydra hydra-clj-refactor-i (:exit t
                                      :columns 1
                                      :idle 1.0)
  "Clojure Refactor (i)"
  ("l" cljr-introduce-let "Introduce let")
  ("s" cljr-inline-symbol "Inline symbol"))

(defhydra hydra-clj-refactor-m (:exit t
                                      :columns 1
                                      :idle 1.0)
  "Clojure Refactor (m)"
  ("f" cljr-move-form "Move form(s) to other ns, :refer functions")
  ("l" cljr-move-to-let "Move to let"))

(defhydra hydra-clj-refactor-p (:exit t
                                      :columns 1
                                      :idle 1.0)
  "Clojure Refactor (p)"
  ("c" cljr-project-clean-functions "Run project clean functions on project")
  ("f" cljr-promote-function "Promote function literal to fn, or fn to defn"))

(defhydra hydra-clj-refactor-r (:exit t
                                      :columns 2
                                      :idle 1.0)
  "Clojure Refactor (r)"
  ("d" cljr-remove-debug-fns "Remove debug function invocations")
  ("D" cljr-reify-to-defrecord "Reify to defrecord")
  ("f" cljr-rename-file-or-dir "Rename file or dir updating any affected files")
  ("l" cljr-remove-let "Remove let, inline all variables and remove the let form")
  ("r" cljr-remove-unused-requires "Remove unused requires")
  ("s" cljr-rename-symbol "Rename symbol")
  ("u" cljr-replace-use "Replace use statements with equivalent require statements"))

(defhydra hydra-clj-refactor-s (:exit t
                                      :columns 2
                                      :idle 1.0)
  "Clojure Refactor (s)"
  ("c" cljr-show-changelog "Show the changelog, to learn about recent changes")
  ("n" cljr-sort-ns "Sort the ns form. :use, :require and :import clauses are sorted")
  ("p" cljr-sort-project-dependencies "Sort project dependencies found in project.clj")
  ("r" cljr-stop-referring "Stop referring (removes :refer [...] from current require, fix references)"))

(defhydra hydra-clj-refactor-t (:exit t
                                      :columns 2
                                      :idle 1.0)
  "Clojure Refactor (t)"
  ("d" cljr-toggle-debug-mode "Toggle debug mode")
  ("f" cljr-thread-first-all "Wrap in thread-first (->) and fully thread")
  ("h" cljr-thread "Thread another expression")
  ("l" cljr-thread-last-all "Wrap in thread-last (->>) and fully thread"))

(defhydra hydra-clj-refactor-u (:exit t
                                      :columns 2
                                      :idle 1.0)
  "Clojure Refactor (u)"
  ("a" cljr-unwind-all "Fully unwind a threaded expression")
  ("p" cljr-update-project-dependencies "Update project dependencies (lein only)")
  ("w" cljr-unwind "Unwind a threaded expression"))

;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

;; Key chords to use:
;; yy jj '; zx ., \] /. ?? ^^ '/ ;. ;, .; /' =- -=
;; jq qg qk qy qz wq xz fq wx qx jx kq vq qj qh hx qp xk
;; sx

(bind-key "s-a" 'hydra-clj-refactor-a/body clojure-mode-map)
(key-seq-define clojure-mode-map ".a" 'hydra-clj-refactor-a/body)
(key-seq-define clojure-mode-map ".c" 'hydra-clj-refactor-c/body)
(key-seq-define clojure-mode-map ".d" 'hydra-clj-refactor-d/body)
(key-seq-define clojure-mode-map ".e" 'hydra-clj-refactor-e/body)
(key-seq-define clojure-mode-map ".f" 'hydra-clj-refactor-f/body)
(key-seq-define clojure-mode-map ".h" 'hydra-clj-refactor-h/body)
(key-seq-define clojure-mode-map ".i" 'hydra-clj-refactor-i/body)
(key-seq-define clojure-mode-map ".m" 'hydra-clj-refactor-m/body)
(key-seq-define clojure-mode-map ".p" 'hydra-clj-refactor-p/body)
(key-seq-define clojure-mode-map ".r" 'hydra-clj-refactor-r/body)
(key-seq-define clojure-mode-map ".s" 'hydra-clj-refactor-s/body)
(key-seq-define clojure-mode-map ".t" 'hydra-clj-refactor-t/body)
(key-seq-define clojure-mode-map ".u" 'hydra-clj-refactor-u/body)

(key-seq-define-global ",p" 'projectile-command-map)
;;(key-seq-define-global ",x" 'smex)
(key-seq-define-global "';" 'smex)

(key-seq-define-global ",l" 'ido-switch-buffer)
(key-seq-define-global ",f" 'ido-find-file)

(bind-key "M-x" 'smex)
(bind-key "M-X" 'smex-major-mode-commands)
(bind-key "C-x C-i" 'idomenu)
(bind-key "C-x C-b" 'ibuffer)

(key-seq-define-global ",u" 'undo-tree-visualize)
(key-seq-define-global "/." 'hydra-mark/body)
(key-seq-define-global "zx" 'hydra-mark/body)
(key-seq-define-global "][" 'hydra-transpose/body)

;;(symbol-function 'hydra-transpose/transpose-sexps-and-exit)

;;;;;;;;;;;;;;;;;;;;;;;
;; Other keybindings ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(bind-key "<f9>" 'quick-switch-buffer)

(bind-key "s-s" 'save-buffer)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

(require 'misc)
(global-set-key (kbd "s-/") 'copy-from-above-command)

(bind-key "C-y" #'hydra-yank-pop/yank)
(bind-key "M-y" #'hydra-yank-pop/yank-pop)

;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "s-SPC") 'set-rectangular-region-anchor)

;; Keybindings to use: M-o (other-window maybe???)

(bind-key "C-;" 'iedit-mode)

(bind-key "C-c m" 'multiple-cursors-hydra/body)

(bind-key "C-S-<mouse-1>" 'mc/add-cursor-on-click)

(bind-key "M-i" 'helm-imenu)

;;;;;;;;;
;; Avy ;;
;;;;;;;;;
(bind-key "M-z" 'avy-zap-to-char-dwim)
(bind-key "M-Z" 'avy-zap-up-to-char-dwim)
(key-seq-define-global ";l" 'avy-goto-char)
(key-seq-define-global "zc" 'avy-goto-word-0)
(key-seq-define-global "z," 'avy-zap-up-to-char)
(key-seq-define-global "z." 'avy-zap-to-char)
(bind-key "C-'" 'avy-isearch isearch-mode-map)

;; Smarter move to beginning/end of line
(bind-key "C-S-a" 'beginning-of-line+)
(bind-key "C-a" 'ot/back-to-indentation-or-beginning)
(bind-key "C-S-e" 'end-of-line+)
(bind-key "C-e" 'ot/end-of-code-or-line+)

(bind-key "M-j" 'ot/join-line)

(bind-key "s-d" 'ot/duplicate-current-line-or-region)

;;;;;;;;;;;;;
;; Paredit ;;
;;;;;;;;;;;;;
(key-seq-define paredit-mode-map "\\]" 'hydra-paredit/body)
(bind-key "s-j" 'paredit-backward-down paredit-mode-map)
(bind-key "s-i" 'paredit-backward-up paredit-mode-map)
(bind-key "s-l" 'paredit-forward-up paredit-mode-map)
(bind-key "s-k" 'paredit-forward-down paredit-mode-map)
(bind-key [H-backspace] 'paredit-forward-delete paredit-mode-map)
(bind-key "s-d" 'ot/paredit-duplicate-after-point paredit-mode-map)
(bind-key "s-D" 'ot/paredit-duplicate-closest-sexp paredit-mode-map)
(bind-key [M-backspace] 'ot/paredit-kill-region-or-backward-word paredit-mode-map)
(bind-key "s-." 'paredit-forward-slurp-sexp paredit-mode-map)
(bind-key "s-," 'paredit-forward-barf-sexp paredit-mode-map)
(bind-key "M-s-," 'paredit-backward-slurp-sexp paredit-mode-map)
(bind-key "M-s-." 'paredit-backward-barf-sexp paredit-mode-map)

(bind-key [H-backspace] 'delete-char)

(bind-key "M-g M-g" 'avy-goto-line)

;; Activate occur easily inside isearch
(bind-key "C-o" (lambda () (interactive)
                  (let ((case-fold-search isearch-case-fold-search))
                    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
          isearch-mode-map)

(bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map)
(bind-key "M-I" 'helm-multi-swoop-all-from-isearch isearch-mode-map)
(bind-key "M-m" 'helm-multi-swoop-current-mode-from-helm-swoop helm-swoop-map)
(bind-key "M-i" 'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)

(bind-key "<M-return>" 'ot/open-line-below)

;; use hippie-expand instead of abbrev
(bind-key "M-/" 'hippie-expand)

;; Version control
(bind-key "<f10>" 'magit-status)
(bind-key "<f11>" 'diff-hl-mode)

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


;;;;;;;;;;;;;;;;;;;;;
;; Clojure / Cider ;;
;;;;;;;;;;;;;;;;;;;;;

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

(provide 'custom-keybindings)
