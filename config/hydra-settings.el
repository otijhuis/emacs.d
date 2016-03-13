;;;;;;;;;;;;;
;; Hydra's ;;
;;;;;;;;;;;;;

;; Hydra - Marking
(defhydra hydra-mark (:exit t
                            :columns 3
                            :idle 1.0)
  "Mark"
  ("d" er/mark-defun "Defun / Function")
  ("f" er/mark-defun "Defun / Function")
  ("F" er/mark-clj-function-literal "Clj anonymous fn")
  ("w" er/mark-word "Word")
  ("W" er/mark-clj-word "CLJ word")
  ("u" er/mark-url "Url")
  ("e" mark-sexp "S-Expression")
  ("E" er/mark-email "Email")
  ("b" hydra-mark-buffer/body "Buffer")
  ("l" mark-line "Line")
  ("p" er/mark-text-paragraph "Paragraph")
  ("r" er/mark-clj-regexp-literal "Clj regexp")
  ("s" er/mark-symbol "Symbol")
  ("S" er/mark-symbol-with-prefix "Prefixed symbol")
  ("q" er/mark-inside-quotes "Inside quotes")
  ("Q" er/mark-outside-quotes "Outside quotes")
  ("(" er/mark-inside-pairs "Inside pairs")
  ("[" er/mark-inside-pairs "Inside pairs")
  ("{" er/mark-inside-pairs "Inside pairs")
  (")" er/mark-outside-pairs "Outside pairs")
  ("]" er/mark-outside-pairs "Outside pairs")
  ("}" er/mark-outside-pairs "Outside pairs")
  ("t" er/mark-inner-tag "Inner tag")
  ("T" er/mark-outer-tag "Outer tag")
  ("c" er/mark-comment "Comment")
  ("a" er/mark-html-attribute "HTML attribute")
  ("." er/expand-region "Expand region" :exit nil)
  ("," er/contract-region "Contract region" :exit nil)
  ("#" er/mark-clj-set-literal "Clj set")
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

(defhydra hydra-copy (:exit t
                            :idle 1.0)
  "Copy"
  ("r" avy-copy-region "Region to point")
  ("l" avy-copy-line "Line to point")
  ("i" copy-inner "Inner")
  ("o" copy-outer "Outer"))

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
  ("S" mc/mark-all-symbols-like-this "Mark all symbols likes this" :exit t)
  ("w" mc/mark-all-words-like-this "Mark all words like this" :exit t)
  ("r" mc/mark-all-in-region "Mark all in region" :exit t)
  ("R" mc/mark-all-in-region-regexp "Mark all in region (regexp)" :exit t)
  ("d" mc/mark-all-like-this-in-defun "Mark all like this in defun" :exit t)
  ("s" mc/mark-all-symbols-like-this-in-defun "Mark all symbols like this in defun" :exit t)
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
  ("s" paredit-splice-sexp "Splice")
  ("bs" cljr-splice-sexp-killing-backward "Splice kill backward")
  ("fs" cljr-splice-sexp-killing-forward "Splice kill forward")
  ("S" paredit-split-sexp "Split")
  ("j" paredit-join-sexps "Join")
  ("J" paredit-join-with-next-list "Join next list")
  ("M-J" paredit-join-with-previous-list "Join prev list")
  ("C" paredit-convolute-sexp "Convolute")
  ("M-c" paredit-copy-as-kill "Copy as kill")
  ("r" paredit-raise-sexp "Raise s-expression")
  ("R" cljr-raise-sexp "Raise s-expression (cljr)")
  ("c" paxedit-copy "Copy explicit expression, implicit expression, or comment")
  ("d" paxedit-delete "Delete expression")
  ("tb" paxedit-transpose-backward "Transpose backward")
  ("tf" paxedit-transpose-forward "Transpose forward")
  ("k" paxedit-kill "Kill explicit expression, implicit expression, or comment")
  (";" paxedit-wrap-comment "Wrap with comment"))

(defhydra hydra-transpose (:columns 3
                                    :idle 1.0)
  "Transpose"
  ("w" transpose-words "Words")
  ("W" ace-swap-window "Windows / Buffers" :color blue)
  ("b" ace-swap-window "Windows / Buffers" :color blue)
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

(defhydra hydra-lisp-eval (:exit t
                                 :columns 2
                                 :idle 1.0)
  "Lisp eval"
  ("r" eval-region "Region")
  ("b" eval-buffer "Buffer")
  ("e" eval-expression "S-expression")
  ("l" eval-last-sexp "Last s-expression")
  ("L" eval-last-sexp-print-value "Last s-expression and print value")
  ("d" eval-defun "Defun / Function")
  ("f" eval-defun "Defun / Function"))

(defhydra hydra-clj-eval (:exit t
                                :columns 2
                                :idle 1.0)
  "Clojure eval"
  ("b" cider-eval-buffer "Buffer")
  ("n" cider-eval-ns-form "Namespace form")
  ("d" cider-eval-defun-at-point "Defun")
  ("r" cider-eval-region "Region")
  ("R" cider-eval-last-sexp-to-repl "Eval to repl")
  ("f" cider-eval-file "File")
  ("l" cider-eval-last-sexp "Last s-expression")
  ("L" cider-eval-last-sexp-and-replace "Last s-expression and replace with result")
  ("e" cider-eval-last-sexp-and-replace "Last s-expression and replace with result"))

(defhydra hydra-clj-cider (:exit t
                                 :columns 2
                                 :idle 1.0)
  "Cider"
  ("c" cider-connect "Connect")
  ("j" cider-jack-in "Jack in")
  ("i" cider-inspect "Inspect")
  ("t" cider-toggle-trace "Toggle trace")
  ("d" cider-doc "Doc")
  ("a" cider-apropos "Apropos")
  ("A" cider-apropos-documentation "Apropos documentation")
  ("E" cider-jump-to-compilation-error "Jump to compilation error")
  ("R" cider-jump-to-resource "Jump to resource"))

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
  ("d" cljr-update-project-dependency "Update project dependency (lein only)")
  ("p" cljr-update-project-dependencies "Update project dependencies (lein only)")
  ("w" cljr-unwind "Unwind a threaded expression"))

(defhydra hydra-ag (:exit t
                          :columns 2
                          :idle 1.0)
  "Ag Search"
  ("c" helm-ag "Current directory")
  ("d" (lambda ()
         (interactive)
         (let ((current-prefix-arg '(4)))
           (call-interactively 'helm-ag))) "Select directory")
  ("D" helm-do-ag "Select directory (interactive)")
  ("f" helm-ag-this-file "Current file")
  ("F" helm-do-ag-this-file "Current file (interactive)")
  ("p" helm-ag-project-root "Project")
  ("b" helm-ag-buffers "Buffers")
  ("B" helm-do-ag-buffers "Buffers (interactive)"))

(provide 'hydra-settings)
