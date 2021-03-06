;; -*- lexical-binding: t -*-
(require 'recentf)

(defun ot/ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))

(defun ot/cider-eval-defun-or-region ()
  "Eval defun at point or region when it is active"
  (interactive)
  (if (use-region-p)
      (cider-eval-region)
    (cider-eval-defun-at-point)))

(defun ot/cider-eval-count-defun-at-point ()
  (interactive)
  (cider-interactive-eval
   (format "(count %s)"
           (cider-eval-defun-at-point))))

(defun ot/cider-nth-from-defun-at-point (n)
  (interactive "p")
  (cider-interactive-eval
   (format "(count %s %s)"
           (cider-eval-defun-at-point) n)))

(defun ot/cider-benchmark-defun-at-point ()
  (interactive)
  (cider-interactive-eval
   (format "(require 'criterium.core)
            (criterium.core/quick-benchmark %s)"
           (cider-eval-defun-at-point))))

(defun ot/goto-match-beginning ()
  (when (and isearch-forward isearch-other-end (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

;; Based on Bodil Stokke's version
;; Only if you duplicate something at the start of a line it will also add a newline above
(defun ot/paredit-duplicate-after-point
    (&optional prefix)
  "Duplicates the content of the line that is after the point."
  (interactive "P")
  (if (use-region-p)
      (ot/duplicate-current-line-or-region 1)
    ;; skips to the next sexp
    (while (looking-at " ")
      (forward-char))
    (set-mark-command nil)
    ;; while we find sexps we move forward on the line
    (while (and (bounds-of-thing-at-point 'sexp)
                (<= (point) (car (bounds-of-thing-at-point 'sexp)))
                (not (= (point) (line-end-position))))
      (forward-sexp)
      (while (looking-at " ")
        (forward-char)))
    (kill-ring-save (mark) (point))
    ;; go to the next line and copy the sexprs we encountered
    (paredit-newline)
    (yank)
    (exchange-point-and-mark)
    (when prefix
      (paredit-newline))))

(defun ot/join-line ()
  (interactive)
  (join-line -1))

(defun ot/move-lines-down-from-point ()
  "Insert empty lines above the current line but move the cursor down with the rest of the text."
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (newline-and-indent)))

(defun ot/cider-eval-count-defun-at-point ()
  (interactive)
  (cider-interactive-eval
   (format "(count %s)"
           (cider-eval-defun-at-point))))

(defun ot/cider-nth-from-defun-at-point (n)
  (interactive "p")
  (cider-interactive-eval
   (format "(count %s %s)"
           (cider-eval-defun-at-point) n)))

(defun ot/cider-benchmark-defun-at-point ()
  (interactive)
  (cider-interactive-eval
   (format "(require 'criterium.core)
            (criterium.core/quick-benchmark %s)"
           (cider-eval-defun-at-point))))

(defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(defun ot/point-in-comment ()
  "Determine if the point is inside a comment"
  (interactive)
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
         (not (nth 3 syn)))))

(defun ot/end-of-code-or-line+ (arg)
  "Move to the end of code. If already there, move to the end of line,
  that is after the possible comment. If at the end of line, move
  to the end of code. Comments are recognized in any mode that
  sets syntax-ppss properly."
  (interactive "P")
  (let ((eoc (save-excursion
               (move-end-of-line arg)
               (while (ot/point-in-comment)
                 (backward-char))
               (skip-chars-backward " \t")
               (point))))
    (cond ((= (point) eoc)
           (move-end-of-line arg))
          (t
           (move-end-of-line arg)
           (while (ot/point-in-comment)
             (backward-char))
           (skip-chars-backward " \t")))))

(defun ot/back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun ot/next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun ot/previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

(defun ot/next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun ot/previous-emacs-buffer ()
  "switch to the previous emacs buffer.
emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

;; Navigating Clojure with Helm
(defun ot/helm-clojure-headlines ()
  "Display headlines for the current Clojure file."
  (interactive)
  (helm-mode t)
  (helm :sources '(((name . "Clojure Headlines")
                    (volatile)
                    (headline "^[(]")))))

(defun ot/indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun ot/esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun ot/esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun ot/esk-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

(defun ot/paredit--is-at-start-of-sexp ()
  (and (looking-at "(\\|\\[")
       (not (nth 3 (syntax-ppss))) ;; inside string
       (not (nth 4 (syntax-ppss))))) ;; inside comment

(defun ot/paredit-duplicate-closest-sexp ()
  (interactive)
  ;; skips to start of current sexp
  (while (not (ot/paredit--is-at-start-of-sexp))
    (paredit-backward))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (bounds-of-thing-at-point 'sexp)
              (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (yank)
  (exchange-point-and-mark))

(defun ot/paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(defun ot/paredit-wrap-square-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-square))

(defun ot/paredit-wrap-curly-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-curly))

(defun ot/paredit-kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-kill-word)))

(defun ot/rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun ot/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun ot/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun ot/move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun ot/move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun ot/open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun ot/open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun ot/replace-regexp-in-region (start end)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (let ((regexp (read-string "Regexp: "))
            (to-string (read-string "Replacement: ")))
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match to-string nil nil))))))

(defun ot/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    ;; Don't include the last line if a region exists and the point is at the beginning of the last line.
    (if (and mark-active (= origin (line-beginning-position)))
        (setq end (- origin 1))
      (setq end (line-end-position)))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun ot/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;; Magnars Emacs config
(defun ot/clj-duplicate-top-level-form ()
  (interactive)
  (save-excursion
    (cljr--goto-toplevel)
    (insert (cljr--extract-sexp) "\n")
    (cljr--just-one-blank-line)))

;; Magnars Emacs config
(defun ot/clj-hippie-expand-no-case-fold ()
  (interactive)
  (let ((old-syntax (char-to-string (char-syntax ?/))))
    (modify-syntax-entry ?/ " ")
    (hippie-expand-no-case-fold)
    (modify-syntax-entry ?/ old-syntax)))

(defun ot/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun ot/kill-to-beginning-of-line ()
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))

(defun ot/kill-and-retry-line ()
  "Kill the entire current line and reposition point at indentation"
  (interactive)
  (back-to-indentation)
  (kill-line))

(defun ot/split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun ot/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(require 'avy-zap)

(defun ot/avy-zap-to-char-save ()
  "Zap to a character, but save instead of kill."
  (interactive)
  (save-excursion
    (avy-zap-to-char)
    (yank)))

(defun ot/avy-zap-up-to-char-save ()
  "Zap up to a character, but save instead of kill."
  (interactive)
  (save-excursion
    (avy-zap-up-to-char)
    (yank)))

;; Clojure

(defun ot/reload-current-clj-ns (next-p)
  (interactive "P")
  (let ((ns (clojure-find-ns)))
    (message (format "Loading %s ..." ns))
    (inf-clojure-eval-string (format "(require '%s :reload)" ns))
    (when (not next-p) (inf-clojure-eval-string (format "(in-ns '%s)" ns)))))

(defun ot/find-tag-without-ns (next-p)
  (interactive "P")
  (find-tag (first (last (split-string (symbol-name (symbol-at-point)) "/")))
            next-p))

(defun ot/erase-inf-buffer ()
  (interactive)
  (with-current-buffer (get-buffer "*inf-clojure*")
    (erase-buffer))
  (inf-clojure-eval-string ""))

(defun ot/step-out-forward ()
  "Step forward out of current list or string."
  (interactive)
  (cond
   ;; if inside comment just insert paren
   ((nth 4 (syntax-ppss (point))) (insert ")"))
   ;; if inside string keep moving forward
   ((nth 3 (syntax-ppss (point)))
    (forward-char)
    (while (and (not (eobp)) (nth 3 (syntax-ppss (point))))
      (forward-char)))
   (t (up-list))))

;; From http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun ot/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if you
         ;; don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun ot/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (thing-at-point 'word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word))
      (setq aft (thing-at-point 'word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(defun ot/kill-sexp ()
  (interactive)
  (if (looking-at-p "\\s(")
      (kill-sexp)
    (paxedit-kill)))

(defun ot/parens-move-backward ()
  (interactive)
  (when (search-backward-regexp "\\s(.?" nil 'noerror)
    (goto-char (- (match-end 0) 1))))

(defun ot/parens-move-forward ()
  (interactive)
  (when (looking-at-p "\\s(\\|\\s)\\|\"")
    (forward-char))
  (while (and (not (eobp))
              (or (nth 3 (syntax-ppss (point)))
                  (not (looking-at-p "\\s(\\|\\s)\\|\""))))
    (forward-char)))

(defun ot/paredit-open-line-below ()
  (interactive)
  (let ((ppss (syntax-ppss (point))))
    (if (= 0 (nth 0 ppss))
        (newline-and-indent)
      (progn
        (when (nth 3 ppss)
          (paredit-forward-up))
        (paredit-forward-up)
        (backward-char)
        (newline-and-indent)))))

(defun ot/current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun ot/first-char-closing-pair-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[^\\s\-]+\\s)")))

;; Avy base movement macros/functions

(defmacro ot/avy-here (pt command &rest body)
  "Avy here"
  (declare (indent 1)
           (debug (form body)))
  `(progn
     (save-excursion
       (goto-char ,pt)
       (funcall ,command)
       ,@body)
     (yank)
     (if (looking-at-p "[\\s_\|\\s(]")
         (just-one-space))))

(defun ot/avy-move-here (pt command)
  "Move region from PT to PT after COMMAND to current location."
  (ot/avy-here pt command
               (kill-region pt (point))
               (if (or (ot/current-line-empty-p)
                       (ot/first-char-closing-pair-p))
                   (join-line)
                 (fixup-whitespace))))

(defun ot/avy-copy-here (pt command)
  "Copy region from PT to PT after COMMAND to current location"
  (ot/avy-here pt command
               (copy-region-as-kill pt (point))))

(defvar ot/avy-selected-pt nil)

;; Avy copy actions

(defun ot/avy-action-copy-hydra (pt)
  (setq ot/avy-selected-pt pt)
  (hydra-avy-copy-actions/body))

(defun ot/avy-action-copy-sexp-here (pt)
  "Move sexp at PT to current location"
  (ot/avy-copy-here pt 'forward-sexp))

(defun ot/avy-action-copy-symbol-here (pt)
  "Move symbol at PT to current location"
  (ot/avy-copy-here pt (lambda () (forward-symbol 1))))

(defun ot/avy-action-copy-surrounding-sexp-here (pt)
  "Move surrounding sexp at PT to current location"
  (let ((new-pt (save-excursion
                  (goto-char pt)
                  (paredit-backward-up)
                  (point))))
    (ot/avy-copy-here new-pt 'forward-sexp)))

(defun ot/avy-action-copy-sexp-forward-here (pt)
  "Move from PT to end of sexp to current location"
  (ot/avy-copy-here pt (lambda ()
                         (ot/step-out-forward)
                         (backward-char))))

(defun ot/avy-action-copy-sexp-backward-here (pt)
  "Move from PT to beginning of sexp to current location"
  (ot/avy-copy-here pt (lambda ()
                         (paredit-backward-up)
                         (forward-char))))

;; Avy move actions

(defun ot/avy-action-move-hydra (pt)
  (setq ot/avy-selected-pt pt)
  (hydra-avy-move-actions/body))

(defun ot/avy-action-move-sexp-here (pt)
  "Move sexp at PT to current location"
  (ot/avy-move-here pt 'forward-sexp))

(defun ot/avy-action-move-symbol-here (pt)
  "Move symbol at PT to current location"
  (ot/avy-move-here pt (lambda () (forward-symbol 1))))

(defun ot/avy-action-move-surrounding-sexp-here (pt)
  "Move surrounding sexp at PT to current location"
  (let ((new-pt (save-excursion
                  (goto-char pt)
                  (paredit-backward-up)
                  (point))))
    (ot/avy-move-here new-pt 'forward-sexp)))

(defun ot/avy-action-move-sexp-forward-here (pt)
  "Move from PT to end of sexp to current location"
  (ot/avy-move-here pt (lambda ()
                         (ot/step-out-forward)
                         (backward-char))))

(defun ot/avy-action-move-sexp-backward-here (pt)
  "Move from PT to beginning of sexp to current location"
  (ot/avy-move-here pt (lambda ()
                         (paredit-backward-up)
                         (forward-char))))

;; Avy goto commands

(defun ot/avy-goto-sexp (arg)
  (interactive "P")
  (avy-with ot/avy-goto-sexp
    (avy--generic-jump "\\s(\\|\\s\"[[:alnum:]]" arg avy-style)))

(defun ot/avy-goto-word-0 (arg)
  "avy-goto-word-0 with modified syntax table"
  (interactive "P")
  (let ((temp-syntax-table (make-syntax-table (syntax-table)))
        (avy-goto-word-0-regexp "\\b\\sw"))
    (modify-syntax-entry ?_ "w" temp-syntax-table)
    (modify-syntax-entry ?: "w" temp-syntax-table)
    (modify-syntax-entry ?- "w" temp-syntax-table)
    (modify-syntax-entry ?/ "w" temp-syntax-table)
    (modify-syntax-entry ?. "w" temp-syntax-table)
    (with-syntax-table temp-syntax-table
      (avy-with ot/avy-goto-word-0
        (avy--generic-jump avy-goto-word-0-regexp arg avy-style)))))

(provide 'custom-defuns)
