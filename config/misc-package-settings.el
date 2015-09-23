;; Rich minority (clean up modeline)
(require 'rich-minority)
(setq rm-blacklist '(" hl-s"
                     " company"
                     " wr"
                     " Guide"
                     " Anzu"
                     " ing"
                     " WLR"
                     " Anzu"
                     " ElDoc"
                     " Paxedit"
                     " Paredit"
                     " $"
                     " =>"
                     " SliNav"
                     " WSC"
                     " VHl"
                     " yas"
                     " drag"))
(rich-minority-mode 1)

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

(setq key-chord-two-keys-delay 0.2)

(key-chord-mode +1)

;; If no region is selected then work on current line
(require 'whole-line-or-region)

(whole-line-or-region-mode 1)

;; Undo tree
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

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

;; Highlight symbol
(setq highlight-symbol-idle-delay 0.5)

;; Wrap region
(wrap-region-global-mode)

;; Drag stuff
(setq drag-stuff-modifier 'hyper)
(require 'drag-stuff)
(drag-stuff-global-mode t)

;; Thing at point
(require 'thingatpt)
(require 'thingatpt+)

;; iMenu list
(require 'imenu-list)
(setq imenu-list-focus-after-activation t)
(setq imenu-list-auto-resize t)
(setq imenu-list-position 'left)

(provide 'misc-package-settings)
