;; Rich minority (clean up modeline)
(with-eval-after-load "rich-minority"
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
                       " drag")))

(rich-minority-mode 1)

;; Multiple Cursors
(with-eval-after-load "multiple-cursors"
  (setq mc/unsupported-minor-modes
        '(company-mode auto-complete-mode flyspell-mode jedi-mode)))

;; Volatile Highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Indent Guide
;; (indent-guide-global-mode)

(setq indent-guide-threshold 0)
(setq indent-guide-recursive t)
(setq indent-guide-delay 0.1)

;; Key chords
(with-eval-after-load "key-chord"
  (setq key-chord-two-keys-delay 0.2))

(key-chord-mode +1)

;; If no region is selected then work on current line
(whole-line-or-region-mode 1)

;; Undo tree
(with-eval-after-load "undo-tree"
  (setq undo-tree-mode-lighter "")
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

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
(wrap-region-add-wrapper "`" "`" nil '(markdown-mode))

;; Drag stuff
(setq drag-stuff-modifier 'hyper)
(drag-stuff-global-mode t)

;; Thing at point
(eval-after-load "thingatpt"
  '(when (require 'thingatpt+)
     (tap-redefine-std-fns)))

;; iMenu list
(setq imenu-list-focus-after-activation t)
(setq imenu-list-auto-resize t)
(setq imenu-list-position 'left)

;; Beacon
(beacon-mode 1)
(setq beacon-color "orange")
(setq beacon-size 50)
(setq beacon-blink-when-point-moves-vertically 3)
(setq beacon-blink-when-point-moves-horizontally 75)
(setq beacon-blink-duration 0.2)
(setq beacon-blink-delay 0.2)
(setq beacon-blink-when-focused t)
(setq beacon-dont-blink-commands '(avy-goto-char
                                   avy-goto-char-in-line
                                   avy-goto-line
                                   avy-goto-word-0
                                   avy-goto-word-1
                                   avy-goto-char-timer
                                   avy-goto-char-2
                                   avy-zap-to-char
                                   avy-zap-up-to-char
                                   avy-zap-to-char-dwim
                                   avy-zap-up-to-char-dwim
                                   avy-goto-word-or-subword-1
                                   exchange-point-and-mark))
(setq beacon-lighter " 💡")

;; State
(state-global-mode 1)

(state-define-state
  scratch
  :key "s"
  :switch "*scratch*")

(state-define-state
  message
  :key "m"
  :switch "*Messages*")

;; Ace Window
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;(ace-window-display-mode 1)

;; ibuffer
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))

;; peep-dired
;; (setq peep-dired-cleanup-on-disable t)
(setq peep-dired-cleanup-eagerly t)
(setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "mov" "elc" "jar" "war" "ear"))
;; (setq peep-dired-enable-on-directories t)

(provide 'misc-package-settings)
