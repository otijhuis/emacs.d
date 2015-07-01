(global-font-lock-mode t)

(if (string= (symbol-name system-type) "windows-nt")
    (setq default-frame-alist '((font . "Consolas-11")))
  (modify-frame-parameters nil '((wait-for-wm . nil))))

(if (string= (symbol-name system-type) "darwin")
    (setq default-frame-alist '((font . "Menlo-14")))
  (modify-frame-parameters nil '((wait-for-wm . nil))))

(load-theme 'flatui t)

;; Flat modeline
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))

(provide 'ui-settings)
