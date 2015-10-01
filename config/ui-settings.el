(global-font-lock-mode t)

(if (string= (symbol-name system-type) "windows-nt")
    (setq default-frame-alist '((font . "Consolas-11")))
  (modify-frame-parameters nil '((wait-for-wm . nil))))

(if (string= (symbol-name system-type) "darwin")
    (setq default-frame-alist '((font . "Menlo-12")))
  (modify-frame-parameters nil '((wait-for-wm . nil))))

;; Disable bold
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal))
 (face-list))

;; Make frame transparent
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))

;;(load-theme 'flatui t)
(load-theme 'molokai t)

;; Flat modeline
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(setq truncate-partial-width-windows nil)

(scroll-bar-mode -1)

(provide 'ui-settings)
