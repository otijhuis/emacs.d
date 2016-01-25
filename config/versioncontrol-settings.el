;; Magit settings
(defun magit-status-fullscreen (prefix)
  (interactive "P")
  (magit-status)
  (unless prefix
    (delete-other-windows)))

;; don't prompt me

(set-default 'magit-unstage-all-confirm nil)
(set-default 'magit-stage-all-confirm nil)
(set-default 'magit-revert-buffers 'silent)

;; When c is pressed when nothing is staged commit all changes
(setq magit-commit-all-when-nothing-staged t)

(setq magit-save-repository-buffers 'dontask)

;; Highlight diffs in fringe
(global-diff-hl-mode)
(diff-hl-flydiff-mode)

;; full screen vc-annotate

(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))

     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

(provide 'versioncontrol-settings)
