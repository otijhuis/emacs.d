;; Magit settings
(defun magit-status-fullscreen (prefix)
  (interactive "P")
  (magit-status)
  (unless prefix
    (delete-other-windows)))

(with-eval-after-load "magit"
  ;; don't prompt me
  (set-default 'magit-unstage-all-confirm nil)
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-revert-buffers 'silent)
  ;; When c is pressed when nothing is staged commit all changes
  (setq magit-commit-all-when-nothing-staged t)

  (setq magit-save-repository-buffers 'dontask))

;; Highlight diffs in fringe
(with-eval-after-load "diff-hl"
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(provide 'versioncontrol-settings)
