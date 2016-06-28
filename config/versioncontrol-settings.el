(with-eval-after-load "magit"
  ;; don't prompt me
  (set-default 'magit-unstage-all-confirm nil)
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-revert-buffers 'silent)
  ;; When c is pressed when nothing is staged commit all changes
  (setq magit-commit-all-when-nothing-staged t)

  (setq magit-save-repository-buffers 'dontask)
  (setq magit-auto-revert-immediately t)
  (add-to-list 'magit-no-confirm 'safe-with-wip)

  (setq magit-completing-read-function 'ivy-completing-read)

  ;; full screen magit-status
  (setq magit-display-buffer-function
        (lambda (buffer)
          (if (or
               ;; the original should stay alive, so we can't go fullscreen
               magit-display-buffer-noselect
               ;; don't go fullscreen for certain magit buffers if current
               ;; buffer is a magit buffer (we're conforming to
               ;; `magit-display-buffer-traditional')
               (and (derived-mode-p 'magit-mode)
                    (not (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))))
              ;; open buffer according to original magit rules
              (magit-display-buffer-traditional buffer)
            ;; open buffer in fullscreen
            (delete-other-windows)
            ;; make sure the window isn't dedicated, otherwise
            ;; `set-window-buffer' throws an error
            (set-window-dedicated-p nil nil)
            (set-window-buffer nil buffer)
            ;; return buffer's window
            (get-buffer-window buffer))))

  )

;; Highlight diffs in fringe
(setq diff-hl-draw-borders nil)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(provide 'versioncontrol-settings)
