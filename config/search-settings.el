;; Ag

(with-eval-after-load "ag"
  (setq ag-highlight-search t)
  (setq ag-reuse-window 't)
  (setq ag-reuse-buffers 't))

;; Cursor position at start of match, not at the end
;; (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
;; (defun my-goto-match-beginning ()
;;   (when (and isearch-forward (not isearch-mode-end-hook-quit))
;;     (goto-char isearch-other-end)))

(provide 'search-settings)
