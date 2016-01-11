;; Ag
(require 'ag)

(setq ag-highlight-search t)
(setq ag-reuse-window 't)
(setq ag-reuse-buffers 't)

;; Anzu
(setq anzu-search-threshold 1000
    anzu-cons-mode-line-p t)
(global-anzu-mode +1)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Cursor position at start of match, not at the end
;; (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
;; (defun my-goto-match-beginning ()
;;   (when (and isearch-forward (not isearch-mode-end-hook-quit))
;;     (goto-char isearch-other-end)))

(provide 'search-settings)
