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

(provide 'search-settings)
