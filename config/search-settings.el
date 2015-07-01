;; Ag
(require 'ag)

(setq ag-highlight-search t)
(setq ag-reuse-window 't)
(setq ag-reuse-buffers 't)

;; Anzu
(setq anzu-search-threshold 1000
    anzu-cons-mode-line-p t)
(global-anzu-mode +1)

(provide 'search-settings)
