(require 'popup)
(require 'yasnippet)

(setq yas-prompt-functions '(yas/popup-isearch-prompt yas-no-prompt))
(yas-global-mode 1)

(provide 'yasnippet-settings)
