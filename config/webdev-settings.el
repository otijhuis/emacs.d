;; (require 'tagedit)

;; (require 'company-web-html)                          ; load company mode html backend
;; (require 'css-eldoc)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode t)))
;; Change some defaults: customize them to override
(setq-default js2-basic-offset 2
              js2-bounce-indent-p nil)

;; json-mode
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(provide 'webdev-settings)
