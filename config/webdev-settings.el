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

;; vue
(defun vue-company-backends ()
  (require 'company-lsp)
  (make-local-variable 'company-backends)
  (setq company-backends '((company-lsp company-capf company-dabbrev))))

(defun setup-vue-mode ()
  (require 'lsp-mode)
  (with-eval-after-load 'lsp-mode
    (require 'lsp-flycheck))
  (require 'lsp-vue)
  (add-node-modules-path)
  (smartparens-mode +1)
  (lsp-vue-mmm-enable)
  (vue-company-backends))

(require 'vue-mode)
(add-hook 'vue-mode-hook #'setup-vue-mode)

;; typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (add-node-modules-path)
  (setq typescript-indent-level 2)
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "dark orange")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (show-paren-mode 1)
  (setq hl-paren-colors '("dark orange"))
  (highlight-parentheses-mode +1)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
  (smartparens-strict-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving - disable, use prettier instead
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; prettier
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'vue-mode-hook 'prettier-js-mode)

(provide 'webdev-settings)
