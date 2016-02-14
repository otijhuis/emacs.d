(with-eval-after-load "flycheck"
  (flycheck-clojure-setup)
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
  (add-to-list 'flycheck-checkers 'clojure-cider-eastwood)
  (flycheck-pos-tip-mode))
;;(add-hook 'after-init-hook #'global-flycheck-mode)

(with-eval-after-load "cider"
  (defun my-cider-mode-enable-flycheck ()
    (when (and (s-ends-with-p ".clj" (buffer-file-name))
               (not (s-ends-with-p "/dev/user.clj" (buffer-file-name))))
      (flycheck-mode 1)))

  (add-hook 'cider-mode-hook 'my-cider-mode-enable-flycheck)
  (add-hook 'cider-mode-hook
            (lambda () (setq next-error-function #'flycheck-next-error-function))))

(provide 'syntax-checking-settings)
