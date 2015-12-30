(require 'flycheck-clojure)

(eval-after-load 'flycheck '(flycheck-clojure-setup))
;;(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(add-hook 'cider-mode-hook
          (lambda () (setq next-error-function #'flycheck-next-error-function)))

(defun my-cider-mode-enable-flycheck ()
  (when (and (s-ends-with-p ".clj" (buffer-file-name))
             (not (s-ends-with-p "/dev/user.clj" (buffer-file-name))))
    (flycheck-mode 1)))

(add-hook 'cider-mode-hook 'my-cider-mode-enable-flycheck)

(eval-after-load 'flycheck '(add-to-list 'flycheck-checkers 'clojure-cider-eastwood))

(provide 'syntax-checking-settings)
