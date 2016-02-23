;; Projectile

;;(require 'projectile)
(with-eval-after-load "projectile"
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line-lighter "Project")
  (setq projectile-enable-idle-timer t)
  (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))))

(projectile-global-mode)

(provide 'project-settings)
