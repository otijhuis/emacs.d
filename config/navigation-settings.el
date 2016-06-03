;; Avy
(with-eval-after-load "avy"
  (setq avy-all-windows 'all-frames)
  (setq avy-style 'at-full)
  (setq avy-background t)
  (setq avy-timeout-seconds 0.5))

(provide 'navigation-settings)
