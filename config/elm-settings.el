(with-eval-after-load "elm-mode"
  (setq elm-tags-on-save t)
  (setq elm-tags-exclude-elm-stuff t)
  (setq elm-sort-imports-on-save nil)
  (setq elm-format-on-save nil))

(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
(add-hook 'elm-mode-hook (lambda ()
                           (flycheck-mode 1)))

(provide 'elm-settings)
