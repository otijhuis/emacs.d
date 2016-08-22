(with-eval-after-load "elm-mode"
  (setq elm-tags-on-save t)
  (setq elm-tags-exclude-elm-stuff t)
  (setq elm-sort-imports-on-save t)
  (setq elm-format-on-save t))

(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)

(provide 'elm-settings)
