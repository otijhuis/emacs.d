;; Avy
(with-eval-after-load "avy"
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?o ?p ?v ?b))
  (setq avy-dispatch-alist '((?X . avy-action-kill-move)
                             (?x . avy-action-kill-stay)
                             (?M . avy-action-mark)
                             (?m . ot/avy-action-move-hydra)
                             (?C . avy-action-copy)
                             (?c . ot/avy-action-copy-hydra)
                             (?i . avy-action-ispell)))
  (setq avy-styles-alist '((avy-goto-word-0 . pre)
                           (ot/avy-goto-word-0 . pre)
                           (ot/avy-goto-sexp . pre)))
  (setq avy-all-windows 'all-frames)
  (setq avy-style 'at-full)
  (setq avy-background t)
  (setq avy-timeout-seconds 0.5))

(provide 'navigation-settings)
