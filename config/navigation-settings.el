;; Avy
(with-eval-after-load "avy"
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?o ?p ?v ?b))
  (setq avy-dispatch-alist '((?X . avy-action-kill-move)
                             (?x . avy-action-kill-stay)
                             (?M . avy-action-mark)
                             (?m . ot/avy-action-move-here)
                             (?c . avy-action-copy)
                             (?i . avy-action-ispell)))
  (setq avy-all-windows 'all-frames)
  (setq avy-style 'at-full)
  (setq avy-background t)
  (setq avy-timeout-seconds 0.5))

(provide 'navigation-settings)
