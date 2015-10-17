(require 'inf-clojure)

;; overlays
(dolist (o (overlays-in (window-start) (window-end)))
  (delete-overlay o))
;; ----

(defun inf-clojure-ext-output-to-minibuffer (txt)
  (let ((txt-without-prompts (replace-regexp-in-string
                              "^\n\\|\n+\\'"
                              ""
                              (replace-regexp-in-string
                               inf-clojure-prompt
                               ""
                               txt))))
    (message txt-without-prompts)))

;;(add-hook 'comint-preoutput-filter-functions #'inf-clojure-ext-remove-subprompts)

(add-hook 'comint-output-filter-functions #'inf-clojure-ext-output-to-minibuffer)

(provide 'inf-clojure-addons)
