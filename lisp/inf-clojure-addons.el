(require 'inf-clojure)

;; overlays
(dolist (o (overlays-in (window-start) (window-end)))
  (delete-overlay o))
;; ----

(defun major-buffer (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
                      (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))

(defun inf-clojure-ext-output-to-minibuffer (txt)
  (let ((txt-without-prompts (replace-regexp-in-string
                              "^\n\\|\n+\\'"
                              ""
                              (replace-regexp-in-string
                               inf-clojure-prompt
                               ""
                               txt))))
    (if (not (string= "" txt-without-prompts))
        (message txt-without-prompts))))

;;(add-hook 'comint-preoutput-filter-functions #'inf-clojure-ext-remove-subprompts)

(add-hook 'comint-output-filter-functions #'inf-clojure-ext-output-to-minibuffer)

(provide 'inf-clojure-addons)
