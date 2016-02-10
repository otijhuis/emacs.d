;;(require 'popup)
;;(require 'yasnippet)

(with-eval-after-load "yasnippet"
  ;; Use only own snippets, do not use bundled ones
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

  (setq yas-prompt-functions '(yas/popup-isearch-prompt yas-no-prompt))
  ;;(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

  ;; No need to be so verbose
  (setq yas-verbosity 1)

  ;; Wrap around region
  (setq yas-wrap-around-region t)
  (yas-global-mode 1)
  )

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

(provide 'yasnippet-settings)
