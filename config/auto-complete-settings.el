;;(require 'color)

;;(require 'company)
;;(require 'company-etags)

;; Auto-complete
(add-hook 'after-init-hook 'global-company-mode)

;; yasnippet backend shadows other completions, see https://github.com/company-mode/company-mode/blob/master/company-yasnippet.el for solutions
;;(eval-after-load 'company '(add-to-list 'company-backends 'company-yasnippet))

;;(eval-after-load 'company '(add-to-list 'company-backends 'company-files))
(with-eval-after-load "company"
  (add-to-list 'company-transformers 'company-sort-by-occurrence)
  (setq company-idle-delay nil) ; never start completions automatically
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 1)
  (setq company-echo-delay 0)
  (setq company-auto-complete nil)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (setq company-dabbrev-other-buffers t)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-tooltip-align-annotations t)
  (setq completion-styles '(basic initials partial-completion emacs22)) ; default is (basic partial-completion emacs22)
  )

;;(setq company-backends '(company-dabbrev (company-keywords company-dabbrev-code) company-files))
(defun clojure-company-backends ()
  (make-local-variable 'company-backends)
  (setq company-backends '((company-etags company-capf company-dabbrev))))
(add-hook 'clojure-mode-hook 'clojure-company-backends)

(defun elm-company-backends ()
  (make-local-variable 'company-backends)
  (setq company-backends '((company-etags company-capf company-dabbrev company-elm))))
(add-hook 'elm-mode-hook 'elm-company-backends)

(defun tern-company-backends ()
  (make-local-variable 'company-backends)
  (setq company-backends '((company-etags company-capf company-dabbrev company-files company-tern))))
(add-hook 'tern-mode-hook 'tern-company-backends)

(defun css-company-backends ()
  (setq company-backends '((company-css company-capf company-dabbrev))))
(add-hook 'css-mode-hook 'css-company-backends)

;; org-mode completions
(defun my-pcomplete-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
(add-hook 'org-mode-hook #'my-pcomplete-capf)

;; Quickhelp
(with-eval-after-load "company-quickhelp"
  (setq company-quickhelp-delay 1))

(add-hook 'company-mode-hook 'company-quickhelp-mode)

;; disable indent guide while completion is active
(add-hook 'company-mode-hook
          (lambda ()
            (add-hook 'company-completion-started-hook (lambda (&optional arg)
                                                         (indent-guide-mode -1)) nil 'make-it-local)))

(add-hook 'company-mode-hook
          (lambda ()
            (add-hook 'company-completion-cancelled-hook (lambda (&optional arg)
                                                           (indent-guide-mode 1)) nil 'make-it-local)))

(add-hook 'company-mode-hook
          (lambda ()
            (add-hook 'company-completion-finished-hook (lambda (&optional arg)
                                                          (indent-guide-mode 1)) nil 'make-it-local)))

;; company-flx
(with-eval-after-load 'company
  (company-flx-mode +1))

(provide 'auto-complete-settings)
