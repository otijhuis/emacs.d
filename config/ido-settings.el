;; ido-mode is like magic pixie dust!
(with-eval-after-load "ido"
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-case-fold  nil                 ; be case-sensitive
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-enable-regexp nil
        ido-enable-last-directory-history nil
        ido-handle-duplicate-virtual-buffers 2
        confirm-nonexistent-file-or-buffer nil
        ido-file-extension-order '(".clj" ".cljs" ".cljc" ".html" ".el" ".org" ".txt" ".js") ; give priority to certain file types
        ido-ignore-extensions t
        ido-default-file-method 'selected-window
        ido-max-prospects 10
        ido-use-faces nil ;; disable ido faces to see flx highlights
        ido-vertical-define-keys 'C-n-C-p-up-down-left-right
        )

  (add-to-list 'ido-ignore-directories "target")
  (add-to-list 'ido-ignore-directories "node_modules")
  (add-to-list 'ido-ignore-directories "out")

  (setq flx-ido-use-faces t) ;; enable flx highlights
  (flx-ido-mode 1)
  (ido-ubiquitous-mode 1)
  (ido-vertical-mode 1)

  ;; Ido at point (C-,)
  (setq ido-at-point-use-helm nil)
  (setq ido-at-point-partial t)
  (setq ido-at-point-fuzzy t)
  (ido-at-point-mode)
  )

(ido-mode t)

;; Use ido everywhere
;;(require 'ido-ubiquitous)
(with-eval-after-load "ido-ubiquitous"


  ;; Fix ido-ubiquitous for newer packages
  (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
    `(eval-after-load ,package
       '(defadvice ,cmd (around ido-ubiquitous-new activate)
          (let ((ido-ubiquitous-enable-compatibility nil))
            ad-do-it))))

  (ido-ubiquitous-use-new-completing-read webjump 'webjump)
  (ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
  (ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet))

(require 'dash)

(defun my/ido-go-straight-home ()
  (interactive)
  (cond
   ((looking-back "~/") (insert "projects/"))
   ((looking-back "/") (insert "~/"))
   (:else (call-interactively 'self-insert-command))))

(defun my/setup-ido ()
  ;; Go straight home
  (define-key ido-file-completion-map (kbd "~") 'my/ido-go-straight-home)
  (define-key ido-file-completion-map (kbd "C-~") 'my/ido-go-straight-home)

  ;; Use C-w to go back up a dir to better match normal usage of C-w
  ;; - insert current file name with C-x C-w instead.
  (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)

  (define-key ido-file-dir-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-dir-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name))

(add-hook 'ido-setup-hook 'my/setup-ido)

(provide 'ido-settings)
