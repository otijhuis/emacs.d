;; ido-mode is like magic pixie dust!
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
      ido-max-prospects 10
      ido-use-faces nil ;; disable ido faces to see flx highlights
      flx-ido-use-faces t ;; enable flx highlights
      ido-vertical-define-keys 'C-n-C-p-up-down-left-right
      )

;;(defvar ido-dont-ignore-buffer-names '("*scratch*"))
(defvar ido-dont-ignore-buffer-names '())

(defun ido-ignore-most-star-buffers (name)
  (and
   (string-match-p "^*" name)
   (not (member name ido-dont-ignore-buffer-names))))

(setq ido-ignore-buffers (list "\\` " #'ido-ignore-most-star-buffers))

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

;;;; ido customization
(require 'flx-ido)
(ido-mode t)
(flx-ido-mode 1)

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)

;; Ido at point (C-,)
(require 'ido-at-point)
(setq ido-at-point-use-helm nil)
(setq ido-at-point-partial t)
(setq ido-at-point-fuzzy t)
(ido-at-point-mode)

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
