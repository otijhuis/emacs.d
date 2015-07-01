;;;;;;;;;;
;; Helm ;;
;;;;;;;;;;

(require 'helm)
(require 'helm-config)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    4 ; scroll 4 lines other window using M-<next>/M-<prior>
      helm-google-suggest-use-curl-p t
      fit-window-to-buffer-horizontally           1
      helm-idle-delay                            0.01
      helm-input-idle-delay                      0.01
      helm-ff-auto-update-initial-value          t
      ;;helm-reuse-last-window-split-state         t
      helm-always-two-windows                    t
      helm-ls-git-status-command                 'magit-status
      helm-M-x-requires-pattern                  0
      helm-dabbrev-cycle-threshold                5
      helm-boring-file-regexp-list               '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$")
      helm-buffer-skip-remote-checking            t
      ido-use-virtual-buffers                     t             ; Needed in helm-buffers-list
      helm-org-headings-fontify                   t
      helm-buffers-to-resize-on-pa                '("*helm apropos*" "*helm ack-grep*"
                                                    "*helm grep*" "*helm occur*"
                                                    "*helm multi occur*" "*helm lsgit*"
                                                    "*helm git-grep*" "*helm hg files*")
      helm-ff-file-name-history-use-recentf t
      helm-completion-in-region-fuzzy-match       t
      helm-recentf-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-locate-fuzzy-match t
      helm-M-x-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-lisp-fuzzy-completion t)

;;; Describe key-bindings
;;
;;
(helm-descbinds-install)            ; C-h b, C-x C-h

;; Avoid hitting forbidden directory .gvfs when using find.
(add-to-list 'completion-ignored-extensions ".gvfs/")

;; Add moccur/occur sources to helm-sources-using-default-as-input.
(add-to-list 'helm-sources-using-default-as-input 'helm-source-occur)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-moccur)

;; (eval-after-load 'helm-mode
;; '(add-to-list 'helm-completing-read-handlers-alist '(find-file)))

(helm-mode 1)
(helm-adaptative-mode 1)

(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)

(helm-autoresize-mode 1)

;;;;;;;;;;;;;;;;
;; Helm swoop ;;
;;;;;;;;;;;;;;;;

(require 'helm-swoop)
;; match only for symbol
;; (setq helm-swoop-pre-input-function
;; (lambda () (format "\\_<%s\\_> " (thing-at-point 'symbol))))

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

(provide 'helm-settings)
