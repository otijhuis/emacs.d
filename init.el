(require 'package)

(when (>= emacs-major-version 24)
  (setq package-archives '(("org" . "http://orgmode.org/elpa/")
                           ("ELPA" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")
                           ("melpa-stable" . "http://stable.melpa.org/packages/")
                           )))

;; Check if we're on Emacs 24.4 or newer, if so, use the pinned package feature
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '(
          ;;(cider . "melpa-stable")
          )))

;;(add-to-list 'package-archives
;;    '("marmalade" .
;;      "http://marmalade-repo.org/packages/"))
(setq package-enable-at-startup t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(ido-ubiquitous
                      ido-vertical-mode
                      flx
                      flx-ido
                      ido-at-point
                      ido-select-window
                      idomenu
                      ido-completing-read+

                      swiper
                      ivy
                      ivy-hydra
                      ivy-todo
                      counsel
                      counsel-projectile

                      smex

                      command-log-mode

                      helm
                      helm-ag
                      helm-swoop
                      helm-projectile
                      helm-flx
                      helm-gtags
                      helm-descbinds
                      helm-c-moccur
                      helm-flycheck
                      helm-emmet
                      helm-clojuredocs

                      elm-mode
                      elm-yasnippets
                      flycheck-elm

                      ggtags
                      noccur ; occur in project
                      state ; navigating between workspaces

                      imenu-list
                      imenu-anywhere ; imenu in a separate buffer

                      engine-mode ; use search engines

                      beacon ; find your cursor
                      avy
                      avy-zap ; move quickly around buffers (see vim EasyMotion as well)
                      ace-window ; window navigation
                      smartscan ; quickly jump to symbols
                      goto-chg

                                        ; dired+ ; emacs wiki
                                        ; dired-sort ; emacs wiki
                      peep-dired

                      dash
                      s
                      f ; api for working with files/directories
                                        ; thingatpt+ ; emacs wiki
                      comment-dwim-2
                                        ; misc-cmds ; misc useful functions , emacs wiki
                      popup popwin ; popups

                      whole-line-or-region ; if no region selected act on the current line
                      wrap-region
                      change-inner ; Emacs version of vim's ci and co commands
                      drag-stuff ; moving lines/regions up/down
                      rich-minority ; don't clutter the modeline with minor mode names
                      fix-word ; improved up-/downcase and capitalize functions
                      ag
                      browse-kill-ring ; list / select / insert previously killed text
                      expand-region ; easily select regions around point
                      multiple-cursors
                      iedit
                      visual-regexp
                      visual-regexp-steroids

                      bind-key
                      free-keys
                      key-chord key-seq ; bind commands to multiple keypresses
                      selected
                      hydra ; sticky bindings
                      which-key ; display keybindings (guide-key replacement)

                      focus ; focus mode (dim text you are not working on)
                      volatile-highlights
                      ;; highlight-symbol
                      symbol-overlay
                      hl-sexp
                      highlight-parentheses
                      highlight-defined
                      diff-hl ; diff highlighting

                      aggressive-indent

                      flycheck
                      flycheck-pos-tip
                      flycheck-clojure ; syntax checking
                      ;; flycheck-haskell

                      org-plus-contrib

                      undo-tree ; visualize undo as a tree (extremely handy)
                      neotree ; nerdtree style directory tree

                      company
                      company-quickhelp
                      company-flx
                      company-web
                      company-ghci
                      company-tern
                      company-lsp
                      ;; company-ghc

                      elisp-slime-nav

                      magit ; version control
                      git-timemachine

                      clojure-mode
                      clj-refactor
                      cider
                      cider-eval-sexp-fu
                      clojure-mode-extra-font-locking
                      clojure-cheatsheet
                      evalator
                      evalator-clojure

                      paredit
                      paredit-menu
                      paxedit
                      sexp-move
                      smartparens

                      flatui-theme
                      dracula-theme
                      darkokai-theme
                      spacemacs-theme

                      projectile
                      ibuffer-vc

                      yasnippet
                      auto-yasnippet
                      ;; haskell-snippets

                      css-eldoc
                      js-doc
                      js2-mode
                      json-mode
                      js2-refactor
                      vue-mode
                      lsp-mode
                      lsp-vue
                      prettier-js
                      tern
                      tagedit
                      emmet-mode
                      web-beautify
                      add-node-modules-path

                      typescript-mode
                      tide

                      ;;haskell-mode
                      ;; shm
                      ;; hindent
                      ;; hi2
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

(autoload 'rotate-text "rotate-text" nil t)
(autoload 'rotate-text-backward "rotate-text" nil t)

;;(add-to-list 'load-path "~/Projects/github/inf-clojure")
;;(require 'inf-clojure)

;; On OSX add /usr/local/bin to exec-path for lein command (installed with brew install leiningen)
(if (eq system-type 'darwin)
    (add-to-list 'exec-path "/usr/local/bin"))

(require 'misc)
(require 'uniquify)
(require 'custom-defuns)
(require 'basic-settings)
(require 'ido-settings)
(require 'ivy-settings)
(require 'expand-region)
(require 'ui-settings)
(require 'lisp-settings)
(require 'webdev-settings)
(require 'navigation-settings)
(require 'project-settings)
(require 'auto-complete-settings)
(require 'yasnippet-settings)
(require 'versioncontrol-settings)
(require 'search-settings)
(require 'helm-settings)
(require 'org-mode-settings)
(require 'syntax-checking-settings)
(require 'hippie-expand-settings)
(require 'dired-settings)
(require 'misc-package-settings)
(require 'hydra-settings)
(require 'clojure-utils)
(require 'haskell-settings)
(require 'elm-settings)
(require 'custom-keybindings)

;;(require 'inf-clojure-addons)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fci-rule-color "#efefef")
 '(helm-gtags-auto-update t)
 '(helm-gtags-display-style (quote detail))
 '(helm-gtags-fuzzy-match t)
 '(helm-gtags-path-style (quote root))
 '(package-selected-packages
   (quote
    (wrap-region whole-line-or-region which-key web-beautify volatile-highlights visual-regexp-steroids undo-tree tide thingatpt+ tagedit state spacemacs-theme smex smartscan smartparens sexp-move selected rich-minority popwin peep-dired paxedit paredit-menu org-plus-contrib noccur neotree misc-cmds magit key-seq json-mode js2-refactor js-doc indent-guide imenu-list imenu-anywhere iedit idomenu ido-vertical-mode ido-ubiquitous ido-select-window ido-at-point ibuffer-vc hl-sexp highlight-symbol highlight-defined helm-swoop helm-projectile helm-gtags helm-flycheck helm-flx helm-emmet helm-descbinds helm-clojuredocs helm-c-moccur helm-ag goto-chg git-timemachine ggtags free-keys focus flycheck-pos-tip flycheck-clojure flx-ido flatui-theme fix-word f evalator-clojure engine-mode elisp-slime-nav drag-stuff dracula-theme dired-sort dired+ diff-hl darkokai-theme css-eldoc counsel-projectile company-web company-tern company-quickhelp company-ghci company-flx comment-dwim-2 command-log-mode clojure-mode-extra-font-locking clojure-cheatsheet clj-refactor cider-eval-sexp-fu change-inner browse-kill-ring bind-key beacon avy-zap auto-yasnippet aggressive-indent ag ace-window)))
 '(safe-local-variable-values
   (quote
    ((eval turn-on-orgtbl)
     (whitespace-line-column . 80)
     (lexical-binding . t))))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((30 . "#e74c3c")
     (60 . "#c0392b")
     (90 . "#e67e22")
     (120 . "#d35400")
     (150 . "#f1c40f")
     (180 . "#d98c10")
     (210 . "#2ecc71")
     (240 . "#27ae60")
     (270 . "#1abc9c")
     (300 . "#16a085")
     (330 . "#2492db")
     (360 . "#0a74b9"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line ((t (:foreground "dark orange" :weight bold))))
 '(avy-lead-face ((t (:foreground "red"))))
 '(avy-lead-face-0 ((t (:foreground "red1" :weight bold))))
 '(company-scrollbar-bg ((t (:background "#fefeff"))))
 '(company-scrollbar-fg ((t (:background "#fefeff"))))
 '(company-tooltip ((t (:inherit default :background "#fefeff"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(diff-added ((t (:foreground "medium spring green"))))
 '(diff-hl-change ((t (:background "deep sky blue"))))
 '(diff-hl-delete ((t (:background "tomato1"))))
 '(diff-hl-insert ((t (:background "light green"))))
 '(diff-removed ((t (:foreground "red3"))))
 '(eval-sexp-fu-flash ((t (:background "dark orange" :foreground "white"))))
 '(flx-highlight-face ((t (:inherit font-lock-variable-name-face :foreground "#F92672"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "gray70" :slant italic))))
 '(font-lock-comment-face ((t (:foreground "gray60" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "#F92672" :weight normal))))
 '(helm-header ((t (:background "dark slate gray" :foreground "white"))))
 '(helm-selection ((t (:background "orange1" :foreground "white" :underline nil :weight bold))))
 '(helm-separator ((t (:foreground "dodger blue"))))
 '(helm-source-header ((t (:background "gray30" :foreground "white" :weight normal :height 1 :family "Sans Serif"))))
 '(helm-swoop-target-word-face ((t (:foreground "dodger blue"))))
 '(helm-visible-mark ((t (:background "dark orange" :foreground "white"))))
 '(hl-paren-face ((t (:weight extra-bold))) t)
 '(hl-sexp-face ((t (:background nil :weight bold))))
 '(ido-first-match ((t (:foreground "#ccff66"))))
 '(ido-incomplete-regexp ((t (:foreground "#ffffff"))))
 '(ido-indicator ((t (:foreground "#ffffff"))))
 '(ido-only-match ((t (:foreground "#ffcc33"))))
 '(ido-subdir ((t (:foreground "#66ff00"))))
 '(iedit-occurrence ((t (:background "SystemHilight" :foreground "deep sky blue"))))
 '(indent-guide-face ((t (:foreground "gainsboro"))))
 '(isearch ((t (:background "white" :foreground "magenta" :weight bold))))
 '(isearch-fail ((t (:background "#c0392b" :foreground "white" :weight bold))))
 '(ivy-current-match ((t (:inherit bold :background "lavender"))))
 '(ivy-minibuffer-match-face-2 ((t (:foreground "#F92672" :underline nil))))
 '(ivy-minibuffer-match-face-3 ((t (:foreground "#F92672" :underline nil))))
 '(ivy-minibuffer-match-face-4 ((t (:foreground "#F92672" :underline nil))))
 '(ivy-virtual ((t nil)))
 '(lazy-highlight ((t (:background "white smoke" :foreground "dark orange" :weight bold))))
 '(match ((t (:background "gray93" :foreground "dodger blue" :weight bold))))
 '(region ((t (:background "SkyBlue4" :foreground "white"))))
 '(swiper-line-face ((t (:background "white" :weight bold))))
 '(vhl/default-face ((t (:background "LightSteelBlue1"))))
 '(web-mode-current-element-highlight-face ((t (:background "black"))))
 '(web-mode-html-attr-name-face ((t (:foreground "royal blue"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "dim gray"))))
 '(web-mode-html-tag-face ((t (:foreground "DarkOrange1")))))
