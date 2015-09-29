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
        '((magit . "melpa-stable")
          ;;(cider . "melpa-stable")
          ;;(clj-refactor . "melpa-stable")
          )))

;;(add-to-list 'package-archives
;;    '("marmalade" .
;;      "http://marmalade-repo.org/packages/"))
(setq package-enable-at-startup t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(ido-ubiquitous ido-vertical-mode flx-ido ido-at-point ido-select-window idomenu ; mini-buffer on steroids
                                     smex ido-completing-read+ ; mini-buffer on steroids (fuzzy completion etc)
                                     helm helm-projectile helm-ag helm-swoop ; helm
                                     helm-descbinds helm-c-moccur ; helm
                                     imenu-list ; imenu in a separate buffer
                                     shrink-whitespace
                                     dash s thingatpt+ ; util libs
                                     comment-dwim-2    ; commenting
                                     wrap-region ; wrap region with quotes, parens etc
                                     bind-key free-keys region-bindings-mode ; keybinding utilities
                                     whole-line-or-region ; if no region selected act on the current line
                                     misc-cmds ; misc useful functions
                                     fix-word ; improved up-/downcase and capitalize functions
                                     whitespace-cleanup-mode ; clean up whitespace on save
                                     key-chord key-seq ; bind commands to multiple keypresses
                                     focus ; focus mode (dim text you are not working on)
                                     hydra ; sticky bindings
                                     volatile-highlights highlight-symbol hl-sexp ; highlighting
                                     change-inner ; Emacs version of vim's ci and co commands
                                     aggressive-indent indent-guide ; indentation
                                     flycheck flycheck-pos-tip helm-flycheck ; syntax checking
                                     which-key ; display keybindings (guide-key replacement)
                                     popup popwin ; popups
                                     org-plus-contrib org-bullets org-projectile ; latest org-mode
                                     undo-tree ; visualize undo as a tree (extremely handy)
                                     neotree ; nerdtree style directory tree
                                     company company-quickhelp ; autocomplete
                                     drag-stuff ; moving lines/regions up/down
                                     elisp-slime-nav highlight-defined ; extensions for elisp
                                     rich-minority ; don't clutter the modeline with minor mode names
                                     ag anzu       ; search / grep
                                     clojure-mode inf-clojure clojure-mode-extra-font-locking ; clojure
                                     align-cljlet ; clojure
                                     avy avy-zap ; move quickly around buffers (see vim EasyMotion as well)
                                     paredit paredit-menu paxedit sexp-move ; working with parens / delimiters
                                     solarized-theme flatui-theme ; color themes
                                     magit diff-hl gist ; git integration
                                     what-the-commit ; funny commit messages
                                     projectile ; moving around in projects
                                     yasnippet auto-yasnippet clojure-snippets datomic-snippets ; snippets
                                     browse-kill-ring ; list / select / insert previously killed text
                                     expand-region ; easily select regions around point
                                     multiple-cursors iedit ; multiple cursors a la Sublime Text
                                     visual-regexp visual-regexp-steroids ; visualize your typed regexp

                                     ;; Web development
                                     js2-mode company-web tagedit emmet-mode helm-emmet web-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; On OSX add /usr/local/bin to exec-path for lein command (installed with brew install leiningen)
(if (eq system-type 'darwin)
    (add-to-list 'exec-path "/usr/local/bin"))

(require 'misc)
(require 'uniquify)
(require 'custom-defuns)
(require 'basic-settings)
(require 'ido-settings)
(require 'expand-region)
(require 'ui-settings)
(require 'lisp-settings)
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
(require 'misc-package-settings)
(require 'hydra-settings)
(require 'custom-keybindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "0ba649556dc51762e6794b92017f6f7406754ae3136eafef686d81c6da176cc5" "8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "08851585c86abcf44bb1232bced2ae13bc9f6323aeda71adfa3791d6e7fea2b6" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "90d329edc17c6f4e43dbc67709067ccd6c0a3caa355f305de2041755986548f2" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" "0251780e8e79d2a5e75eec7ee3b6c646b882495cb884d9dd32f30c60f9d65db6" "d809ca3cef02087b48f3f94279b86feca896f544ae4a82b523fba823206b6040" "1ba463f6ac329a56b38ae6ac8ca67c8684c060e9a6ba05584c90c4bffc8046c3" "f5e9f66da69f504cb61aacedeb8284d8f38f2e6f835fd658cac5f0ad5d924549" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "a99e7c91236b2aba4cd374080c73f390c55173c5a1b4ac662eeb3172b60a9814" "c3fb7a13857e799bba450bb81b9101ef4960281c4d5908e05ecac9204c526c8a" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "7dd515d883520286fc8936ce32381fb01b978d0d7cfb6fe56f7f55d8accbf63a" "57072d797dc09fcf563051a85a29d6a51d6f2b1a602e029c35b05c30df319b2a" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "c4e6fe8f5728a5d5fd0e92538f68c3b4e8b218bcfb5e07d8afff8731cc5f3df0" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" default)))
 '(fci-rule-color "#efefef")
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
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
 '(cursor ((t (:background "OrangeRed1" :foreground "#2c3e50"))))
 '(flx-highlight-face ((t (:inherit font-lock-variable-name-face :foreground "#69D2E7"))))
 '(helm-header ((t (:background "dark slate gray" :foreground "white"))))
 '(helm-selection ((t (:background "orange1" :foreground "white" :underline nil :weight bold))))
 '(helm-separator ((t (:foreground "dodger blue"))))
 '(helm-source-header ((t (:background "gray30" :foreground "white" :weight normal :height 1 :family "Sans Serif"))))
 '(helm-swoop-target-word-face ((t (:foreground "dodger blue"))))
 '(helm-visible-mark ((t (:background "dark orange" :foreground "white"))))
 '(highlight-symbol-face ((t (:background "gray88" :underline nil))))
 '(hl-sexp-face ((t (:background "LightSteelBlue1"))))
 '(ido-first-match ((t (:foreground "#ccff66"))))
 '(ido-incomplete-regexp ((t (:foreground "#ffffff"))))
 '(ido-indicator ((t (:foreground "#ffffff"))))
 '(ido-only-match ((t (:foreground "#ffcc33"))))
 '(ido-subdir ((t (:foreground "#66ff00"))))
 '(iedit-occurrence ((t (:background "SystemHilight"))))
 '(show-paren-match ((t (:foreground "#ecf0f1" :weight bold))))
 '(web-mode-current-element-highlight-face ((t (:background "black"))))
 '(web-mode-html-attr-name-face ((t (:foreground "royal blue"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "dim gray"))))
 '(web-mode-html-tag-face ((t (:foreground "DarkOrange1")))))
