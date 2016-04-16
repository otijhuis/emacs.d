;; Use stack to install the following:
;; ghc-mod
;; happy
;; hindent
;; hasktags
;; stylish-haskell
;; present
;; hlint
;; hoogle
;; structured-haskell-mode
;; ghci-ng

(let ((my-cabal-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-type 'stack-ghci)
 '(haskell-tags-on-save t)
 '(haskell-process-log t))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; (add-to-list 'company-backends 'company-ghc)
;; (custom-set-variables '(company-ghc-show-info t))

;; (add-hook 'haskell-mode-hook #'hindent-mode)

(provide 'haskell-settings)
