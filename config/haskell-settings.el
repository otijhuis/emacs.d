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

(let ((haskell-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat
                  haskell-path path-separator
                  "/usr/local/bin" path-separator
                  "/Users/okke/.stack/programs/x86_64-osx/ghc-7.10.3/bin" path-separator
                  (getenv "PATH")))
  (add-to-list 'exec-path haskell-path))

(setq haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-type 'stack-ghci
      haskell-indent-spaces 2
      haskell-process-path-ghci "stack"
      haskell-stylish-on-save t
      ;; haskell-enable-ghci-ng-support t
      haskell-tags-on-save t
      haskell-process-log t)

;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(add-hook 'haskell-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (append '((company-capf company-dabbrev-code))
                         company-backends))))

(add-hook 'haskell-mode-hook 'structured-haskell-mode)

;; don't turn this on! will mess with key-seq keyboard shortcuts
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)

;; (add-to-list 'company-backends 'company-ghc)
;; (setq company-ghc-show-info t)

;; (add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook 'turn-on-hi2)

(provide 'haskell-settings)
