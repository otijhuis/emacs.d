;; Ivy / Swiper / Counsel
(with-eval-after-load "ivy"
  (require 'flx)
  (setq ivy-format-function 'ivy-format-function-line)

  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))

  ;;(setq ivy-initial-inputs-alist nil)

  (setq ivy-extra-directories nil)
  (setq ivy-fixed-height-minibuffer t)

  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  ;;(setq ivy-count-format "(%d/%d) ")
  (setq ivy-count-format "")
  (setq ivy-flx-limit 1000)

  (setq ivy-wrap t))

(ivy-mode 1)

(with-eval-after-load "counsel"
  (setq counsel-find-file-at-point t)

  (setq counsel-find-file-ignore-regexp
        (concat
         ;; file names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))

  ;; Redefine `counsel-ag-base-command' with my required options, especially
  ;; the `--follow' option to allow search through symbolic links.
  (setq counsel-ag-base-command (concat "ag "
                                        "--noheading "
                                        "--nogroup "
                                        "--nocolor "
                                        "--skip-vcs-ignores "
                                        "--smart-case "
                                        "--follow " ; follow symlinks
                                        "%S")))

(provide 'ivy-settings)
