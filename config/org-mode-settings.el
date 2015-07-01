(eval-after-load "org"
	'(require 'ox-md nil t))

(setq
 org-directory (expand-file-name "~/orgs")
 org-default-notes-file (concat org-directory "/notes.org")
 org-agenda-files `(,org-directory))

;; support shift-selection-mode
(setq org-support-shift-select t)

;; make ctrl-a and ctrl-e behave specially in headlines (eg. move to beginning of text instead of first *)
(setq org-special-ctrl-a/e t)

(setq org-special-ctrl-k t)

;; for date selection start on Mondays
(setq calendar-week-start-day 1)

;; (setq org-agenda-custom-commands
;;       `(;; match those tagged with :inbox:, are not scheduled, are not DONE.
;;         ("p" "[p]ersonal inbox" tags "+inbox+personal")
;;         ("w" "[w]ork inbox" tags "+inbox+work")
;;         ("n" "Find a TAGged note" tags "" ((org-agenda-archives-mode t)))))


(setq org-capture-templates
      `(("t" "Task"
         entry (file+olp ,org-default-notes-file "Inbox" "Personal")
         "* TODO %?\n\n")
        ("w" "Work task"
         entry (file+olp ,org-default-notes-file "Inbox" "Work")
         "* TODO %? :work:\n\n")
        ("n" "Note"
         entry (file+headline ,org-default-notes-file "Notes")
         "* %?\n\n  %i\n")
        ("b" "Bookmark"
         entry (file+headline ,(expand-file-name "bookmarks.org" org-directory) "Bookmarks")
         "* %?\n\n  %c%i\n")
        ("s" "Scratch"
         entry (file+headline ,(expand-file-name "scratch.org" org-directory) "Scratch")
         "* Scratch it %U\n%i\n   #+begin_src text\n%?\n   #+end_src\n")))

;; define the sorting order
(setq org-agenda-sorting-strategy '((agenda habit-down time-up todo-state-down)))

;; use S-<up> and S-<down> to modify timestamps
(setq org-edit-timestamp-down-means-later t)

;; ensure that we check all boxes before marking parent as DONE
(setq org-enforce-todo-checkbox-dependencies t)

;; activate org-todo and org-archive-* for headlines in region
(setq org-loop-over-headlines-in-active-region t)

(setq org-log-done 'time) ; log completed items. adds timestamp and lets org-agenda mark it
(setq org-todo-keywords
      '((sequence "TODO" "INPROGRESS" "DONE")))
(setq org-todo-keyword-faces
      '(("INPROGRESS" . (:foreground "blue" :weight bold)))) ; add inprogress keyword

(setq org-confirm-babel-evaluate nil ; stop asking if you are sure you want to evaluate a src block
      org-src-fontify-natively t ; inside src block use the colors like the major mode of the src type
      org-src-tab-acts-natively t ; inside a src block let tab act like it was in major mode of the src type
      )

(require 'ob)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)))

;; make it possible to embed clojure code in org documents with org-babel(setq org-confirm-babel-evaluate nil ; stop asking if you are sure you want to evaluate a src block
(require 'ob)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)))

;; make it possible to embed clojure code in org documents with org-babel
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure
  '((:results . "silent") (:tangle . "yes")))

(defun org-babel-execute:clojure (body params)
  (lisp-eval-string body)
  "Done!")

(provide 'org-mode-settings)
