(require 'f)
(require 's)
(require 'ivy)
(require 'cl-lib)

(defvar clj-projects-dir "/Users/okke/Projects/clojure"
  "Directory containing the Clojure projects")

(defun list-clj-projects ()
  (mapcar
   (lambda (s)
     (cons (car (last (f-split (f-dirname s)))) s))
   (cl-remove-if 's-blank?
                 (s-lines
                  (shell-command-to-string
                   (format "find %s -depth 2 -name \"project.clj\" -print0 | xargs -0 ls -t"
                           clj-projects-dir))))))

(defun create-clj-project (&optional name template)
  (interactive)
  (let ((name (or name
                  (read-string "Project name: ")))
        (template (or template
                      (ivy-read "Lein template: "
                                '(app compojure)))))
    (shell-command (format "cd %s && lein new %s %s" clj-projects-dir template name))
    (magit-call-git "init" (f-join clj-projects-dir name))))

(defun switch-clj-project ()
  "Open Clojure project. Create new project if it doesn't exist yet."
  (interactive)
  (ivy-read
   "Clojure project: "
   (list-clj-projects)
   :action (lambda (name)
             (if name
                 (find-file name)
               (create-clj-project ivy-text)))))

(switch-clj-project)
