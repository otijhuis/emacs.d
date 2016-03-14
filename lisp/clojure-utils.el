(require 'f)
(require 'ivy)
(autoload 'magit-call-git
  "magit" "magit package" nil)

(defvar clj-utils-projects-dir "/Users/okke/Projects/clojure"
  "Directory containing the Clojure projects")

(defvar clj-utils-lein-templates '(app compojure)
  "Leiningen templates to select from")

(defun clj-utils-find-projects (directory)
  "Return the leiningen projects in the provided directory. Not recursive"
  (mapcar (lambda (dir)
            (cons (f-filename dir) dir))
          (f-directories directory
                         (lambda (dir)
                           (f-exists? (f-join dir "project.clj")))
                         nil)))

(defun clj-utils-create-project (directory &optional name template)
  "Create new leiningen project and git repo"
  (let ((name (or name
                  (read-string "Project name: ")))
        (template (or template
                      (ivy-read "Lein template: "
                                clj-utils-lein-templates))))
    (shell-command (format "cd %s && lein new %s %s" directory template name))
    (magit-call-git "init" (f-join directory name))))

(defun clj-utils-switch-project ()
  "Open Clojure project. Create new project if it doesn't exist yet"
  (interactive)
  (ivy-read
   "Clojure project: "
   (clj-utils-find-projects clj-utils-projects-dir)
   :sort t
   :action (lambda (name)
             (if name
                 (find-file (f-join name "project.clj"))
               (let ((project-name ivy--current))
                 (clj-utils-create-project clj-utils-projects-dir project-name)
                 (find-file (f-join clj-utils-projects-dir project-name "project.clj"))))))
  nil)

(provide 'clojure-utils)
