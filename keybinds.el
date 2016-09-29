(global-set-key [f12] (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(require 'projectile)

(defun find-project-file (file)
  (find-file (expand-file-name file (projectile-project-root))))

(global-set-key [f9]
                (lambda () (interactive)
                  (find-project-file "dev/user.clj")))

(global-set-key [f10]
                (lambda () (interactive)
		  (find-project-file "project.clj")))
