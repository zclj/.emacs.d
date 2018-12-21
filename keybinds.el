;; key bindings for MacOS
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

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

;; using shift+arrow to move between windows
(windmove-default-keybindings)

;; Swap current buffer with buffer in direction of arrow
(global-set-key (kbd "<M-s-right>") 'buf-move-right)
(global-set-key (kbd "<M-s-left>") 'buf-move-left)
(global-set-key (kbd "<M-s-up>") 'buf-move-up)
(global-set-key (kbd "<M-s-down>") 'buf-move-down)

;; hide/show
(global-set-key (kbd "C-.") 'hs-toggle-hiding)
(global-set-key (kbd "C-,") 'hs-hide-all)
(global-set-key (kbd "C-x C-,") 'hs-show-all)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Revert without any fuss
(global-set-key (kbd "M-<escape>") (lambda (revert-buffer t t)))
