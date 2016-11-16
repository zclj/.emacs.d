;-----------
; Packages
; ----------
(require 'package)

(add-to-list 'package-archives
              '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(defvar my-packages
  '(;; Clojure
    cider
    clj-refactor
    clojure-cheatsheet
    helm-cider
    cider-eval-sexp-fu
    clojure-snippets
    
    ;; Navigation
    helm
    avy
    ace-window

    ;; Project navigation
    projectile
    helm-projectile
    
    ;; List
    rainbow-delimiters
    paredit

    ;; Doc
    which-key
    
    ;; Auto complete
    company
    company-flx

    ;; Misc
    magit
    move-text
    expand-region))

(when (not package-archive-contents)
  (package-refresh-contents))

;; Automaticaly install any missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Load key bindings.
(load (concat user-emacs-directory "keybinds.el"))

;;----------------------------------------
;; expand region - form aware selection
;;----------------------------------------
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

;;----------------------------------------
;; move text
;; It allows you to move the current line using M-up / M-down
;; if a region is marked, it will move the region instead.
;;----------------------------------------

(require 'move-text)
(move-text-default-bindings)

;;----------------------------------------
;; Docs
;;----------------------------------------
(which-key-mode)

;;----------------------------------------
;; ace window
;;----------------------------------------
(global-set-key (kbd "M-p") 'ace-window)
;; set to home row instead of default 0-9
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;----------------------------------------
;; avy
;;----------------------------------------
;; TODO : provar så här..
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
;;(global-set-key (kbd "M-p") 'avy-goto-char-2)
;; (global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
;; (global-set-key (kbd "s-w") 'ace-window)


;; TODO - REMOVE if avy works out
;;----------------------------------------
;; ace jump mode
;;----------------------------------------
;; ace jump mode major function
;; 
;(add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")
;; (autoload
;;   'ace-jump-mode
;;   "ace-jump-mode"
;;   "Emacs quick move minor mode"
;;   t)

;; you can select the key you prefer to
;;(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;; 
;; enable a more powerful jump back function from ace jump mode
;;
;; (autoload
;;   'ace-jump-mode-pop-mark
;;   "ace-jump-mode"
;;   "Ace jump back:-)"
;;   t)
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))
;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;----------------------------------------
;; Workgroups2
;;----------------------------------------
;(require 'workgroups2)

;;(setq wg-session-load-on-start t)    ; default: (not (daemonp))

;; Change prefix key (before activating WG)
;(setq wg-prefix-key (kbd "C-c z"))

;; Change workgroups session file
;(setq wg-session-file "~/.emacs.d/.emacs_workgroups")

;; Set your own keyboard shortcuts to reload/save/switch WGs:
;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
;(global-set-key (kbd "<pause>")     'wg-reload-session)
;(global-set-key (kbd "C-S-<pause>") 'wg-save-session)
;(global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
;(global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)

;(workgroups-mode 1)

;;----------------------------------------
;; Magit
;;----------------------------------------
(global-set-key (kbd "C-x g") 'magit-status)

;;----------------------------------------
;; Clojure
;;----------------------------------------
(require 'cider)
(require 'clj-refactor)

(defun clojure-mode-hook ()
  (paredit-mode)
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(require 'cider-eval-sexp-fu)

;; Eldoc displays function signatures in the minibuffer as you're typing. 
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'clojure-mode-hook)

;;----------------
;; squiggly-clojure / flycheck
;;----------------
;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (eval-after-load 'flycheck
;;   '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;;----------------
;; helm
;;----------------
(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key (kbd "M-x") 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;; helm cider
(helm-cider-mode 1)

;; helm projectile
(require 'helm-projectile)
(helm-projectile-on)

;---------------
; auto-complete
;----------------
; new auto-complete
(global-company-mode)

;; turn on company-flx for fuzzy search
(with-eval-after-load 'company
  (company-flx-mode +1))

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(global-set-key [M-tab] 'company-complete)

;---------------
; Zenburn-theme
;---------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;;--------------
;; Visual
;;--------------
; Set font
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-12"))

;------------
; Misc stuff
;------------

;; turn off tool bar
(tool-bar-mode -1)

;; using shift+arrow to move between windows
(windmove-default-keybindings)

(add-hook `text-mode-hook 'turn-on-visual-line-mode)

(setq initial-scratch-message nil)

(projectile-global-mode)

;--------
; Octave
;--------
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;----------------
; Org-mode
;----------------
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

;;---------------------------
;; Global Clipboard support
;;---------------------------
(global-set-key "\C-w" 'clipboard-kill-region) ;; \C-w
(global-set-key "\M-w" 'clipboard-kill-ring-save) ;; \M-w
(global-set-key "\C-y" 'clipboard-yank) ;;\C-y

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("52d43c63ce3d612053fc9f98a13ecd60fbe116095a41c1b90c76556519044f19" default)))
 '(initial-buffer-choice t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;---------------------------
;; Common place for Backup files
;;---------------------------
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )
