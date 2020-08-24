;-----------
; Packages
;-----------
(require 'package)
 
(add-to-list 'package-archives
              '("melpa" . "https://melpa.org/packages/"))
 
(package-initialize)
 
(defvar my-packages
  '(;; Clojure

    ;; New test Clojure
    ;;clojure-mode
    ;;inf-clojure
    
    ;; Old Clojure
    cider
    clj-refactor
    ;;clojure-cheatsheet
    helm-cider
    clojure-snippets

    ;; Haskell
    haskell-mode
    
    ;; Linting
    flycheck
    flycheck-clj-kondo
 
    ;; Web
    web-mode

    ;; Docker
    dockerfile-mode
    
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

    ;; Document formats
    markdown-mode

    ;; Modeling
    plantuml-mode

    ;; statistics
    ess
    
    ;; Misc
    magit
    move-text
    buffer-move
    expand-region
    smart-mode-line
    exec-path-from-shell

    ;; Theme
    solarized-theme
    dracula-theme))
 
(when (not package-archive-contents)
  (package-refresh-contents))

;; Automaticaly install any missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
 
;; Load key bindings.
(load (concat user-emacs-directory "keybinds.el"))

;;----------------------------------------
;; Proxy
;;----------------------------------------

;; Load proxy settings - note, file is local and not in VC
(when (file-readable-p "proxy.el")
  (load (concat user-emacs-directory "proxy.el")))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;;----------------------------------------
;; PlantUML
;;----------------------------------------
;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

;; path to plantuml .jar
(setq plantuml-jar-path "~/bin/plantuml.jar")

(setq plantuml-exec-mode 'jar)
(setq plantuml-output-type 'png)

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
;; Markdown
;;----------------------------------------
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "52d43c63ce3d612053fc9f98a13ecd60fbe116095a41c1b90c76556519044f19" default)))
 '(initial-buffer-choice t)
 '(markdown-command "/usr/local/bin/pandoc")
 '(package-selected-packages
   (quote
    (php-mode docker-compose-mode solarized-theme web-mode zenburn-theme which-key rainbow-delimiters move-text magit helm-projectile helm-cider expand-region company-flx clojure-snippets clojure-cheatsheet clj-refactor cider-eval-sexp-fu ace-window))))

;;----------------------------------------
;; Docs
;;----------------------------------------
(which-key-mode)
 
;;----------------------------------------
;; ace window
;;----------------------------------------
;;(global-set-key (kbd "M-p") 'ace-window)

;; Trying to use default 'other-window' binding
(global-set-key (kbd "C-x o") 'ace-window)

;; set to home row instead of default 0-9
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
 
;;----------------------------------------
;; avy
;;----------------------------------------
;; TODO : provar så här..
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-;") 'avy-goto-char-2)
;;(global-set-key (kbd "M-p") 'avy-goto-char-2)
;; (global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
;; (global-set-key (kbd "s-w") 'ace-window)
 
  
;;----------------------------------------
;; Magit
;;----------------------------------------
(global-set-key (kbd "C-x g") 'magit-status)
 
;;----------------------------------------
;; React
;; https://code-and-cocktails.herokuapp.com/blog/2016/01/10/emacs-setup-for-react-slash-reactnative/
;;----------------------------------------
;;(add-to-list 'auto-mode-alist '(".*/react/.*\\.js[x]?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)
 
(setq js-indent-level 2)
 
(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

;;----------------------------------------
;; Clojure - Test new
;;----------------------------------------
;; (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
;; (add-hook 'clojure-mode-hook 'paredit-mode)

;; (add-hook 'clojure-mode-hook #'eldoc-mode)
;; (add-hook 'inf-clojure-mode-hook #'eldoc-mode)

;; (setf inf-clojure-tools-deps-cmd '("localhost" . 5555))


;;----------------------------------------
;; Clojure - OLD
;;----------------------------------------
(require 'cider)
(require 'clj-refactor)
;;(require 'cider-eval-sexp-fu)

(require 'flycheck-clj-kondo)

(defun clojure-mode-hook ()
  (paredit-mode)
  (flycheck-mode)
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))
  
;;Eldoc displays function signatures in the minibuffer as you're typing.
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

;;(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'clojure-mode-hook)

;;helm cider
(helm-cider-mode 1)

(define-clojure-indent
  (render 'defun)
  (query 'defun)
  (dom/div 'defun)
  (dom/select 'defun)
  (dom/table 'defun)
  (dom/tr 'defun)
  (dom/thead 'defun))

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
(global-set-key (kbd "C-x b") #'helm-buffers-list)

(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
  
;; helm projectile
(require 'helm-projectile)
(helm-projectile-on)

;---------------
; Haskell
;----------------
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))


;---------------
; auto-complete
;----------------
(global-company-mode)
 
;; turn on company-flx for fuzzy search
(with-eval-after-load 'company
  (company-flx-mode +1))
 
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(global-set-key [M-tab] 'company-complete)
 
;;--------------
;; Visual
;;--------------
; Set font
;; (add-to-list 'default-frame-alist
;;              '(font . "DejaVu Sans Mono-14"))

(if is-mac
    (add-to-list 'default-frame-alist
		 '(font . "Menlo 16"))
    (add-to-list 'default-frame-alist
		 '(font . "DejaVu Sans Mono-14")))

;; theme
;;(load-theme 'solarized-dark t)
(load-theme 'dracula t)

;; Highlight paren pairs
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; use line numbers globaly
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; turn of sound and blink warnings
(setq ring-bell-function 'ignore)
(setq visible-bell 1)
 
;; turn of bars
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
  
(add-hook `text-mode-hook 'turn-on-visual-line-mode)

;; smart mode line
(sml/setup)


;; hideshow blocks
(load-library "hideshow")
(setq hs-hide-comments t)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; rainbow parens when programming
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; if two files with same name is open show dir instead of <n>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;------------
; Misc stuff
;------------

;; Setup environment variables from the user's shell.
(when is-mac
  (exec-path-from-shell-initialize))

;; use aspell instead of ispell
(setq ispell-program-name "aspell")

;; the scratch buffer starts empty
(setq initial-scratch-message nil)

;; save location of point for next time the a file is opened
(save-place-mode 1)

;; save backup files in .emacs.d
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;------------
; Projectile
;------------
(projectile-global-mode)
 
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
 
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

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)
   (dot . t)))

;; org - plantuml
(setq org-plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))

;;---------------------------
;; Global Clipboard support
;;---------------------------
(global-set-key "\C-w" 'clipboard-kill-region) ;; \C-w
(global-set-key "\M-w" 'clipboard-kill-ring-save) ;; \M-w
(global-set-key "\C-y" 'clipboard-yank) ;;\C-y
 

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
