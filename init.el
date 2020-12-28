;-----------
; Packages
;-----------
(require 'package)

(add-to-list 'package-archives
	      '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(defvar my-packages
  '(
    ;; Clojure
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

    ;; diff
    diff-hl
    
    ;; Misc
    magit
    move-text
    buffer-move
    expand-region
    smart-mode-line
    smart-mode-line-powerline-theme
    exec-path-from-shell
    adoc-mode
    crux
    guru-mode
    key-chord
    undo-tree
    browse-kill-ring
    
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

;; Load custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Seems Emacs 27.1 do not open files in ~/ by default (in MacOS anyway)
(when (string= default-directory "/")
  (setq default-directory "~/")
  (with-current-buffer "*Messages*"
    (setq default-directory "~/")))

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
;; AsciiDoc
;;----------------------------------------
(require 'adoc-mode)
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
(add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))

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

;;----------------------------------------
;; Docs
;;----------------------------------------
(which-key-mode)

;;----------------------------------------
;; ace window
;;----------------------------------------

;; Trying to use default 'other-window' binding
(global-set-key (kbd "C-x o") 'ace-window)

;; set to home row instead of default 0-9
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;----------------------------------------
;; Clojure
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
(if is-mac
    (add-to-list 'default-frame-alist
		 '(font . "Menlo 16"))
    (add-to-list 'default-frame-alist
		 '(font . "DejaVu Sans Mono-14")))

(blink-cursor-mode -1)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

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
(sml/apply-theme 'powerline)


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

(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

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

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

;------------
; Projectile
;------------
(projectile-global-mode)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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

;;---------------------------
;; Guru mode
;;---------------------------
(guru-global-mode +1)

;;---------------------------

;; diff-hl
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;;---------------------------
;; key-chords
;;---------------------------
(require 'key-chord)

(key-chord-define-global "jj" 'avy-goto-word-1)
(key-chord-define-global "jl" 'avy-goto-line)
(key-chord-define-global "jk" 'avy-goto-char)
(key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-define-global "xx" 'execute-extended-command)
(key-chord-define-global "yy" 'browse-kill-ring)

(key-chord-mode +1)
