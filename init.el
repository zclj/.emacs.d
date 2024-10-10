;; ----------------------------------------
;; Performance
;; ----------------------------------------
;; By increasing the garbage collection threshold, we reduce GC pauses
;; during heavy operations, leading to smoother performance.
(setq gc-cons-threshold #x40000000)

;; Set the maximum output size for reading process output, allowing for larger data transfers.
(setq read-process-output-max (* 1024 1024 4))

;-----------
; Packages
;-----------
(require 'package)

(add-to-list 'package-archives
	      '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(setopt package-install-upgrade-built-in t)

(defvar my-packages
  '(
    ;; Clojure
    cider
    clj-refactor
    helm-cider
    clojure-snippets

    ;; Rust
    rust-mode
    rustic
    
    ;; Linting
    flycheck
    flycheck-clj-kondo

    ;; Web
    web-mode

    ;; Docker
    dockerfile-mode

    ;; yaml
    yaml-mode
    
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
    exec-path-from-shell
    adoc-mode
    crux
    guru-mode
    key-chord
    undo-tree
    browse-kill-ring
    anzu
    string-inflection

    use-package
    
    ;; Theme
    solarized-theme
    dracula-theme))

(when (not package-archive-contents)
  (package-refresh-contents))

;; Automaticaly install any missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Define a global var to control if Nerd Fonts are used or not
(defcustom ek-use-nerd-fonts t
  "Configuration for using Nerd Fonts Symbols."
  :type 'boolean
  :group 'appearance)

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
;; Odin
;;----------------------------------------
(add-to-list 'load-path "~/.emacs.d/odin-mode")
(require 'odin-mode)

(use-package eglot
  :ensure t
  :config (add-to-list 'eglot-server-programs '(odin-mode . ("ols")))
  :hook ((odin-mode . eglot-ensure))
  )

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
;; Disable for now, works like crap
;; (require 'adoc-mode)
;; (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
;; (add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))

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
  (defspec 'defun)
  (for-all 'defun)
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
; Flymake
;----------------
;; https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html#Top
(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

;---------------
; Rust
;----------------
;;;; Docs
;; https://github.com/brotzeit/rustic
;; https://github.com/joaotavora/eglot

(require 'rust-mode)

;; The Rust style guide recommends spaces rather than tabs for indentation
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

;; M-? - find-references
;; imenu
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map              
              ;;("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c r" . eglot-rename)
              ;;("C-c C-t" . rustic-cargo-current-test)
              )
  :config
  ;; use eglot instead of lsp-mode (which is default)
  (setq rustic-lsp-client 'eglot)
  
  ;; comment to disable rustfmt on save
  ;;(setq rustic-format-on-save t)

  ;; don't use inlay-hints
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))

  ;; eglot speed?
  (setq eglot-sync-connect 0)
  (setq eglot-events-buffer-size 0)
  (fset #'jsonrpc--log-event #'ignore)
  (add-hook 'focus-out-hook 'garbage-collect)
  )

;---------------
; auto-complete
;----------------
(global-company-mode)

;; turn on company-flx for fuzzy search
;; (with-eval-after-load 'company
;;   (company-flx-mode +1))

;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
;; (global-set-key [M-tab] 'company-complete)

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

;;--------------
;; Visual
;;--------------
(if is-mac
    (add-to-list 'default-frame-alist
		 '(font . "CaskaydiaMono Nerd Font 16"))
    ;; (add-to-list 'default-frame-alist
    ;;     	 '(font . "Menlo 16"))
    (add-to-list 'default-frame-alist
		 '(font . "DejaVu Sans Mono-14")))

;; Use nerd icons if defined
(use-package nerd-icons
  ;; Load the package only if the user has configured to use nerd fonts.
  :if ek-use-nerd-fonts
  ;; Ensure the package is installed.
  :ensure t
  ;; Load the package only when needed to improve startup time.
  :defer t)


(blink-cursor-mode -1)

;; don't want startup message
(setq inhibit-startup-message t)

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

;; ----------------------------------------
;; Doom mode line
;; ----------------------------------------
(use-package doom-modeline
  :ensure t
  :defer t
  :custom
  ;; Set the buffer file name style to just the buffer name (without path).
  (doom-modeline-buffer-file-name-style 'buffer-name)
  ;; Enable project detection for displaying the project name.
  (doom-modeline-project-detection 'project)
  ;; Show the buffer name in the mode line.
  (doom-modeline-buffer-name t)
  ;; Limit the version control system (VCS) branch name length to 25 characters.
  (doom-modeline-vcs-max-length 25)
  :config
  ;; Check if nerd fonts are being used.
  (if ek-use-nerd-fonts
      ;; Enable icons in the mode line if nerd fonts are used.
      (setq doom-modeline-icon t)
    ;; Disable icons if nerd fonts are not being used.
    (setq doom-modeline-icon nil))
  :hook
  (after-init . doom-modeline-mode))

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
; Yaml
;------------
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

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

;; ----------------------------------------
;; undo-tree
;; ----------------------------------------
;; (require 'undo-tree)
;; (global-undo-tree-mode)
(use-package undo-tree
  :defer t
  :ensure t
  :hook
  (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; Increase undo limits to avoid losing history due to Emacs' garbage collection.
        ;; These values can be adjusted based on your needs.
        ;; 10X bump of the undo limits to avoid issues with premature
        ;; Emacs GC which truncates the undo history very aggressively.
        undo-limit 800000                     ;; Limit for undo entries.
        undo-strong-limit 12000000            ;; Strong limit for undo entries.
        undo-outer-limit 120000000)           ;; Outer limit for undo entries.
  :config
  ;; Set the directory where `undo-tree' will save its history files.
  ;; This keeps undo history across sessions, stored in a cache directory.
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo"))))



;; anzu-mode enhances isearch & query-replace by showing total matches and current match position
(require 'anzu)
(global-anzu-mode)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
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
