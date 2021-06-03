;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time () 
  (message "Emacs loaded in %s with %d garbage collections." (format "%.2f seconds" (float-time
										     (time-subtract
										      after-init-time
										      before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 180)
(defvar efs/default-variable-font-size 180)
;; Make frame transparency overridable
(defvar efs/frame-transparency '(90 . 90))

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-message t)

(if (display-graphic-p) 
    (progn (scroll-bar-mode -1)		; Disable visible scrollbar
	   (tool-bar-mode -1)		; Disable the toolbar
	   (tooltip-mode -1)		; Disable tooltips
	   (set-fringe-mode 10)))	; Give some breathing room

(menu-bar-mode -1)

(setq ring-bell-function 'ignore)

;; Set up the visible bell
(setq visible-bell t)

;;(set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

					; Determine the underlying operating system
(setq os-aquamacs 
      (featurep 'aquamacs))
(setq os-linux 
      (featurep 'x))
(setq os-win32 (not (or os-aquamacs 
			os-linux)))

(setq emacs-config "~/.emacs")

;; windows - windows-nt, ms-dos
;; linux - gnu, gnu/linux
;; macos - darwin
;; cygwin - cygwin
(if (or (eq system-type 'windows-nt) 
	(eq system-type 'ms-dos)) 
    (setq emacs-config "W:\misc\.emacs.el"))

(when os-win32)

(defun load-emacs-config () 
  (interactive) 
  (find-file emacs-config))
(define-key global-map "\eC" 'load-emacs-config)

(defun quit-emacs () 
  (interactive) 
  (kill-emacs))
(define-key global-map "\eq" 'quit-emacs)

(defun toggle-fullscreen () 
  (interactive) 
  (toggle-frame-fullscreen))
(define-key global-map "\e1" 'toggle-fullscreen)

;; Don't create backup files
(setq make-backup-files nil)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/") 
			 ("org" . "https://orgmode.org/elpa/") 
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package) 
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Auto update packages
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook)) 
  (add-hook mode (lambda () 
		   (display-line-numbers-mode 0))))

(use-package 
  command-log-mode)

(use-package 
  ivy
  :diminish 
  :bind (("C-s" . swiper) :map ivy-minibuffer-map ("TAB" . ivy-alt-done) 
	 ("C-l" . ivy-alt-done) 
	 ("C-j" . ivy-next-line) 
	 ("C-k" . ivy-previous-line) 
	 :map ivy-switch-buffer-map ("C-k" . ivy-previous-line) 
	 ("C-l" . ivy-done) 
	 ("C-d" . ivy-switch-buffer-kill) 
	 :map ivy-reverse-i-search-map ("C-k" . ivy-previous-line) 
	 ("C-d" . ivy-reverse-i-search-kill)) 
  :config (ivy-mode 1))

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts

(use-package 
  all-the-icons)

(use-package 
  doom-modeline 
  :init (doom-modeline-mode 1) 
  :custom ((doom-modeline-height 15)))

(use-package 
  doom-themes 
  :init (load-theme 'doom-gruvbox t))

(use-package 
  rainbow-delimiters 
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package 
  which-key 
  :init (which-key-mode) 
  :diminish which-key-mode 
  :config (setq which-key-idle-delay 1))

(use-package 
  ivy-rich 
  :init (ivy-rich-mode 1))

(use-package 
  counsel 
  :bind (("M-x" . counsel-M-x) 
	 ("C-x b" . counsel-ibuffer) 
	 ("C-x C-f" . counsel-find-file) 
	 :map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history)))

(use-package 
  helpful 
  :custom (counsel-describe-function-function #'helpful-callable) 
  (counsel-describe-variable-function #'helpful-variable) 
  :bind ([remap describe-function] . counsel-describe-function) 
  ([remap describe-command] . helpful-command) 
  ([remap describe-variable] . counsel-describe-variable) 
  ([remap describe-key] . helpful-key))

(use-package 
  general 
  :config (general-create-definer rune/leader-keys 
	    :keymaps '(normal insert visual emacs) 
	    :prefix "SPC" 
	    :global-prefix "C-SPC") 
  (rune/leader-keys "t" 
    '(:ignore t 
	      :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package 
  evil 
  :init (setq evil-want-integration t) 
  (setq evil-want-keybinding nil) 
  (setq evil-want-C-u-scroll t) 
  (setq evil-want-C-i-jump nil) 
  :config (evil-mode 1) 
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) 
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line) 
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line) 
  (evil-set-initial-state 'messages-buffer-mode 'normal) 
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package 
  evil-collection 
  :after evil 
  :config (evil-collection-init))

(use-package 
  hydra)

(defhydra hydra-text-scale 
  (:timeout 4)
  "scale text" ("j" text-scale-increase "in") 
  ("k" text-scale-decrease "out") 
  ("f" nil "finished" 
   :exit t))

(rune/leader-keys "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package 
  projectile 
  :diminish projectile-mode 
  :config (projectile-mode) 
  :custom ((projectile-completion-system 'ivy)) 
  :bind-keymap ("C-c p" . projectile-command-map) 
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code") 
    (setq projectile-project-search-path '("~/Projects/Code"))) 
  (setq projectile-switch-project-action #'projectile-dired))

(use-package 
  counsel-projectile 
  :config (counsel-projectile-mode))

(use-package 
  magit 
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; TODO
;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
;;(use-package forge)

;; IDE Confugation
;; LSP

;;(use-package breadcrumb)

(defun efs/lsp-mode-setup () 
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)) 
  (lsp-headerline-breadcrumb-mode))

(use-package 
  lsp-mode 
  :commands (lsp lsp-deferred) 
  :hook (lsp-mode . efs/lsp-mode-setup) 
  :hook ((c-mode c++-mode json-mode python-mode tex-mode typescript-mode xml-mode go-mode) . lsp) 
  :init (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
  :config (lsp-enable-which-key-integration t))

(use-package 
  lsp-ui 
  :hook (lsp-mode . lsp-ui-mode) 
  :custom (lsp-ui-doc-position 'bottom))

(use-package 
  lsp-treemacs 
  :after lsp)

(use-package 
  lsp-ivy)

(defun format-buffer () 
  (interactive) 
  (lsp-format-buffer))
(define-key global-map "\eF" 'format-buffer)

;; DAP Mode
(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

;; Company

(use-package 
  company 
  :after lsp-mode 
  :ensure t 
  :hook (lsp-mode . company-mode) 
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection)) 
  (:map lsp-mode-map 
	("<tab>" . company-indent-or-complete-common)) 
  :custom (company-minimum-prefix-length 1) 
  (company-idle-delay 0.0))

(use-package 
  company-box 
  :after company 
  :hook (company-mode . company-box-mode))

(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company '(push 'company-c-headers company-backends))

;; Languages
;; Elisp
(use-package 
  elisp-format)

;; C/C++
(use-package 
  cc-mode)
(use-package 
  ccls 
  :after projectile
  ;;:ensure-system-package ccls
  :custom (ccls-args nil) 
  (ccls-executable (executable-find "ccls")) 
  (projectile-project-root-files-top-down-recurring (append '("compile_commands.json" ".ccls")
							    projectile-project-root-files-top-down-recurring)) 
  :config (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(use-package 
  google-c-style 
  :hook (((c-mode c++-mode) . google-set-c-style) 
	 (c-mode-common . google-make-newline-indent)))

;; TypeScript
(use-package 
  prettier-js
  :delight 
  :custom (prettier-js-args '("--print-width" "100" "--single-quote" "true" "--trailing-comma"
			      "all")))

(use-package 
  typescript-mode 
  :mode ("\\.ts\\'" "\\.tsx\\'") 
  :hook (typescript-mode . lsp-deferred) 
  :hook (typescript-mode . prettier-js-mode) 
  :custom (add-hook 'typescript-mode-hook #'(lambda () 
					      (enable-minor-mode '("\\.tsx?\\'" .
								   prettier-js-mode)))) 
  :config (setq typescript-indent-level 2))

(use-package 
  evil-nerd-commenter 
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package 
  rainbow-delimiters 
  :hook (prog-mode . rainbow-delimiters-mode))
