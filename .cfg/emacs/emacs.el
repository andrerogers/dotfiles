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

(setq device (getenv "PLAYGROUND_DEVICE"))

;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 100)
;; (defvar efs/default-variable-font-size 180)
(if (eq device "hackbox") 
    ((defvar 
       runemacs/default-font-size
       140)))

;; Make frame transparency overridable
(defvar efs/frame-transparency '(98. 98))

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

(setq inhibit-startup-message t)

(if (or (display-graphic-p) 
	(daemonp)) 
    (progn (scroll-bar-mode -1)		; Disable visible scrollbar
	   (tool-bar-mode -1)		; Disable the toolbar
	   (tooltip-mode -1)		; Disable tooltips
	   (set-fringe-mode 10)))	; Give some breathing room

(menu-bar-mode -1)

(setq ring-bell-function 'ignore)

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil 
		    :font "Fira Code Retina" 
		    :height runemacs/default-font-size)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

					; Determine the underlying operating system
(setq os-aquamacs 
      (featurep 'aquamacs))
(setq os-linux 
      (featurep 'x))
(setq os-win32 (not (or os-aquamacs 
			os-linux)))

(setq emacs-config "~/.emacs.el")

;; windows - windows-nt, ms-dos
;; linux - gnu, gnu/linux
;; macos - darwin
;; cygwin - cygwin
(if (or (eq system-type 'windows-nt) 
	(eq system-type 'ms-dos)) 
    (setq emacs-config "W:\misc\.emacs.el"))

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

;; Window Resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

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
(use-package 
  auto-package-update 
  :custom (auto-package-update-interval 7) 
  (auto-package-update-prompt-before-update t) 
  (auto-package-update-hide-results t) 
  :config (auto-package-update-maybe) 
  (auto-package-update-at-time "09:00"))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook)) 
  (add-hook mode (lambda () 
		   (display-line-numbers-mode 0))))


(use-package 
  no-littering)

					; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

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
  all-the-icons 
  :if (display-graphic-p))

(use-package 
  doom-modeline 
  :init (doom-modeline-mode 1) 
  :custom ((doom-modeline-height 15)))

(use-package 
  doom-themes 
  :init (load-theme 'doom-tomorrow-night t))

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
  :init (setq ivy-rich-mode 1))

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

(setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

(global-set-key "\eS" 'counsel-rg)

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

;; (use-package breadcrumb)

(defun efs/lsp-mode-setup () 
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)) 
  (lsp-headerline-breadcrumb-mode))

(use-package 
  lsp-mode 
  :commands (lsp lsp-deferred) 
  :hook (lsp-mode . efs/lsp-mode-setup) 
  :hook ((c-mode c++-mode json-mode python-mode tex-mode typescript-mode xml-mode go-mode
		 rust-mode-hook rustic-mode-hook) . lsp) 
  :init (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
  :config (lsp-enable-which-key-integration t) 
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy") 
  (lsp-eldoc-render-all t) 
  (lsp-idle-delay 0.6) 
  (lsp-rust-analyzer-server-display-inlay-hints t) 
  :config (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package 
  lsp-ui 

  :ensure 
  :commands lsp-ui-mode 
  :custom (lsp-ui-peek-always-show t) 
  (lsp-ui-sideline-show-hover t) 
  (lsp-ui-doc-enable nil) 
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

(use-package 
  exec-path-from-shell 

  :ensure 
  :init (exec-path-from-shell-initialize))

;; DAP Mode
(use-package 
  dap-mode 
  :commands dap-debug 
  :custom (lsp-enable-dap-auto-configure nil) 
  :config (dap-ui-mode) 
  (dap-tooltip-mode 1) 
  (tooltip-mode 1) 
  (dap-ui-controls-mode 1) 
  (require 'dap-cpptools) 
  (require 'dap-node) 
  (require 'dap-lldb) 
  (require 'dap-gdb-lldb) 
  (require 'dap-dlv-go) 
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (dap-go-setup) 
  (dap-gdb-lldb-setup) 
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed
  (dap-register-debug-template "Rust::LLDB Run Configuration" (list :type "lldb" 
								    :request "launch" 
								    :name "LLDB::Run" 
								    :gdbpath "rust-lldb" 
								    :target nil 
								    :cwd nil)) 
  (dap-register-debug-template "Node Inspector::Attach" (list: :type "node" 
							       :request "attach" 
							       :port: 9229 
							       :name "Node Inspector::Attach")) 
  (general-define-key :keymaps 'lsp-mode-map 
		      :prefix lsp-keymap-prefix 
		      "d" '(dap-hydra t 
				      :wk "debugger")))


;; (require 'dap-lldb)
;; (require 'dap-chrome)
;; (dap-chrome-setup
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
(eval-after-load 'company '(append '((company-c-headers company-solidity company-capf
							company-dabbrev-code)) company-backends))

;; Languages
(use-package 
  flycheck 
  :ensure t 
  :init (global-flycheck-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "/usr/bin/multimarkdown"))

(use-package 
  flymake-shellcheck 
  :commands flymake-shellcheck-load 
  :init (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;; Elisp
(use-package 
  elisp-format)

;; Go - lsp-mode
(defun lsp-go-install-save-hooks () 
  (add-hook 'before-save-hook #'lsp-format-buffer t t) 
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package 
  go-mode 
  :config
  ;; Start LSP Mode and YASnippet mode
  (add-hook 'go-mode-hook 'lsp-deferred) 
  (add-hook 'go-mode-hook (lambda () 
			    (add-hook 'before-save-hook 'lsp-go-install-save-hooks))))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3")))

;; C/C++
(use-package 
  cc-mode)

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
  rustic
  :ensure 
  :bind (:map rustic-mode-map
	      ("M-j" . lsp-ui-imenu) 
	      ("M-?" . lsp-find-references) 
	      ("C-c C-c l" . flycheck-list-errors) 
	      ("C-c C-c a" . lsp-execute-code-action) 
	      ("C-c C-c r" . lsp-rename) 
	      ("C-c C-c q" . lsp-workspace-restart) 
	      ("C-c C-c Q" . lsp-workspace-shutdown) 
	      ("C-c C-c s" . lsp-rust-analyzer-status)) 
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t) 
  (add-hook 'rustic-mode-hook 'ar/rustic-mode-hook))

(defun ar/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name (setq-local buffer-save-without-query t)))

(use-package 
  typescript-mode 
  :mode ("\\.ts\\'" "\\.tsx\\'" "\\.js\\'") 
  :hook (typescript-mode . lsp-deferred) 
  :hook (typescript-mode . prettier-js-mode) 
  :custom (add-hook 'typescript-mode-hook #'(lambda () 
					      (enable-minor-mode '("\\.tsx?\\'" .
								   prettier-js-mode)))) 
  :config (setq typescript-indent-level 2))

;; Solidity
(use-package 
  solidity-mode 
  :config (setq solidity-comment-style 'slash) 
  :config (setq solidity-solium-path '/usr/bin/solium) 
  :config (setq solidity-solc-path '/usr/bin/solcjs))
;; TODO: need to add a conditional here to set a path for windows

(use-package 
  evil-nerd-commenter 
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package 
  rainbow-delimiters 
  :hook (prog-mode . rainbow-delimiters-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(markdown-preview-mode rustic rust-mode dap-mode which-key use-package typescript-mode solidity-mode rainbow-delimiters prettier-js no-littering magit lsp-ui lsp-treemacs lsp-ivy ivy-rich helpful google-c-style general flymake-shellcheck flycheck evil-nerd-commenter evil-collection elisp-format doom-themes doom-modeline counsel-projectile company-box command-log-mode ccls auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
