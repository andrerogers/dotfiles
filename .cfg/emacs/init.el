;; Set up Emacs Package Manager
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; Let Emacs know that it can apply custom
;; code in a separate file
(setq custom-file "~/.emacs.d/custom-file.el")
;; Make sure custom code added by
;; package management is executed
(load-file custom-file)

;; Stop Emacs from losing undo information
;; by setting the limit very high
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

; Determine the underlying operating system
(setq os-aquamacs (featurep 'aquamacs))
(setq os-linux (featurep 'x))
(setq os-win32 (not (or os-aquamacs os-linux)))

(setq compilation-directory-locked nil)
(setq shift-select-mode nil)
(setq enable-local-variables nil)

;; emacs config file
(setq emacs-config "w:/misc/init.el")

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

(defun start-server ()
  (interactive)
  (server-start)
  (message "Emacs Server has started..."))
(define-key global-map "\eS" 'start-server)

;; Don't create backup files
(setq make-backup-files nil)

;; -------------------------------

;; REVIEW
;; Path & Environment
(setenv  "PATH" (concat

                 "C:\\Windows\\System32" ";" 

                 "C:\\Windows\\Microsoft.NET\\Framework\\v4.0.30319" ";"

                 ;; Unix tools 
		 "C:\\Program Files\\Git\\usr\\bin" ";"

                 "C:\\coreutils-5.3.0\\bin" ";"

                 "C:\\diffutils-2.8.7-1\\bin" ";"

                 (getenv "PATH")
         ))

;; -------------------------------

;; Interface
(setq-default
 ;; Suppress startup screen
 inhibit-startup-message t

 ;; Do not open a file or or user dialog box
 use-file-dialog nil
 use-dialog-box nil

 ;; Use Unix's \n (LF - Line Feed) and UTF instead
 ;; of Windows \r\n (CRLF - Carriage Return Line Feed)
 buffer-file-encoding-system 'utf-8-unix)

;; UTF-8 everywhere
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Setting fullscreen or maximized
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . fullboth)))))

;; Disable bell
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

;; Disabling unneccessary GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Smooth scroll
(setq scroll-step 3)

;; Clock
(display-time)

;; Highlight current line
(global-hl-line-mode t)
(set-face-background 'hl-line "midnight-blue")

;; Show line numbers
(global-linum-mode t)

;; Automatically update buffers if file content
;; on the disk has changed
(global-auto-revert-mode t)

;; Set theme
(load-theme 'material t)

;; --------------------------------

;; Completion Engine
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-grid-mode 1)
(ido-mode t)

;; --------------------------------

;; Search
(global-set-key (kbd "C-s") 'swiper)

;; --------------------------------

;; Window Management
;; ace-window
(setq aw-dispatch-always t)
(setq aw-background nil)
(ace-window-display-mode 1)
(global-set-key (kbd "M-o") 'ace-window)

;; resizing windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; --------------------------------

;; Org Mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; EVIL Mode
;; vim layer for Emacs
;; to allow for scrolling with C - u in vim mode
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

;; evil-commentary
(evil-commentary-mode)

;; evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; evil-magit
(require 'evil-magit)
(setq evil-magit-state 'normal)
;; optional: disable additional bindings for yanking text
;; (setq evil-magit-use-y-for-yank nil)

;; --------------------------------

;; REVIEW
;; Terminal
;; (require 'vterm)

;; (defun run-bash ()
;;       (interactive)
;;       (let ((shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe"))
;; 	(shell "*bash*"))
;;       )

;; (defun run-cmd ()
;;       (interactive)
;;       (let ((shell-file-name "cmd.exe"))
;; 	(shell "*cmd.exe*"))
;;       )

;; (defun run-wsl ()
;;       (interactive)
;;       (let ((shell-file-name "wsl.exe"))
;; 	(shell "*wsl.exe*"))
;;       )

;; --------------------------------

;; Project Management
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; --------------------------------

;; Buffer Management
; Setup my find-files
(define-key global-map "\ef" 'find-file)
(define-key global-map "\eF" 'find-file-other-window)

(global-set-key (read-kbd-macro "\eb")  'ido-switch-buffer)
(global-set-key (read-kbd-macro "\eB")  'ido-switch-buffer-other-window)

;; --------------------------------

;; DAP Mode
(require 'dap-go)

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

(setq dap-auto-configure-features '(sessions locals controls tooltip))

(dap-mode 1)
(dap-ui-mode 1)


;; --------------------------------

;; LSP Mode
(setq lsp-keymap-prefix "C-c l")
(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

(require 'lsp-mode)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'go-mode-hook #'lsp)
(add-hook 'json-mode-hook #'lsp-mode)

;; Company-LSP (LSP dependency)
(require 'company-lsp)
(push 'company-lsp company-backends)

;; Yasnippet (LSP dependency)
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; Develop in ~/emacs.d/mysnippets, but also
;; try out snippets in ~/Downloads/interesting-snippets
(setq yas-snippet-dirs '("~/emacs.d/mysnippets"
                           "~/Downloads/interesting-snippets"))

;; --------------------------------

;; Compilation
(require 'compile)

(when os-win32 
  (setq project-makescript "build.bat")
)

(setq project-todo "todo.org")

(setq compilation-context-lines 0)
(setq compilation-error-regexp-alist
    (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
     compilation-error-regexp-alist))

(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p project-makescript) t
      (cd "../")
      (find-project-directory-recursive)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
  (cd find-project-from-directory)
  (find-project-directory-recursive)
  (setq last-compilation-directory default-directory)))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile project-makescript))
  (other-window 1))
(define-key global-map "\em" 'make-without-asking)

;; Type F9 to call M-x compile
;; Type Ctrl + F9 to call M-x recompile - running the compilation command again.
;; (global-set-key (kbd "<f9>") #'compile)

;; (global-set-key (kbd "<C-f9>")
;;                 (lambda () (interactive)
;;                   (save-buffer)
;;                   (recompile)                
;;                   ))

;; --------------------------------

;; C Hook
(require 'cc-mode)

(define-key global-map [f9] 'first-error)
(define-key global-map [f10] 'previous-error)
(define-key global-map [f11] 'next-error)

; C++ indentation style
(defconst c-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
    "C++ Style")

;; Does stuff for C, C++
(defun c-hook ()
    ;; Set my style for the current buffer
    (c-add-style "C++-Style" c-style t)

    ;; 4-space tabs
    (setq tab-width 4
	    indent-tabs-mode nil)

    ;; Additional style stuff
    (c-set-offset 'member-init-intro '++)

    ;; No hungry backspace
    (c-toggle-auto-hungry-state -1)

    ;; Newline indents, semi-colon doesn't
    (define-key c++-mode-map "\C-m" 'newline-and-indent)
    (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))

    ;; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
    (setq dabbrev-case-replace t)
    (setq dabbrev-case-fold-search t)
    (setq dabbrev-upcase-means-case-search t)

    ;; Abbrevation expansion
    (abbrev-mode 1)

    (defun header-format ()
	"Format the given file as a header file."
	(interactive)
	(setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
	(insert "#if !defined(")
	(push-mark)
	(insert BaseFileName)
	(upcase-region (mark) (point))
	(pop-mark)
	(insert "_H)\n")
	(insert "/* ========================================================================\n")
	(insert "   $File: $\n")
	(insert "   $Date: $\n")
	(insert "   $Revision: $\n")
	(insert "   $Creator: Andre Rogers $\n")
	(insert "   $Notice: (C) Copyright 2020 by Dre Codes, Inc. All Rights Reserved. $\n")
	(insert "   ======================================================================== */\n")
	(insert "\n")
	(insert "#define ")
	(push-mark)
	(insert BaseFileName)
	(upcase-region (mark) (point))
	(pop-mark)
	(insert "_H\n")
	(insert "#endif")
    )

    (defun source-format ()
	"Format the given file as a source file."
	(interactive)
	(setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
	(insert "/* ========================================================================\n")
	(insert "   $File: $\n")
	(insert "   $Date: $\n")
	(insert "   $Revision: $\n")
	(insert "   $Creator: Andre Rogers $\n")
	(insert "   $Notice: (C) Copyright 2020 by Dre Codes, Inc. All Rights Reserved. $\n")
	(insert "   ======================================================================== */\n")
    )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]hin" buffer-file-name) (source-format))
        ((string-match "[.]cin" buffer-file-name) (source-format))
        ((string-match "[.]h" buffer-file-name) (header-format))
        ((string-match "[.]cpp" buffer-file-name) (source-format)))

  (defun find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (setq CorrespondingFileName nil)
    (setq BaseFileName (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.h" buffer-file-name)
       (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
	   (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
    (if (string-match "\\.hin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".cin")))
    (if (string-match "\\.cin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".hin")))
    (if (string-match "\\.cpp" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if CorrespondingFileName (find-file CorrespondingFileName)
       (error "Unable to find a corresponding file")))
  (defun find-corresponding-file-other-window ()
    "Find the file that corresponds to this one."
    (interactive)
    (find-file-other-window buffer-file-name)
    (casey-find-corresponding-file)
    (other-window -1))
  (define-key c++-mode-map [f12] 'find-corresponding-file)
  (define-key c++-mode-map [M-f12] 'find-corresponding-file-other-window)

  (define-key c++-mode-map "\t" 'dabbrev-expand)
  (define-key c++-mode-map [S-tab] 'indent-for-tab-command)
  (define-key c++-mode-map "\C-y" 'indent-for-tab-command)
  (define-key c++-mode-map [C-tab] 'indent-region)

  ; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'vc-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(vc-devenv
   "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
    2 3 nil (4)))
)

(add-hook 'c-mode-common-hook 'c-hook)

;; --------------------------------

;; Go Hook

(add-hook 'before-save-hook #'gofmt-before-save)

;; --------------------------------

;; Project Specific
(defun find-project-todo-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p project-todo) t
      (cd "../")
      (find-project-directory-recursive)))


(defun find-project-todo-directory ()
  "Find the project todo's directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (cd find-project-from-directory)
  (find-project-todo-directory-recursive))

(defun load-todo ()
  "Open the project's todo file."
  (interactive)
  (if (find-project-directory) (find-file project-todo))
  (other-window 1))
(define-key global-map "\et" 'load-todo)

(setq project-log-file "w:/Lockdown/log.txt")
(defun insert-timeofday ()
   (interactive "*")
   (insert (format-time-string "---------------- %a, %d %b %y: %I:%M%p")))
(defun load-log ()
  (interactive)
  (find-file project-log-file)
  (if (boundp 'longlines-mode) ()
    (longlines-mode 1)
    (longlines-show-hard-newlines))
  (if (equal longlines-mode t) ()
    (longlines-mode 1)
    (longlines-show-hard-newlines))
  (end-of-buffer)
  (newline-and-indent)
  (insert-timeofday)
  (newline-and-indent)
  (newline-and-indent)
  (end-of-buffer)
)
(define-key global-map "\eT" 'load-log)

;; Bright-red TODOs
 (setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
 (make-face 'font-lock-fixme-face)
 (make-face 'font-lock-study-face)
 (make-face 'font-lock-important-face)
 (make-face 'font-lock-note-face)
 (mapc (lambda (mode)
	 (font-lock-add-keywords
	  mode
	  '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
	    ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
	    ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
            ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
	fixme-modes)
 (modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
 (modify-face 'font-lock-study-face "Yellow" nil nil t nil t nil nil)
 (modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
 (modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)
