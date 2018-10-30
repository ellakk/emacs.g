;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

;;; Bootstrap

;;;; Optimization

(progn
  (defvar kalle-after-emacs-load-hook '())
  (run-with-idle-timer 0 nil (lambda () (run-hooks 'kalle-after-emacs-load-hook)))

  (defvar kalle-before-first-cmd-hook '())
  (defun kalle/run-before-first-cmd-hook ()
    (run-hooks 'kalle-before-first-cmd-hook)
    (remove-hook 'pre-command-hook 'kalle/run-before-first-cmd-hook))
  (add-hook 'pre-command-hook 'kalle/run-before-first-cmd-hook)

  (defvar file-name-handler-alist-old file-name-handler-alist)

  ;; these are reset at the end of this file
  (setq file-name-handler-alist nil
        gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

;;;; Startup

(progn
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-scratch-message "")

  (setq load-prefer-newer t))

;;;; Early birds

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (setq use-package-verbose nil
        use-package-compute-statistics nil
        use-package-enable-imenu-support t)
  (require 'use-package))

(use-package auto-compile
  :hook (kalle-after-emacs-load . kalle/load-auto-compile)
  :preface
  (defun kalle/load-auto-compile ()
    (require 'auto-compile))
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package no-littering
  :config
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Emacs settings

;;;; Editor

(progn
  ;; Only use utf-8
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8)

  ;; Enable some disabled commands
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Enable y/n answers
  (fset 'yes-or-no-p 'y-or-n-p)

  (setq-default
   switch-to-visible-buffer nil            ; Dont switch to already visible buffer
   hscroll-margin 2                        ; Smooth Scrolling
   hscroll-step 1                          ; ^
   scroll-conservatively 1001              ; ^
   scroll-margin 0                         ; ^
   scroll-preserve-screen-position t       ; ^
   comint-prompt-read-only t               ; Undeletable prompt
   enable-recursive-minibuffers t          ; Enable recursive minibuffers
   history-length 1000                     ; Keep a longer history list
   save-interprogram-paste-before-kill t   ; Save each kill to clipboard
   create-lockfiles nil)                   ; Dont create lockfiles
  )

;;;; Editing

(progn
  (setq-default
   require-final-newline t                ; All files get a final newline
   sentence-end-double-space nil          ; Double space does not end sentences
   indent-tabs-mode nil                   ; New tabs are forbidden
   tab-width 4                            ; Tab appearance
   fill-column 80                         ; Maximum line width
   set-mark-command-repeat-pop t)         ; C-SPC pops mark after C-u C-SPC
  )

;;;; Visual

(progn
  (setq-default
   echo-keystrokes 0.02                   ; Echo keystrokes faster
   frame-title-format "%b (%f)"           ; Filename in frame title
   image-animate-loop t                   ; Loop images forever
   inhibit-startup-message t              ; Dont show startup message
   blink-matching-paren nil               ; Dont blink when creating matching parens
   mode-line-default-help-echo nil        ; Disable mode-line mouseovers
   ring-bell-function #'ignore            ; Disable the bell
   use-dialog-box nil                     ; Only use minibuffer for prompts
   truncate-lines t)                      ; Do not display continuation lines
  )

;; Disable some bars
(menu-bar-mode -1)
(tooltip-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; Enable column number in modeline
(column-number-mode +1)

;;; Functions

;;;; Buffers

(defun kalle/kill-buffer-maby-window (&optional arg)
  "Kill buffers and if universal arg is 4 then also kill the current window.
If the univeral arg is 16 then only kill the window."
  (interactive "P")
  (when (or (equal '(4) arg) (equal nil arg))
    (call-interactively 'kill-this-buffer))
  (when (or (equal '(4) arg) (equal '(16) arg))
    (delete-window)))

(defun kalle/switch-to-scratch ()
  "Switch to the scratch buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun kalle/switch-to-messages ()
  "Switch to the message buffer"
  (interactive)
  (switch-to-buffer "*Messages*"))

;;;; Editing

(defun kalle/4-next-lines ()
  "Move 4 lines down."
  (interactive)
  (next-line '4))

(defun kalle/4-previous-lines ()
  "Move 4 lines down."
  (interactive)
  (previous-line '4))

;;;; Files

;;;; Misc

;;;; Windows

(defun kalle/delete-window-maby-buffer (&optional arg)
  "Delete the current window. If the universal prefix argument is
used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

;;; Keybindings

;;;; Regular

(bind-keys
 ("C-x 0" . kalle/delete-window-maby-buffer)
 ("M-n" . kalle/4-next-lines)
 ("M-p" . kalle/4-previous-lines)
 ("M-SPC" . cycle-spacing)
 ("M-o" . other-window))

(bind-keys*
 ("M-k" . kalle/kill-buffer-maby-window)
 ("M-u" . universal-argument)
 :map universal-argument-map
 ("M-u" . universal-argument-more))

;;;; Leader

(bind-key "M-m" nil)

(bind-keys
 ;; Top-level
 :prefix-map kalle-map
 :prefix "M-m"

 ;; (B)uffers
 ("bm" . kalle/switch-to-messages)
 ("bs" . kalle/switch-to-scratch)
 ("bd" . kalle/kill-buffer-maby-window)

 ;; (F)iles
 ("fc" . set-buffer-file-coding-system)

 ;; (L)ines
 ("ld" . delete-duplicate-lines)
 ("lk" . keep-lines)
 ("lm" . delete-matching-lines)
 ("lM" . delete-non-matching-lines)
 ("ls" . sort-lines)

 ;; (Q)uit
 ("qf" . delete-frame)
 ("qq" . save-buffers-kill-terminal)

 ;; (S)earch

 ;; (W)indows
 ("wd" . kalle/delete-window-maby-buffer)
 ("wm" . delete-other-windows)
 ("ws" . split-window-below)
 ("wv" . split-window-horizontally)
 )

(global-set-key (kbd "M-m r") ctl-x-r-map)
(global-set-key (kbd "M-m h") help-map)
(global-set-key (kbd "M-m n") narrow-map)
(global-set-key (kbd "M-m k") kmacro-keymap)

(define-key key-translation-map (kbd "M-m f s") (kbd "C-x C-s"))
(define-key key-translation-map (kbd "M-m f S") (kbd "C-x s"))
(define-key key-translation-map (kbd "M-m b r") (kbd "C-x C-q"))

;;; Packages

;;;; Completion

(use-package company
  :defer 0.5
  :bind
  ((:map company-active-map)
   ("C-j" . company-complete-selection))
  :init
  (setq company-minimum-prefix-length 2
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-idle-delay 0.2)
  :config
  (global-company-mode)
  (company-tng-configure-default))

(use-package company-statistics
  :hook (company-mode . +company-statistics/start)
  :preface
  (defun +company-statistics/start ()
    (require 'shut-up)
    (shut-up
      (company-statistics-mode))))

(use-package counsel
  :bind
  (("C-x rb" . counsel-bookmark)
   ("M-i" . kalle/counsel-jump-in-buffer)
   ("M-y" . counsel-yank-pop)
   (:map kalle-map)
   ("M-m" . counsel-M-x)
   ("fd" . counsel-fzf)
   ("ff" . counsel-find-file)
   ("fl" . counsel-locate)
   ("fr" . counsel-recentf)
   ("sd" . kalle/rg-current-directory)
   ("sD" . kalle/rg-select-directory))
  :preface
  (defun kalle/rg-current-directory ()
    "Search current DIR with RG"
    (interactive)
    (counsel-rg "" default-directory nil (concat default-directory)))

  (defun kalle/rg-select-directory ()
    "Prompt for DIR to search with RG"
    (interactive)
    (let ((dir (read-directory-name "Directory:")))
      (counsel-rg "" dir nil (concat dir))))

  (defun kalle/counsel-jump-in-buffer ()
    "Jump in buffer with `counsel-imenu' or `counsel-org-goto' if in org-mode"
    (interactive)
    (call-interactively
     (cond
      ((eq major-mode 'org-mode) 'counsel-org-goto)
      (t 'counsel-imenu))))
  :init
  (setq counsel-yank-pop-separator "\n----\n")
  (define-key minibuffer-local-map (kbd "C-r")
    'counsel-minibuffer-history))

(use-package hippie-exp
  :bind
  (("M-/" . hippie-expand)
   ("M-ö" . hippie-expand))
  :init
  (setq-default
   hippie-expand-try-functions-list
   '(
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol)))

(use-package ivy
  :hook (kalle-before-first-cmd . ivy-mode)
  :bind
  ((:map ivy-minibuffer-map)
   ("M-j" . ivy-avy)
   (:map kalle-map)
   ("TAB" . kalle/switch-to-previous-buffer)
   ("bb" . ivy-switch-buffer))
  :preface
  (defvar kalle-filter-allowed-majors
    '(dired-mode magit-status-mode lisp-interaction-mode messages-buffer-mode))

  (defun kalle/buffer-filter (buffer-name)
    "Returns nil if `buffer-name' is to be shown in ivy buffer list"
    (let* ((buffer (get-buffer buffer-name))
           (major-mode (buffer-local-value 'major-mode buffer))
           (keep nil))
      (when (member major-mode kalle-filter-allowed-majors) (setq keep t))
      (when (buffer-file-name buffer) (setq keep t))
      (eq keep nil)))

  (defun kalle/switch-to-previous-buffer ()
    "Switch to prevous buffer based on buffers shown in `ivy-switch-buffer'"
    (interactive)
    (let ((ivy-buffers (ivy--switch-buffer-matcher "" (ivy--buffer-list ""))))
      (if (> (length ivy-buffers) 1)
          (switch-to-buffer (nth 1 ivy-buffers))
        (switch-to-buffer (other-buffer (current-buffer) 1)))))
  :init
  (setq ivy-fixed-height-minibuffer t
        ivy-on-del-error-function nil
        ivy-format-function 'ivy-format-function-arrow
        ivy-height 15
        ivy-initial-inputs-alist nil
        projectile-completion-system 'ivy
        magit-completing-read-function 'ivy-completing-read
        ivy-magic-slash-non-match-action nil
        smex-completion-method 'ivy
        ivy-use-ignore-default 'always)
  :config
  (add-to-list 'ivy-ignore-buffers #'kalle/buffer-filter)
  (require 'ivy-hydra)
  (define-key ivy-minibuffer-map (kbd "C-l") (kbd "DEL")))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode))

(use-package ivy-yasnippet
  :bind
  ((:map kalle-map)
   ("is" . ivy-yasnippet))
  :init
  (setq ivy-yasnippet-expand-keys nil))

(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (smex-initialize))

(use-package yasnippet
  :commands (yas-hippie-try-expand)
  :init
  (setq yas-verbosity 0
        yas-also-auto-indent-first-line t
        yas-prompt-functions '(yas-completing-prompt yas-ido-prompt yas-no-prompt)
        yas-minor-mode-map nil)
  (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;;;; Files/History/Bookmarks

(use-package autorevert
  :hook (kalle-after-emacs-load . global-auto-revert-mode)
  :init
  (setq-default
   global-auto-revert-non-file-buffers t
   auto-revert-verbose nil))

(use-package dired
  :defer t
  :init
  (setq-default
   dired-recursive-deletes 'always
   dired-recursive-copies 'always
   dired-dwim-target t
   dired-listing-switches "-aBhl  --group-directories-first"))

(use-package dired+
  :hook (dired-mode . kalle/load-dired+)
  :preface
  (defun kalle/load-dired+ ()
    (require 'dired+))
  :init
  (setq diredp-hide-details-initially-flag nil
        font-lock-maximum-decoration (quote ((dired-mode . 1) (t . t))))
  :config
  (toggle-diredp-find-file-reuse-dir t)
  (global-dired-hide-details-mode 1))

(use-package dired-x
  :hook (dired-mode . kalle/load-dired-x)
  :bind
  ((:map kalle-map)
   ("f J" . dired-jump-other-window)
   ("f j" . dired-jump))
  :preface
  (defun kalle/load-dired-x ()
    (require 'dired-x)))

(use-package ediff
  :defer t
  :init
  (setq-default
   ediff-split-window-function 'split-window-horizontally
   ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package recentf
  :hook (kalle-after-emacs-load . kalle/recentf)
  :preface
  (defun kalle/recentf ()
    (unless recentf-mode
      (require 'shut-up)
      (shut-up
        (recentf-mode)
        (recentf-track-opened-file))))
  :init
  (setq-default recentf-max-saved-items 100
                recentf-auto-cleanup 'never)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

(use-package savehist
  :hook (kalle-after-emacs-load . savehist-mode)
  :init
  (setq-default
   savehist-additional-variables '(search-ring regexp-search-ring extended-command-history)
   savehist-autosave-interval 60))

(use-package saveplace
  :config
  (save-place-mode))

(use-package treemacs
  :commands (treemacs-current-visibility)
  :bind
  ((:map kalle-map)
   ("f t" . treemacs)
   ("p t" . treemacs-projectile))
  :preface
  (defvar treemacs-use-collapsed-directories (if (executable-find "python") 3 0))

  (defvar treemacs-use-git-mode
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t) 'extended)
      (`(t . _) 'simple)))
  :init
  (setq treemacs-follow-after-init t
        treemacs-width 35
        treemacs-position 'left
        treemacs-is-never-other-window nil
        treemacs-silent-refresh nil
        treemacs-indentation 2
        treemacs-change-root-without-asking nil
        treemacs-sorting 'alphabetic-desc
        treemacs-show-hidden-files t
        treemacs-never-persist nil
        treemacs-goto-tag-strategy 'refetch-index
        treemacs-collapse-dirs treemacs-use-collapsed-directories)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

;;;; Help

(use-package google-translate
  :bind
  ((:map kalle-map)
   ("h t" . google-translate-smooth-translate))
  :init
  (setq google-translate-translation-directions-alist
        '(("en" . "sv") ("sv" . "en") ("de" . "sv") ("fr" . "sv")))
  :config
  (require 'google-translate-smooth-ui))

(use-package helpful
  :bind
  ((:map kalle-map)
   ("h ." . helpful-at-point)
   ("h f" . helpful-callable)
   ("h k" . helpful-key)
   ("h v" . helpful-variable)))

(use-package which-key
  :hook (kalle-after-emacs-load . which-key-mode)
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-compute-remaps t)
  :config
  (which-key-add-key-based-replacements
    "M-m b" "Buffers"
    "M-m br" "read-only-mode"
    "M-m f" "Files"
    "M-m f e" "Edit"
    "M-m e" "Errors/Linting"
    "M-m fs" "save-buffer"
    "M-m fS" "save-some-buffers"
    "M-m g" "Git"
    "M-m h" "Help"
    "M-m i" "Insert"
    "M-m k" "KMacros"
    "M-m l" "Lines/Workspace"
    "M-m n" "Narrow"
    "M-m p" "Projects"
    "M-m ps" "Search"
    "M-m q" "Quit"
    "M-m r" "Rectangle/Register"
    "M-m S" "Spelling"
    "M-m s" "Search"
    "M-m w" "Windows"
    "M-m z" "Zoom"))

;;;; Misc

(use-package hydra
  :bind
  ((:map kalle-map)
   ("w r" . hydra-window-size/body))
  :config
  (defhydra hydra-window-size (:color red)
    "Windows size"
    ("h" shrink-window-horizontally "shrink horizontal")
    ("j" shrink-window "shrink vertical")
    ("k" enlarge-window "enlarge vertical")
    ("l" enlarge-window-horizontally "enlarge horizontal")))

;;;; Projects

(use-package counsel-projectile
  :bind
  ((:map kalle-map)
   ("p b" . counsel-projectile-switch-to-buffer)
   ("p d" . counsel-projectile-find-dir)
   ("p f" . counsel-projectile-find-file)
   ("p g" . counsel-projectile-find-file-dwim)
   ("p O c" . counsel-projectile-org-capture)
   ("p p" . counsel-projectile-switch-project)
   ("p s g" . counsel-projectile-grep)
   ("p s r" . counsel-projectile-rg)
   ("p s s" . counsel-projectile-ag)
   ("p SPC" . counsel-projectile)
   ("s P" . counsel-projectile-git-grep)
   ("s p" . counsel-projectile-rg)))

(use-package projectile
  :commands (projectile-project-root
             projectile-project-p)
  :hook (kalle-after-emacs-load . projectile-mode)
  :bind
  ((:map kalle-map)
   ("p !" . projectile-run-shell-command-in-root)
   ("p &" . projectile-run-async-shell-command-in-root)
   ("p a" . projectile-find-other-file)
   ("p C" . projectile-configure-project)
   ("p c" . projectile-compile-project)
   ("p D" . projectile-dired)
   ("p e" . projectile-recentf)
   ("p E" . projectile-edit-dir-locals)
   ("p F" . projectile-find-file-in-known-projects)
   ("p i" . projectile-invalidate-cache)
   ("p I" . projectile-ibuffer)
   ("p j" . projectile-find-tag)
   ("p k" . projectile-kill-buffers)
   ("p l" . projectile-find-file-in-directory)
   ("p m" . projectile-commander)
   ("p o" . projectile-multi-occur)
   ("p q" . projectile-switch-open-project)
   ("p P" . projectile-test-project)
   ("p r" . projectile-replace)
   ("p R" . projectile-regenerate-tags)
   ("p S" . projectile-save-project-buffers)
   ("p C-t" . projectile-toggle-between-implementation-and-test)
   ("p T" . projectile-find-test-file)
   ("p u" . projectile-run-project)
   ("p v" . projectile-vc)
   ("p V" . projectile-browse-dirty-projects)
   ("p z" . projectile-cache-current-file)
   ("p ESC" . projectile-project-buffers-other-buffer))
  :init
  (setq projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")))

;;;; Shell

(use-package compile
  :defer t
  :init
  (setq-default compilation-always-kill t
                compilation-scroll-output 'first-error))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package shell-pop
  :commands (shell-pop kalle/projectile-shell-pop)
  :bind
  (("M-'" . kalle/shell-pop-no-cwd)
   (:map kalle-map)
   ("'" . kalle/shell-pop-full-screen-no-cwd)
   ("p '" . kalle/projectile-shell-pop))
  :preface
  (defun kalle/projectile-shell-pop ()
    "Open a term buffer at projectile project root."
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (call-interactively 'shell-pop)))

  (defun kalle/shell-pop-no-cwd ()
    (interactive)
    (let ((shell-pop-autocd-to-working-dir nil))
      (call-interactively 'shell-pop)))

  (defun kalle/shell-pop-full-screen-no-cwd ()
    (interactive)
    (let ((shell-pop-autocd-to-working-dir nil)
          (shell-pop-window-position 'full))
      (call-interactively 'shell-pop)))
  :init
  (setq shell-pop-shell-type '("ansi-term" "*shell-pop-term*" (lambda nil (ansi-term shell-pop-term-shell)))
        shell-pop-restore-window-configuration nil
        shell-pop-window-size 50
        shell-pop-full-span t))

(use-package term
  :commands term
  :preface
  (defun kalle/expose-global-binding-in-term (binding)
    (define-key term-raw-map binding
      (lookup-key (current-global-map) binding)))
  :config
  (kalle/expose-global-binding-in-term (kbd "M-'"))
  (kalle/expose-global-binding-in-term (kbd "M-o"))
  (kalle/expose-global-binding-in-term (kbd "M-m"))
  (define-key term-raw-map (kbd "C-S-v") 'term-paste)
  (define-key term-raw-map (kbd "C-c C-y") 'term-paste))

(use-package xterm-color
  :defer t
  :init
  (setq compilation-environment '("TERM=xterm-256color"))
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  (add-hook 'compilation-start-hook
            (lambda (proc)
              ;; We need to differentiate between compilation-mode buffers
              ;; and running as part of comint (which at this point we assume
              ;; has been configured separately for xterm-color)
              (when (eq (process-filter proc) 'compilation-filter)
                ;; This is a process associated with a compilation-mode buffer.
                ;; We may call `xterm-color-filter' before its own filter function.
                (set-process-filter
                 proc
                 (lambda (proc string)
                   (funcall 'compilation-filter proc
                            (xterm-color-filter string))))))))

;;;; Text-editing

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("M-*" . crux-top-join-line)
   ("C-<backspace>" . crux-kill-line-backwards)
   ([(shift return)] . crux-smart-open-line)
   ([(control shift return)] . crux-smart-open-line-above)
   (:map kalle-map)
   ("b C-d" . crux-kill-other-buffers)
   ("f D" . crux-delete-file-and-buffer)
   ("f E" . crux-sudo-edit)
   ("f e i" . crux-find-user-init-file)
   ("f R" . crux-rename-file-and-buffer)
   ("=" . crux-cleanup-buffer-or-region))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify))

(use-package delsel
  :hook (kalle-after-emacs-load . delete-selection-mode))

(use-package dtrt-indent
  :commands (dtrt-indent--search-hook-mapping)
  :hook (kalle-after-emacs-load . dtrt-indent-global-mode)
  :init
  (setq dtrt-indent-verbosity 0))

(use-package flycheck
  :defer 0.5
  :bind
  ((:map kalle-map)
   ("e c"   . flycheck-buffer)
   ("e C"   . flycheck-clear)
   ("e C-c" . flycheck-compile)
   ("e n"   . flycheck-next-error)
   ("e p"   . flycheck-previous-error)
   ("e l"   . flycheck-list-errors)
   ("e C-w" . flycheck-copy-errors-as-kill)
   ("e s"   . flycheck-select-checker)
   ("e ?"   . flycheck-describe-checker)
   ("e h"   . flycheck-display-error-at-point)
   ("e e"   . flycheck-explain-error-at-point)
   ("e H"   . display-local-help)
   ("e i"   . flycheck-manual)
   ("e V"   . flycheck-version)
   ("e v"   . flycheck-verify-setup)
   ("e x"   . flycheck-disable-checker))
  :init
  (setq flycheck-indication-mode 'right-fringe
        flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (global-flycheck-mode 1))

(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  :init
  (setq flycheck-popup-tip-error-prefix "✕ "))

(use-package flyspell
  :bind ((:map kalle-map)
         ("S b" . flyspell-buffer)
         ("S e" . kalle/ispell-english)
         ("S s" . kalle/ispell-swedish)
         ("S w" . flyspell-word)
         ("t S" . kalle/toggle-spelling))
  :preface
  (defun kalle/ispell-swedish ()
    (interactive)
    (ispell-change-dictionary "svenska"))

  (defun kalle/ispell-english ()
    (interactive)
    (ispell-change-dictionary "english"))

  (defun kalle/toggle-spelling ()
    (interactive)
    (if (bound-and-true-p flyspell-mode)
        (flyspell-mode -1)
      (if (derived-mode-p 'prog-mode)
          (flyspell-prog-mode)
        (flyspell-mode))))
  :init
  (setq ispell-program-name "hunspell"
        flyspell-mode-map nil))

(use-package flyspell-correct
  :bind ((:map kalle-map)
         ("S c" . flyspell-correct-wrapper))
  :config
  (require 'flyspell-correct-ivy))

(use-package format-all
  :bind
  ((:map kalle-map)
   ("e f" . format-all-buffer)))

(use-package multiple-cursors
  :bind (("M-h" . nil)
         ("M-h M-h" . mc/mark-all-dwim)
         ("M-h M-n" . mc/mark-next-like-this-symbol)
         ("M-h M-p" . mc/mark-previous-like-this-symbol)
         ("M-h A" . mc/mark-all-like-this)
         ("M-h a" . mc/mark-all-like-this-in-defun)
         ("M-h N" . mc/unmark-next-like-this)
         ("M-h n" . mc/mark-next-like-this)
         ("M-h l" . mc/edit-lines)
         ("M-h P" . mc/unmark-previous-like-this)
         ("M-h p" . mc/mark-previous-like-this)
         ("M-h S" . mc/mark-all-symbols-like-this)
         ("M-h s" . mc/mark-all-symbols-like-this-in-defun)
         ("M-h W" . mc/mark-all-words-like-this)
         ("M-h w" . mc/mark-all-words-like-this-in-defun))
  :init
  (with-eval-after-load 'multiple-cursors-core
    (require 'multiple-cursors)))

(use-package smartparens
  :defer 0.1
  :preface
  (defun +smartparens-smartparens-pair-newline (id action context)
    (save-excursion
      (newline)
      (indent-according-to-mode)))

  (defun +smartparens-pair-newline-and-indent (id action context)
    (+smartparens-smartparens-pair-newline id action context)
    (indent-according-to-mode))
  :init
  (setq sp-autoskip-closing-pair 'always
        sp-highlight-pair-overlay nil
        sp-base-key-bindings 'smartparens
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-from-inside t)
  :config
  (require 'smartparens-config)

  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode 1)
  (smartparens-global-mode 1)
  (define-key smartparens-mode-map (kbd "M-<backspace>") 'nil)
  (sp-pair "{" nil :post-handlers
           '(:add (+smartparens-pair-newline-and-indent "RET")))
  (sp-pair "[" nil :post-handlers
           '(:add (+smartparens-pair-newline-and-indent "RET"))))

(use-package undo-tree
  :hook (kalle-after-emacs-load . global-undo-tree-mode))

(use-package wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :hook (grep-mode . wgrep-setup))

(use-package wgrep-ag
  :hook (ag-mode . wgrep-ag-setup))

(use-package ws-butler
  :hook (kalle-after-emacs-load . ws-butler-global-mode))

(use-package zop-to-char
  :bind
  (("M-z" . zop-to-char)
   ("M-Z" . zop-up-to-char)))

;;;; Text-navigation

(use-package ag
  :commands (ag-project))

(use-package avy
  :bind
  (("M-j" . avy-goto-char-2)
   ("M-l" . avy-goto-line)
   ("C-M-l" . downcase-word))
  :init
  (eval-after-load "isearch"
    '(define-key isearch-mode-map (kbd "M-j") 'avy-isearch))
  (setq avy-all-windows t
        avy-background t))

(use-package goto-chg
  :bind ("M-m ;" . goto-last-change))

(use-package dumb-jump
  :bind
  ("C-M-g" . dumb-jump-go)
  :init
  (setq dumb-jump-selector 'ivy))

(use-package expand-region
  :bind
  ((:map kalle-map)
   ("v" . er/expand-region))
  :init
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"))

(use-package ivy-xref
  :commands ivy-xref-show-xrefs
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package jump-char
  :bind*
  (("M-e" . jump-char-forward)
   ("M-a" . jump-char-backward)))

(use-package smart-jump
  :bind*
  (("M-." . smart-jump-go)
   ("M-," . smart-jump-back)
   ("M-?" . smart-jump-references)
   ("M-P" . smart-jump-peek))
  :config
  (smart-jump-setup-default-registers))

(use-package subword
  :hook (kalle-after-emacs-load . global-subword-mode))

(use-package swiper
  :bind
  ((:map kalle-map)
   ("s S" . swiper-all)
   ("s s" . swiper)
   (:map swiper-map)
   ("M-j" . swiper-avy)))

;;;; Version-control

(use-package diff-hl
  :hook (after-init . global-diff-hl-mode)
  :commands (diff-hl-magit-post-refresh diff-hl-dired-mode)
  :bind
  ((:map kalle-map)
   ("gn" . diff-hl-next-hunk)
   ("gp" . diff-hl-previous-hunk)
   ("gm" . diff-hl-mark-hunk))
  :preface
  (defun kalle/setup-fringe-bitmaps ()
           "Define thin fringe bitmaps for maximum sexiness."
           (define-fringe-bitmap 'diff-hl-bmp-top [224] nil nil '(center repeated))
           (define-fringe-bitmap 'diff-hl-bmp-middle [224] nil nil '(center repeated))
           (define-fringe-bitmap 'diff-hl-bmp-bottom [224] nil nil '(center repeated))
           (define-fringe-bitmap 'diff-hl-bmp-insert [224] nil nil '(center repeated))
           (define-fringe-bitmap 'diff-hl-bmp-single [224] nil nil '(center repeated))
           (define-fringe-bitmap 'diff-hl-bmp-delete [240 224 192 128] nil nil 'top))

  (defun kalle/vc-gutter-type-at-pos (type _pos)
           "Return the bitmap for `diff-hl' to use for change at point."
           (pcase type
             (`unknown 'question-mark)
             (`delete  'diff-hl-bmp-delete)
             (`change  'diff-hl-bmp-middle)
             (`ignored 'diff-hl-bmp-i)
             (x (intern (format "diff-hl-bmp-%s" x)))))
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  :config
  (if (display-graphic-p)
      (progn
      (setq diff-hl-fringe-bmp-function #'kalle/vc-gutter-type-at-pos
            diff-hl-draw-borders nil)
      (add-hook 'diff-hl-mode-hook #'kalle/setup-fringe-bitmaps))
    (progn
        (setq diff-hl-margin-symbols-alist
              '((insert . "❙") (delete . "^") (change . "❙")
                (unknown . "❙") (ignored . "❙")))
        (diff-hl-margin-mode))))

(use-package git-timemachine
  :bind
  ((:map kalle-map)
   ("gs" . git-timemachine))
  :init
  (add-hook 'git-timemachine-mode-hook '(lambda () (diff-hl-mode -1))))

(use-package magit
  :defer t
  :bind
  ((:map kalle-map)
   ("gs" . magit-status)
   ("gd" . magit-dispatch-popup)
   ("gf" . magit-file-popup)
   ("gb" . magit-blame)
   (:map magit-file-mode-map)
   ("C-c M-g" . nil)
   ("C-x g" . nil))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

;;;; Visual

(use-package all-the-icons
  :defer t)

(use-package anzu
  :hook (kalle-after-emacs-load . global-anzu-mode)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

(use-package default-text-scale
  :bind ((:map kalle-map)
         ("z" . hydra-text-size/body))
  :config
  (defhydra hydra-text-size (:color red)
    "Windows size"
    ("+" default-text-scale-increase "Increase default face")
    ("-" default-text-scale-decrease "Decrease default face")
    ("=" default-text-scale-reset "Reset default face")))

(use-package display-line-numbers
  :when (version< "26" emacs-version)
  :hook ((text-mode conf-mode prog-mode) . display-line-numbers-mode))

(use-package doom-modeline
  :hook (emacs-startup . kalle-load-doom-modeline)
  :preface
  (defun kalle-load-doom-modeline ()
    (require 'shut-up)
    (shut-up
      (doom-modeline-init)))
  :init
  (setq doom-modeline-height 29))

(use-package doom-themes
  :config
  (if (display-graphic-p)
      (load-theme 'doom-one 't)
    (load-theme 'doom-molokai 't))
  (doom-themes-treemacs-config)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package fill-column-indicator
  :hook ((git-commit-setup . kalle/git-fill-column-indicator))
  :preface
  (defun kalle/git-fill-column-indicator ()
    (setq fill-column 72)
    (fci-mode))
  :config
  (with-eval-after-load 'company
    (add-hook 'fci-mode-hook '(lambda () (company-mode -1)))))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package uniquify
  :defer t
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package volatile-highlights
  :hook (kalle-after-emacs-load . volatile-highlights-mode)
  :config
  (vhl/define-extension 'undo-tree
                        'undo-tree-move
                        'undo-tree-yank)
  (with-eval-after-load 'undo-tree
    (vhl/install-extension 'undo-tree)
    (vhl/load-extension 'undo-tree)))

(use-package whitespace
  :defer t
  :init
  (setq whitespace-display-mappings
        '((tab-mark ?\t [?› ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark ?\ [?·] [?.]))))

;;;; Windows

(use-package ace-window
  :bind
  ((:map kalle-map)
   ("ww" . ace-select-window)
   ("wD" . ace-delete-window)
   ("wS" . ace-swap-window))
  :init
  (setq aw-keys '(?a ?f ?j ?k ?l))
  :config
  (set-face-attribute
   'aw-leading-char-face nil
   :weight 'bold
   :height 2.0))

(use-package eyebrowse
  :bind
  ((:map kalle-map)
   ("lp" . eyebrowse-prev-window-config)
   ("ln" . eyebrowse-next-window-config)
   ("l TAB" . eyebrowse-last-window-config)
   ("ld" . eyebrowse-close-window-config)
   ("l0" . eyebrowse-switch-to-window-config-0)
   ("l1" . eyebrowse-switch-to-window-config-1)
   ("l2" . eyebrowse-switch-to-window-config-2)
   ("l3" . eyebrowse-switch-to-window-config-3)
   ("l4" . eyebrowse-switch-to-window-config-4)
   ("l5" . eyebrowse-switch-to-window-config-5)
   ("l6" . eyebrowse-switch-to-window-config-6)
   ("l7" . eyebrowse-switch-to-window-config-7)
   ("l8" . eyebrowse-switch-to-window-config-8)
   ("l9" . eyebrowse-switch-to-window-config-9))
  :init
  (setq eyebrowse-wrap-around nil)
  :config
  (eyebrowse-mode))

(use-package popwin
  :after window-purpose
  :config
  ;; Use our own settings
  (setq popwin:special-display-config nil)

  (push '("^\\*helpful.+\\*$"      :regexp t   :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*Process List*"         :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
  (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '(" *undo-tree*"           :dedicated t :position right  :stick t :noselect nil :width   60) popwin:special-display-config)
  (push '("*undo-tree Diff*"       :dedicated t :position bottom :stick t :noselect nil :height 0.3) popwin:special-display-config)
  (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
  (push '("^\\*ivy-occur.+\\*$"    :regexp t :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
  (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("^\*WoMan.+\*$"          :regexp t             :position bottom                                   ) popwin:special-display-config)
  (push '("*Google Translate*"     :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("^\\*Flycheck.+\\*$"     :regexp t :dedicated t :position bottom :stick t :noselect nil) popwin:special-display-config))

(use-package spacemacs-purpose-popwin
  :after (window-purpose popwin)
  :preface
  (defun kalle/advice-pupo-close-window (orig-fun &rest args)
    (if (region-active-p)
        (call-interactively 'keyboard-quit)
      (apply orig-fun args)))
  :config
  (advice-add 'pupo/close-window :around #'kalle/advice-pupo-close-window)
  (require 'cl)
  (pupo-mode))

(use-package windmove
  :bind
  ((:map kalle-map)
   ("w h" . windmove-left)
   ("w j" . windmove-down)
   ("w k" . windmove-up)
   ("w l" . windmove-right)))

(use-package window-purpose
  :hook (kalle-after-emacs-load . purpose-mode)
  :bind ((:map kalle-map)
         ("wpw" . purpose-toggle-window-purpose-dedicated)
         ("wpb" . purpose-toggle-window-buffer-dedicated))
  :config
  (setcdr purpose-mode-map nil)
  (setcdr (assq 'switch-to-buffer purpose-action-sequences)
          '(purpose-display-maybe-same-window
            purpose-display-reuse-window-buffer
            purpose-display-reuse-window-purpose
            purpose-display-maybe-other-window
            purpose-display-maybe-other-frame
            purpose-display-maybe-pop-up-window
            purpose-display-maybe-pop-up-frame)))

;;; Languages

;;;; CSharp

(use-package csharp-mode
  :defer t
  :config
  (setcdr csharp-mode-map nil))

(use-package omnisharp
  :commands (omnisharp-install-server)
  :hook (csharp-mode . omnisharp-mode)
  :bind
  (:map omnisharp-mode-map
        ("C-c g e" . omnisharp-solution-errors)
        ("C-c g G" . omnisharp-go-to-definition-other-window)
        ("C-c g u" . omnisharp-helm-find-usages)
        ("C-c g U" . omnisharp-find-usages-with-ido)
        ("C-c g s" . omnisharp-helm-find-symbols)
        ("C-c g i" . omnisharp-find-implementations)
        ("C-c g I" . omnisharp-find-implementations-with-ido)
        ("C-c g r" . omnisharp-navigate-to-region)
        ("C-c g m" . omnisharp-navigate-to-solution-member)
        ("C-c g M" . omnisharp-navigate-to-solution-member-other-window)
        ("C-c g f" . omnisharp-navigate-to-solution-file)
        ("C-c g F" . omnisharp-navigate-to-solution-file-then-file-member)
        ("C-c g c" . omnisharp-navigate-to-current-file-member)
        ("C-c h t" . omnisharp-current-type-information)
        ("C-c h T" . omnisharp-current-type-information-to-kill-ring)
        ("C-c r m" . omnisharp-rename)
        ("C-c r M" . omnisharp-rename-interactively)
        ("C-c r r" . omnisharp-run-code-action-refactoring)
        ("C-c s s" . omnisharp-start-omnisharp-server)
        ("C-c s S" . omnisharp-stop-server)
        ("C-c s r" . omnisharp-reload-solution)
        ("C-c s i" . omnisharp-install-server))
  :config
  (eval-after-load
      'company
    '(add-to-list 'company-backends 'company-omnisharp)))

;;;; Elisp

(use-package dash
  :hook ((emacs-lisp-mode) . dash-enable-font-lock))

(use-package outline
  :hook (emacs-lisp-mode . outline-minor-mode))

(use-package outshine
  :hook (outline-minor-mode . outshine-hook-function))

;;;; HTML

(use-package css-mode
  :mode (("\\.css\\'" . css-mode))
  :bind (:map css-mode-map
              ("C-c C-d" . css-lookup-symbol))
  :init
  (setq css-indent-offset 2))

(use-package emmet-mode
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode)
         (web-mode . emmet-mode)
         (rjsx-mode . kalle/rjsx-emmet-mode))
  :preface
  (defun kalle/rjsx-emmet-mode ()
    "Activate `emmet-mode' and configure it for local buffer."
    (emmet-mode)
    (setq-local emmet-expand-jsx-className? t)))

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :init
  (setq web-mode-enable-html-entities-fontification t
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-enable-auto-pairing nil))

;;;; JavaScript

(use-package add-node-modules-path
  :hook (js2-mode . add-node-modules-path))

(use-package company-tern
  :after tern
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil)
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2-mode")))
  (js2-imenu-extras-setup))

(use-package rjsx-mode
  :defer t
  :preface
  (defun kalle/react-inside-string-q ()
    "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
    (let ((result (nth 3 (syntax-ppss))))
      result))

  (defun kalle/react-inside-comment-q ()
    "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
    (let ((result (nth 4 (syntax-ppss))))
      result))

  (defun kalle/react-inside-string-or-comment-q ()
    "Return non-nil if point is inside string, documentation string or a comment.
If optional argument P is present, test this instead of point."
    (or (kalle/react-inside-string-q)
        (kalle/react-inside-comment-q)))

  (defun kalle/javascript-jsx-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "js")
         (re-search-forward "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
                            magic-mode-regexp-match-limit t)
         (progn (goto-char (match-beginning 1))
                (not (kalle/react-inside-string-or-comment-q)))))
  :init
  ;; enable rjsx mode by using magic-mode-alist
  (add-to-list 'magic-mode-alist (cons #'kalle/javascript-jsx-file-p 'rjsx-mode)))

(use-package tern
  :hook (js2-mode . tern-mode)
  :init
  (setq tern-command '("tern" "--no-port-file"))
  :config
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil))

;;;; Minor languages

(use-package ansible
  :hook (yaml-mode . ansible))

(use-package git-modes
  :defer t)

(use-package json-mode
  :defer t)

(use-package yaml-mode
  :defer t)

;;;; Python

(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package company-anaconda
  :after anaconda-mode
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :init
  (setq python-shell-interpreter "python3"))

;;;; Org

(use-package org
  :defer t
  :init
  (setq-default
   org-src-window-setup 'current-window
   org-edit-src-content-indentation 0
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-hide-leading-stars t
   org-hide-leading-stars-before-indent-mode t
   org-image-actual-width nil
   org-imenu-depth 8
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-startup-indented t
   org-startup-with-inline-images t
   outline-blank-line t))

;;; The end

(progn ;     startup
  (add-hook 'kalle-after-emacs-load-hook
            `(lambda ()
               (setq file-name-handler-alist file-name-handler-alist-old
                     gc-cons-threshold 800000
                     gc-cons-percentage 0.1)
               (garbage-collect)) t)

  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
