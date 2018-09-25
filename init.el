;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

;;; Bootstrap

;;;; Optimization

(progn
  (defvar file-name-handler-alist-old file-name-handler-alist)

  (setq file-name-handler-alist nil
        gc-cons-threshold 402653184
        gc-cons-percentage 0.6)

  (add-hook 'window-setup-hook
            `(lambda ()
               (setq file-name-handler-alist file-name-handler-alist-old
                     gc-cons-threshold 800000
                     gc-cons-percentage 0.1)
               (garbage-collect)) t)

  (defvar kalle-before-first-cmd-hook '())
  (defun kalle/run-before-first-cmd-hook ()
    (run-hooks 'kalle-before-first-cmd-hook)
    (remove-hook 'pre-command-hook 'kalle/run-before-first-cmd-hook))
  (add-hook 'pre-command-hook 'kalle/run-before-first-cmd-hook))

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
  (setq initial-major-mode 'text-mode)
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
  :hook (window-setup . kalle/load-auto-compile)
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

 ;; (L)ines
 ("ld" . delete-duplicate-lines)
 ("lk" . keep-lines)
 ("lm" . delete-matching-lines)
 ("ln" . delete-non-matching-lines)
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
   ("bb" . ivy-switch-buffer))
  :init
  (setq ivy-fixed-height-minibuffer t
        ivy-format-function 'ivy-format-function-arrow
        ivy-height 15
        ivy-initial-inputs-alist nil
        projectile-completion-system 'ivy
        magit-completing-read-function 'ivy-completing-read
        ivy-magic-slash-non-match-action nil
        smex-completion-method 'ivy
        ivy-use-ignore-default 'always
        ivy-ignore-buffers '("\\` "
                             "\\*Flycheck"
                             "\\*anaconda\\*"
                             "\\*anaconda-mode\\*"
                             "\\*ansi-term-.\\*"
                             "\\*bookmark list\\*"
                             "\\*spacemacs\\*"
                             "\\*flycheck.+\\*"
                             "\\*google translate\\*"
                             "\\*grep\\*"
                             "\\*help\\*"
                             "\\*helpful"
                             "\\*ibuffer*"
                             "\\*ivy-occur"
                             "\\*MDN CSS\\*"
                             "\\*occur\\*"
                             "\\*shell-"
                             "magit: "
                             "magit-process: "
                             "magit-diff: "
                             "magit-revision: "))
  :config
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
  :defer 0.1
  :init
  (setq-default
   global-auto-revert-non-file-buffers t
   auto-revert-verbose nil)
  :config
  (global-auto-revert-mode))

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
  :hook (find-file . kalle/recentf)
  :preface
  (defun kalle/recentf ()
    (unless recentf-mode
      (let ((inhibit-message t))
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
  :hook (kalle-before-first-cmd . savehist-mode)
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
  (defun kalle/treemacs-project-toggle ()
    "Toggle and add the current project to treemacs if not already added."
    (interactive)
    (if (eq (treemacs-current-visibility) 'visible)
        (delete-window (treemacs-get-local-window))
      (let ((path (projectile-project-root))
            (name (projectile-project-name)))
        (unless (treemacs-current-workspace)
          (treemacs--find-workspace))
        (treemacs-do-add-project-to-workspace path name)
        (treemacs-select-window))))

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
  :defer 0.1
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-compute-remaps t)
  :config
  (which-key-add-key-based-replacements
    "M-m b" "Buffers"
    "M-m br" "read-only-mode"
    "M-m f" "Files"
    "M-m e" "Errors/Linting"
    "M-m fs" "save-buffer"
    "M-m fS" "save-some-buffers"
    "M-m g" "Git"
    "M-m h" "Help"
    "M-m i" "Insert"
    "M-m l" "Lines"
    "M-m m" "Macros"
    "M-m n" "Narrow"
    "M-m p" "Projects"
    "M-m ps" "Search"
    "M-m q" "Quit"
    "M-m r" "Rectangle/Register"
    "M-m s" "Search"
    "M-m w" "Windows"
    "M-m z" "Zoom")
  (which-key-mode 1))

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
  :hook (window-setup . projectile-mode)
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
  :hook (compilation-filter . kalle/colorize-compilation-buffer)
  :init
  (setq-default compilation-always-kill t
                compilation-scroll-output 'first-error)
  :config
  (defun kalle/colorize-compilation-buffer ()
    (require 'ansi-color)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(use-package shell-pop
  :commands (shell-pop kalle/projectile-shell-pop)
  :bind
  (("M-'" . kalle/shell-pop-no-cwd)
   ("C-'" . kalle/shell-pop-full-screen-no-cwd)
   (:map kalle-map)
   ("'" . shell-pop)
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
  (setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))
        shell-pop-restore-window-configuration nil
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
   ("TAB" . crux-switch-to-previous-buffer)
   ("b C-d" . crux-kill-other-buffers)
   ("f D" . crux-delete-file-and-buffer)
   ("f R" . crux-rename-file-and-buffer)
   ("=" . crux-cleanup-buffer-or-region))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify))

(use-package delsel
  :defer 0.1
  :config
  (delete-selection-mode))

(use-package dtrt-indent
  :defer 0.1
  :config
  (dtrt-indent-global-mode))

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
  (global-flycheck-mode 1))

(use-package iedit
  :bind ("C-;" . iedit-mode))

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
  :defer 0.1
  :config
  (global-undo-tree-mode))

(use-package wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :hook (grep-mode . wgrep-setup))

(use-package wgrep-ag
  :hook (ag-mode . wgrep-ag-setup))

(use-package ws-butler
  :hook (find-file . ws-butler-global-mode))

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
  :defer 0.1
  :config
  (global-subword-mode))

(use-package swiper
  :bind
  ((:map kalle-map)
   ("s S" . swiper-all)
   ("s s" . swiper)
   (:map swiper-map)
   ("M-j" . swiper-avy)))

;;;; Version-control

(use-package diff-hl
  :defer 0.5
  :commands (diff-hl-magit-post-refresh diff-hl-dired-mode)
  :bind
  ((:map kalle-map)
   ("gn" . diff-hl-next-hunk)
   ("gp" . diff-hl-previous-hunk)
   ("gm" . diff-hl-mark-hunk))
  :init
  (setq diff-hl-margin-symbols-alist '((insert . " ")
                                       (delete . " ")
                                       (change . " ")
                                       (unknown . " ")
                                       (ignored . " ")))
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  :config
  (global-diff-hl-mode)
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

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
   ("gb" . magit-blame-mode)
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
  :defer 0.1
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

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
  :hook ((text-mode conf-mode prog-mode) . display-line-numbers-mode))

(use-package doom-modeline
  :hook (window-setup . doom-modeline-init)
  :init
  (setq doom-modeline-height 29))

(use-package fill-column-indicator
  :hook ((git-commit-setup . kalle/git-fill-column-indicator))
  :preface
  (defun kalle/git-fill-column-indicator ()
    (setq fill-column 72)
    (fci-mode))
  :config
  (with-eval-after-load 'company
    (add-hook 'fci-mode-hook '(lambda () (company-mode -1)))))

(use-package doom-themes
  :config
  (if (display-graphic-p)
      (load-theme 'doom-one 't)
    (load-theme 'doom-molokai 't))
  (add-hook 'treemacs-mode-hook 'doom-themes-treemacs-config)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package uniquify
  :defer t
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package volatile-highlights
  :defer 0.1
  :config
  (vhl/define-extension 'undo-tree
                        'undo-tree-move
                        'undo-tree-yank)
  (with-eval-after-load 'undo-tree
    (vhl/install-extension 'undo-tree)
    (vhl/load-extension 'undo-tree))
  (volatile-highlights-mode))

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
  :defer 0.1
  :bind ((:map kalle-map)
         ("wpw" . purpose-toggle-window-purpose-dedicated)
         ("wpb" . purpose-toggle-window-buffer-dedicated))
  :config
  (purpose-mode)
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
  :hook ((emacs-lisp-mode lisp-interaction-mode) . dash-enable-font-lock))

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
