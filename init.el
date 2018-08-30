;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(progn ;     init optimization
  (defvar file-name-handler-alist-old file-name-handler-alist)
  
  (setq file-name-handler-alist nil
        gc-cons-threshold 402653184
        gc-cons-percentage 0.6)

  (add-hook 'after-init-hook
            `(lambda ()
               (setq file-name-handler-alist file-name-handler-alist-old
                     gc-cons-threshold 800000
                     gc-cons-percentage 0.1)
               (garbage-collect)) t))

(progn ;     startup
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
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (setq use-package-verbose nil
        use-package-compute-statistics nil
        use-package-enable-imenu-support t)
  (require  'use-package))

(use-package auto-compile
  :demand t
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

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

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

;;; Long tail

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

(progn ;     personalize
  (defvar kalle-before-first-cmd-hook '())
  (defun kalle/run-before-first-cmd-hook ()
    (run-hooks 'kalle-before-first-cmd-hook)
    (remove-hook 'pre-command-hook 'kalle/run-before-first-cmd-hook))
  (add-hook 'pre-command-hook 'kalle/run-before-first-cmd-hook)
    
  (let ((org-file (expand-file-name "settings.org" user-emacs-directory))
        (settings (expand-file-name "var/tangled/settings.el" user-emacs-directory)))
    (when (file-newer-than-file-p org-file settings)
      (require 'org)
      (unless (file-directory-p (file-name-directory settings))
              (make-directory (file-name-directory settings) t))
      (org-babel-tangle-file org-file settings))
    (load settings)
    (message "Loading %s...done (%.3fs)" settings
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
