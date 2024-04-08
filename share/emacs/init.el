;;; init.el --- Dr. Cotterell's site configuration. -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq read-process-output-max (* 3 1024 1024))

(require 'package)

(setq package-enable-at-startup nil)

(setq package-install-upgrade-built-in t)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("melpa" .  4)
        ("melpa-stable" . 3)
        ("nongnu" . 2)
        ("gnu" . 1)))

(when (fboundp 'prefer-coding-system)
  (prefer-coding-system 'utf-8-unix))

(when (fboundp 'set-language-environment)
  (set-language-environment "UTF-8"))

(eval-and-compile
  (if (version< emacs-version "27.0")
    (package-initialize)))

(eval-and-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(defun cotterell/init/display-tab ()
  "Visually display TAB character."
  (defvar whitespace-style)
  (setq indent-tabs-mode nil)
  (setq buffer-display-table (make-display-table))
  (setq whitespace-style '(face lines tabs))
  (whitespace-mode +1))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(require 'bind-key)

(setq use-package-verbose nil)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(use-package display-line-numbers
  :ensure nil
  :if (version<= "26.0.50" emacs-version)
  :hook ((prog-mode . display-line-numbers-mode)
         (text-mode . display-line-numbers-mode)))

(add-hook 'java-mode-hook #'cotterell/init/display-tab)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; stub (global-linum-mode) -- it was obsoleted in emacs-29.1
(when (version<= "29.1" emacs-version)
  (unless (fboundp 'global-linum-mode)
    (defun global-linum-mode (&optional arg)
      "Toggle Linum mode in all buffers.
With prefix ARG, enable Global-Linum mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil."
      (warn "Emacs 29.1 marks linum-mode and global-linum-mode as obsolete.")
      (warn "Potential Fix: Replace '%s' with '%s' in your %s file."
            "(global-linum-mode)"
            "(global-display-line-numbers-mode)"
            user-init-file)
      (global-display-line-numbers-mode arg))))

(setq-default
 ; do not load outdated byte code
 load-prefer-newer t)

(setq-default
 ; support longer lines in elisp docstrings
 byte-compile-docstring-max-column most-positive-fixnum)

(setq-default
 ; suppress BEL sound
 visible-bell t)

(setq-default
 c-basic-offset 4         ; basic offset between indentation levels
 c-default-style "linux"  ; default style for new CC Mode buffers
 tab-width 4              ; width of a TAB character
 whitespace-line-column 100
 whitespace-style '(face lines))

(setq-default
 line-number-mode t       ; show current line number on mode line
 column-number-mode t)    ; show current column number on mode line

(setq-default
 indent-tabs-mode nil)    ; TAB key produces spaces instead of \t

(setq-default
 indent-line-function 'insert-tab) ; set the indent function

(setq-default
 ;; set window titles in terminal
 xterm-set-window-title t)

;;----------------------------------------------------------------------------;;
;;                        NATIVE COMPILATION                                  ;;
;;----------------------------------------------------------------------------;;

(when (and (version<= "28.1" emacs-version)
           (native-comp-available-p))

  (setq-default
   ;; enables asynchronous (a.k.a. just-in-time, or JIT) native compilation
   native-comp-jit-compilation t)

  (setq-default
   ;; optimization level for native compilation (2 = max optimization)
   native-comp-speed 2)

  (setq-default
   ;; maximum number of concurrent native-compilation subprocesses
   native-comp-async-jobs-number 1)

  (setq-default
   ;; log native-compilation warnings without popping up the *Warnings* buffer
   native-comp-async-report-warnings-errors 'silent)

  (setq-default
   ;; natively compile packages as part of their installation
   package-native-compile t))

;; add .saves to the list o backup directories
(add-to-list 'backup-directory-alist
	     (cons "." (expand-file-name ".saves" user-emacs-directory)))

(setq-default
 version-control t        ; use version numbers for backups
 kept-new-versions 10     ; number of newest versions to keep
 kept-old-versions 0      ; number of oldest versions to keep
 delete-old-versions t    ; don't ask to delete excess backup versions
 backup-by-copying t      ; backups are copies
 vc-make-backup-files t)  ; backup files even if they are managed by a VCS

(set-face-attribute 'minibuffer-prompt nil
                    :foreground "cornflowerblue")

(set-face-attribute 'font-lock-function-name-face nil
                    :foreground "cornflowerblue")

(use-package flycheck
  :commands (flycheck-parse-checkstyle global-flycheck-mode)
  :init (global-flycheck-mode)
  :config
  (let ((cs1302-xml (expand-file-name "../lib/cs1302_checks.xml" user-emacs-directory)))
    ;; setup a syntax checker that conforms to the CSCI 1302 Code Style Guide
    (flycheck-define-checker java-checkstyle-checker
      "A Java style checker using Checkstyle."
      :command ("checkstyle" (option "-c" check1302-xml) "-f" "xml" source)
      :error-parser flycheck-parse-checkstyle
      :enable t
      :modes java-mode)
    ;; show style guide violations in emacs when in java-mode
    (add-to-list 'flycheck-checkers 'java-checkstyle-checker)))

(use-package modus-themes
  :ensure
  :demand
  :defines (modus-themes-to-toggle)
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-mixed-fonts t)
  (modus-themes-to-toggle '(modus-vivendi modus-operandi))
  (modus-themes-prompts '(bold))
  :config
  (load-theme (car modus-themes-to-toggle) t t)
  (defun init/modus-themes-init ()
    "Load the first theme in `modus-themes-to-toggle'."
    (load-theme (car modus-themes-to-toggle) t nil))
  :bind ("<f5>" . modus-themes-toggle)
  :hook (after-init . init/modus-themes-init))

(use-package xt-mouse
  :ensure nil
  :unless (display-graphic-p)
  :init
  (advice-add 'normal-mouse-startup-screen :override #'normal-no-mouse-startup-screen)
  (xterm-mouse-mode t)
  :bind* (("<mouse-4>" . scroll-down-line)
          ("<mouse-5>" . scroll-up-line)))

(provide 'init)

;;; init.el ends here
