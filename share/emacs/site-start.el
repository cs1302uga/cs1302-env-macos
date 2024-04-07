;;; site-start.el --- Dr. Cotterell's site configuration. -*- mode: emacs-lisp; lexical-binding: t; -*-

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

(defun cotterell/site-start/display-tab ()
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

(add-hook 'java-mode-hook #'cotterell/site-start/display-tab)
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

(provide 'site-start)

;;; site-start.el ends here
