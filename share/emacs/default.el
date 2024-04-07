;;; default.el --- Dr. Cotterell's site defaults.

;; Local Variables:
;; mode: emacs-lisp
;; End:

;;; Commentary:

;; To learn more about a setting, use describe-variable (C-h v).

;;; Code:

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

(provide 'default)

;;; default.el ends here
