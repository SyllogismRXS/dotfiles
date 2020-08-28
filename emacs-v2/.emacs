; References:
; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
; https://medium.com/@suvratapte/configuring-emacs-from-scratch-packages-220bbc5e55b7
; https://menno.io/posts/use-package/					;

;Disable the startup message
(setq inhibit-startup-message t)
					;
;Disable the icon tool bar at the top of emacs
(tool-bar-mode -1)

; Enable the "File" "Edit" "Options" etc.
(menu-bar-mode 1)

; Disable the side scrolling bar
(scroll-bar-mode -1)

; Highlight the current line
(global-hl-line-mode t)

; Display the line number at the bottom of emacs
(line-number-mode t)

; Allow easier resizing of windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

; use S-<left>, S-<right>, S-<up>, S-<down> to change windows
(windmove-default-keybindings)

; Don't wrap lines when the text runs past the current window.
(setq-default truncate-lines 1)

(custom-set-variables '(indent-tabs-mode nil))

; Put all backup files in ~/.emacs.d/backups
(custom-set-variables
 '(backup-directory-alist
   `(("." . ,(concat user-emacs-directory "backups")))))

; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Do not use `init.el` for `custom-*` code - use `custom-file.el`.
(setq custom-file "~/.emacs.d/custom-file.el")

;; Assuming that the code in custom-file is execute before the code
;; ahead of this line is not a safe assumption. So load this file
;; proactively.
(load-file custom-file)

; Require package and install packages from melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; If it is not installed, install use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
        doom-themes-enable-italic nil) ; if nil, italics is universally disabled

  (load-theme 'doom-acario-dark t)
  ;(load-theme 'doom-gruvbox t)
  ;(load-theme 'doom-oceanic-next t)
  ;(load-theme 'doom-tomorrow-night t)
  ;(load-theme 'doom-dark+ t)
  ;(load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
(use-package helm
  :ensure t
  :bind
  (("M-x"     . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   ("C-x b"   . helm-multi-files)
   :map helm-map
   ("C-j" . helm-next-line)
   ("C-k" . helm-previous-line))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load magit for git
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load company for auto-completion
(use-package company
  :ensure t
  :bind (:map company-active-map
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous))
  :config
  ;; Use company mode everywhere.
  (global-company-mode t)
  (setq company-idle-delay 0.3)
  )

(use-package deft
  :ensure t
  :config
  (setq deft-extensions '("txt" "tex" "org"))
  (setq deft-directory "~/repos/private/org/roam")
  (setq deft-recursive t)
  )

(use-package org
  :ensure t
  :pin manual
  :load-path ("lisp/org-mode/lisp" "lisp/org-mode/lisp/contrib/lisp")
  )

(use-package org-journal
  :ensure t
  :after org
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/repos/private/org/journal")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-enable-agenda-integration t)
  :bind
  ("C-c j" . org-journal-new-entry))

;; TODO:
					; org-mode
					; helm-projectile
					; (server-start)
					; org-journal
					; multi-term
					; helm-mt
