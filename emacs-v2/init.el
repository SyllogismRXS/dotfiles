; References:
; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
; https://medium.com/@suvratapte/configuring-emacs-from-scratch-packages-220bbc5e55b7
; https://menno.io/posts/use-package/					;

; Disable the startup message
(setq inhibit-startup-message t)

; Set the load path for where lisp code can be loaded outside of the
; package system.
(add-to-list 'load-path "~/.emacs.d/lisp")
                                        ;
; Disable the icon tool bar at the top of emacs
(tool-bar-mode -1)

; Enable the "File" "Edit" "Options" etc.
(menu-bar-mode 1)

; Disable the side scrolling bar
(scroll-bar-mode -1)

; Highlight the current line
(global-hl-line-mode t)

; Display the line number at the bottom of emacs
(line-number-mode t)

(setq-default fill-column 79)

; The value is in 1/10pt, so 130 will give you 130pt, etc.
(set-face-attribute 'default nil :height 130)

; Disable lockfiles
(setq create-lockfiles nil)

; Set indent level to 2 in javscript mode
(setq js-indent-level 2)

(setq user-mail-address "kevin.demarco@gmail.com")
(setq user-website "http://www.kevindemarco.com")
(setq user-full-name "Kevin DeMarco")

; Allow easier resizing of windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

; Create a key binding for revert-buffer
(global-set-key (kbd "C-c r") 'revert-buffer)

; use SHIFT-<left>, SHIFT-<right>, SHIFT-<up>, SHIFT-<down> to change
; windows
(windmove-default-keybindings)

; Display the column number next to line number at the bottom of the
; emacs window
(column-number-mode 1)

; Don't wrap lines when the text runs past the current window.
(setq-default truncate-lines 1)

; Use spaces, not hard tabs
(custom-set-variables '(indent-tabs-mode nil))

; Prevent asking for confirmation to kill processes when exiting.
(custom-set-variables '(confirm-kill-processes nil))

; Put all backup files in ~/.emacs.d/backups
(custom-set-variables
 '(backup-directory-alist
   `(("." . ,(concat user-emacs-directory "backups")))))

; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Highlight matching parenthesis
(show-paren-mode)

; Check to see if server-running-p function is bound and only start
; server if it isn't running yet
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;; Do not use `init.el` for `custom-*` code - use `custom-file.el`.
;; TODO: Can we create this file automatically if it doesn't exist?
(setq custom-file "~/repos/dotfiles/emacs-v2/custom-file.el")

;; Assuming that the code in custom-file is execute before the code
;; ahead of this line is not a safe assumption. So load this file
;; proactively.
(load-file custom-file)

; Require package and install packages from melpa
(require 'package)
(custom-set-variables '(package-archives
                        '(("marmalade" . "https://marmalade-repo.org/packages/")
                          ("melpa"     . "https://melpa.org/packages/")
                          ("elpa"      . "https://elpa.gnu.org/packages/"))))

; Initialize the package system
(package-initialize)

; Refresh the package archive contents                                        ;
(when (not package-archive-contents)
  (package-refresh-contents))

; Install use-package if it doesn't exist
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

; Load use-package
(require 'use-package)

; Disable use-package's "ensure" by default.
(custom-set-variables '(use-package-always-ensure nil))

; Defer loading of packages by default
(custom-set-variables '(use-package-always-defer t))

; Used for debugging the loading of packages
(custom-set-variables '(use-package-verbose nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install icons
(use-package all-the-icons
  :ensure t
  :defer nil
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom themes
(use-package doom-themes
  :ensure t
  :defer nil
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
        doom-themes-enable-italic nil) ; if nil, italics is universally disabled

  ;(load-theme 'doom-one t)
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
  (doom-themes-org-config)

  ;; Set the comment font size to the same as the other text. This is
  ;; necessary in the doom-acario-dark theme.
  (custom-set-faces
   `(font-lock-comment-face           ((t (:inherit font-lock-builtin-face))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-builtin-face)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth scrolling (line by line) instead of jumping by half-screens.
(use-package smooth-scrolling
  :ensure t
  :defer nil
  :config
  (smooth-scrolling-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display line numbers
(when (>= emacs-major-version 26)
  (use-package display-line-numbers
    :defer nil
    :ensure t
    :config
    (global-display-line-numbers-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The which-key package makes Emacs functionality much easier to
; discover and explore: in short, after you start the input of a
; command and stop, pondering what key must follow, it will
; automatically open a non-intrusive buffer at the bottom of the
; screen offering you suggestions for completing the
; command.
(use-package which-key
  :ensure t
  :defer nil
  :diminish which-key-mode
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
(use-package helm
  :ensure t
  :defer nil
  :config
  ;(require 'helm-config)
  :init
  (helm-mode 1) ; Needed for org-roam to autocomplete correctly
  :bind
  (("M-x"     . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   ("C-x b"   . helm-multi-files)
   ("M-y"     . helm-show-kill-ring)
   ("C-c p"   . helm-projectile-ag)
   :map helm-map
   ("C-j" . helm-next-line)
   ("C-k" . helm-previous-line)
   ("C-z" . helm-select-action)
   ("<tab>" . helm-execute-persistent-action)
   )
  )

; helm-ag
(use-package helm-ag
  :ensure t
  :defer nil
  )

; helm-tramp
(use-package helm-tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh")
  ; Don't make a backup files and lockfiles at remote server, it will be saved
  ; faster.
  (setq make-backup-files nil
        create-lockfiles nil)

  ; Turn off slow extensions when executing helm-tramp
  (add-hook 'helm-tramp-pre-command-hook '(lambda () (projectile-mode 0)))
  ; Turn on extensions when executing helm-tramp-quit command.
  (add-hook 'helm-tramp-quit-hook '(lambda () (projectile-mode 1)))
  :bind
  (("C-c s" . helm-tramp)
  )
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
  :bind ("<f8>" . deft)
  :config
  (setq deft-extensions '("txt" "tex" "org" "md" "text"))
  (setq deft-directory "~/repos/private/org")
  (setq deft-recursive t)
  )

(use-package org
  :ensure t
  :defer nil
  :config
  (setq org-directory "~/repos/private/org")
  (setq org-agenda-files (list "~/repos/private/org/inbox.org"
                               "~/repos/private/org/gtd.org"
                               "~/repos/private/org/schedule.org"
                               "~/repos/private/org/phone.org"
                               ))
  (setq org-todo-keywords
        '((sequence "TODO" "NEXT" "FEEDBACK" "|" "DONE" "DEFERRED" "MEETING")))
  (setq org-log-done 'time) ; Create a timestamp when DONE
  (setq org-archive-location "~/repos/private/org/archive/%s_archive::")
  (setq org-agenda-custom-commands
      '(("h" "Home" tags-todo "@home")
        ("n" "Next" todo "NEXT")
        ("p" "Side Projects" tags-todo "@side")
        ("c" "Consulting" tags-todo "@consulting")
        ("l" "Learning" tags-todo "@learning")
        ("r" "RIF Robotics" tags-todo "@rif")
        ("e" "Entrepreneur" tags-todo "@entrepreneur")))
  (setq org-refile-targets '(("~/repos/private/org/gtd.org" :maxlevel . 2)
                             ("~/repos/private/org/someday.org" :level . 1)
                             ("~/repos/private/org/schedule.org" :maxlevel . 1)))

  (setq org-tag-alist '( ("@side" . ?s)
                         ("@consulting" . ?c)
                         ("@learning" . ?l)
                         ("@rif" . ?r)
                         ("@entrepreneur" . ?e)))
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/repos/private/org/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("m" "Meeting [inbox]" entry
                                 (file+headline "~/repos/private/org/inbox.org" "Meetings")
                                 "* MEETING %i%? %U")
                                ("s" "Schedule" entry
                                 (file "~/repos/private/org/schedule.org")
                                 "* %^{Event Name} \n  :PROPERTIES:\n  :calendar-id: kevin.demarco@gmail.com\n  :END:\n:org-gcal:\n%i%?\n:END:")))
  (setq org-clock-persist 'history)
  (setq org-clock-idle-time 15)
  (org-clock-persistence-insinuate)
  ; Fix the SHIFT+Arrow keys in org-mode (when not on headline)
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  (add-to-list 'org-file-apps '("\\.pdf" . "evince %s"))
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c C-x o" . org-clock-out)
  ("C-c C-x l" . org-clock-in-last)
  )

;(use-package org-journal
;  :ensure t
;  :defer nil
;  :custom
;  (org-journal-date-prefix "#+TITLE: ")
;  (org-journal-file-format "%Y-%m-%d.org")
;  (org-journal-dir "~/repos/private/org/journal")
;  (org-journal-date-format "%A, %d %B %Y")
;  (org-journal-enable-agenda-integration t)
;  :bind
;  ("C-c j" . org-journal-new-entry)
;  )

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/repos/private/org/roam-v2")
  (org-roam-completion-everywhere t)
  :bind
  ("C-c j" . org-roam-dailies-capture-today)
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i" . completion-at-point)
   :map org-roam-dailies-map
   ("Y" . org-roam-dailies-capture-yesterday)
   ("T" . org-roam-dailies-capture-tomorrow)
   )
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n\
#+OPTIONS: toc:nil num:nil author:nil date:nil\n\
#+STARTUP: showall align\n\
#+LaTeX_CLASS: article\n\
#+LaTeX_CLASS_OPTIONS: [9pt,twocolumn,portrait]\n\
#+LATEX_HEADER: \\usepackage[margin=0.5in]{geometry}\n\
#+LATEX_HEADER: \\usepackage{enumitem}\n"))))
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  )

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/bash")
  )

(use-package helm-mt
  :ensure t
  :bind
  ("C-x t" . helm-mt)
  )

(use-package projectile
  :ensure t
  :defer nil
  :init
  :config
  (setq projectile-completion-system 'helm)
  )

(use-package helm-projectile
  :ensure t
  :defer nil
  :init
  (helm-projectile-on)
  :config
  (setq projectile-switch-project-action 'helm-projectile)
  :bind
  ("C-x C-g" . helm-projectile)
  )

(use-package org-gcal
  :ensure t
  :defer nil
  :config
  (setq org-gcal-client-id "411566505301-idk29ar5mp596dcllvg9dksiisc4shda.apps.googleusercontent.com"
        org-gcal-client-secret "ZiJOjE8zPNXWA2dBTtIq450v"
        org-gcal-file-alist '(("kevin.demarco@gmail.com" .  "~/repos/private/org/schedule.org"))
        )
  ;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  ;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Setup Google C / C++ Style
(use-package google-c-style
  :ensure t
  :defer nil
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)
  )

;; (use-package flycheck-google-cpplint
;;   :ensure t
;;   :defer nil
;;   )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package lua-mode
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tex-site
  ;:ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  ;(setq TeX-auto-save t)
  ;(setq TeX-parse-self t)
  ;(setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (visual-line-mode)
              (flyspell-mode)
              ;(rainbow-delimiters-mode)
              ;(company-mode)
              ;(smartparens-mode)
              ;(turn-on-reftex)
              ;(setq reftex-plug-into-AUCTeX t)
              ;(reftex-isearch-minor-mode)
              ;(setq TeX-PDF-mode t)
              ;(setq TeX-source-correlate-method 'synctex)
              ;(setq TeX-source-correlate-start-server t)))
              ))

  ;;; Update PDF buffers after successful LaTeX runs
  ;(add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
  ;          #'TeX-revert-document-buffer)
  ;
  ;;; to use pdfview with auctex
  ;(add-hook 'LaTeX-mode-hook 'pdf-tools-install)
  ;
  ;;; to use pdfview with auctex
  ;(setq TeX-view-program-selection '((output-pdf "pdf-tools"))
  ;      TeX-source-correlate-start-server t)
  ;(setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  )

; Use nxml-mode for common ROS and Gazebo extensions
(add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.world\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.sdf\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xacro\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.urdf\\'" . nxml-mode))

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; When in c/c++ mode, specify where to find the other header or source
; file. TODO : Should this be in a "use-package" somehow?
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-get-other-file)))

(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c f") 'ff-find-other-file)))

(setq cc-search-directories '("."
                              "../include/*/"
                              "../../../src/*"
                              "../../include/*/*"
                              "../../*/*"
                              "../../include"
                              "../../../../include/*/*/*/*"
                              "../../../../../src/*/*/*/*"
                              "../../../../../include/*/*/*/*/*"
                              "../../../../../../src/*/*/*/*"
                              "/usr/local/*/src/*"
                              "$PROJECT/src"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy from emacs to other programs:
; http://emacs.stackexchange.com/questions/1051/copy-region-from-emacs-without-newlines
; TODO: Should I store custom functions somewhere else?
(defun copy-without-newline (&optional beg end)
  "Save the current region (or line) to the `kill-ring' after stripping extra whitespace and new lines"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((my-text (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (insert my-text)
      (goto-char 1)
      (while (looking-at "[ \t\n]")
        (delete-char 1))
      (let ((fill-column 9333999))
        (fill-region (point-min) (point-max)))
      (kill-region (point-min) (point-max)))))


;; TODO:
					; org-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spacemacs theme configuration
;(use-package spacemacs-theme
;  :ensure t
;  :defer nil
;  :init
;  (load-theme 'spacemacs-dark t)
;  (setq spacemacs-theme-org-agenda-height nil)
;  (setq spacemacs-theme-org-height nil))
;
;(use-package spaceline
;  :ensure t
;  :defer nil
;  :init
;  (setq powerline-default-separator 'arrow-fade)
;  :config
;  (require 'spaceline-config)
;  (spaceline-spacemacs-theme))


; Starting formatter for LaTeX docs
;(defun my/replace-dot-with-newline ()
;  (interactive)
;  (replace-regexp "\\.\\W*" ".\n"))
;
;(defun my/replace-newline-with-space ()
;  (interactive)
;  (replace-regexp "\\([a-zA-Z0-9]\\)\n" "\\1 "))
;
;(defun my/latex-clean-lines ()
;  (interactive)
;  (my/replace-dot-with-newline)
;  (delete-trailing-whitespace)
;  (my/replace-newline-with-space))

;; Originally from stevey, adapted to support moving to a new directory.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     ;; Disable ido auto merge since it too frequently jumps back to the original
     ;; file name if you pause while typing. Reenable with C-z C-z in the prompt.
     (let ((ido-auto-merge-work-directories-length -1))
       (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                       (buffer-file-name))))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; Only rename if the file was saved before. Update the
  ;; buffer name and visited file in all cases.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil)))

  (setq default-directory (file-name-directory new-name))

  (message "Renamed to %s." new-name))
