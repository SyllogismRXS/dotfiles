;;; init.el --- Sample user customizations for Emacs EDT emulation

;; Copyright (C) 1986, 1992, 1993, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010  Free Software Foundation, Inc.

;; Author: Kevin Gallagher <kgallagh@@spd.dsccc.com>
;; Maintainer: Kevin Gallagher <kgallagh@@spd.dsccc.com>
;; Keywords: emulations

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an example of the `edt-user.el' file that you can use
;; to customize the Emacs EDT emulation mode.  Copy this file to
;; somewhere in your `load-path', and edit it as desired.
;; See Info node `edt' for more details.

;; ====================================================================

(load-library "packages")

;;; Code:

;;;;
;;;; Setup user custom EDT key bindings.
;;;;

(defun edt-setup-user-bindings ()
  "Assigns user custom EDT Emulation keyboard bindings."

  ;; PF1 (GOLD), PF2, PF3, PF4
  ;;
  ;; This file MUST contain a binding of PF1 to edt-user-gold-map.  So
  ;; DON'T CHANGE OR DELETE THE REGULAR KEY BINDING OF PF1 BELOW!
  ;; (However, you may change the GOLD-PF1 binding, if you wish.)
  (edt-bind-function-key "PF1" 'edt-user-gold-map 'edt-mark-section-wisely)
  (edt-bind-function-key "PF2" 'query-replace 'other-window)
  (edt-bind-function-key "PF4" 'edt-delete-entire-line 'edt-undelete-line)

  ;; EDT Keypad Keys
  (edt-bind-function-key "KP1" 'edt-word-forward 'edt-change-case)
  (edt-bind-function-key "KP3" 'edt-word-backward 'edt-copy)
  (edt-bind-function-key "KP6" 'edt-cut-or-copy 'yank)
  (edt-bind-function-key "KP8" 'edt-scroll-window 'fill-paragraph)
  (edt-bind-function-key "KP9" 'open-line 'edt-eliminate-all-tabs)
  (edt-bind-function-key "KPP"
			 'edt-toggle-select 'edt-line-to-middle-of-window)
  (edt-bind-function-key "KPE" 'edt-change-direction 'overwrite-mode)

  ;; GOLD bindings for regular keys.
  (edt-bind-gold-key "a" 'edt-append)
  (edt-bind-gold-key "A" 'edt-append)
  (edt-bind-gold-key "h" 'edt-electric-user-keypad-help)
  (edt-bind-gold-key "H" 'edt-electric-user-keypad-help)

  ;; Control bindings for regular keys.
  ;;; Leave binding of C-c as original prefix key.
  (edt-bind-key "\C-j" 'edt-duplicate-word)
  (edt-bind-key "\C-k" 'edt-define-key)
  (edt-bind-gold-key  "\C-k" 'edt-restore-key)
  (edt-bind-key "\C-l" 'edt-learn)
  ;;; Leave binding of C-m to newline.
  (edt-bind-key "\C-n" 'edt-set-screen-width-80)
  (edt-bind-key "\C-o" 'open-line)
  (edt-bind-key "\C-p" 'fill-paragraph)
  ;;; Leave binding of C-r to isearch-backward.
  ;;; Leave binding of C-s to isearch-forward.
  (edt-bind-key "\C-t" 'edt-display-the-time)
  (edt-bind-key "\C-v" 'redraw-display)
  (edt-bind-key "\C-w" 'edt-set-screen-width-132)
  ;;; Leave binding of C-x as original prefix key.
  )

;;;
;;; LK-201 KEYBOARD USER EDT KEYPAD HELP
;;;

(defun edt-user-keypad-help ()
  "
                                USER EDT Keypad Active

                                +----------+----------+----------+----------+
   F7: Copy Rectangle           |Prev Line |Next Line |Bkwd Char |Frwd Char |
   F8: Cut Rect Overstrike      |   (UP)   |  (DOWN)  |  (LEFT)  | (RIGHT)  |
 G-F8: Paste Rect Overstrike    |Window Top|Window Bot|Bkwd Sent |Frwd Sent |
   F9: Cut Rect Insert          +----------+----------+----------+----------+
 G-F9: Paste Rect Insert
  F10: Cut Rectangle
G-F10: Paste Rectangle
  F11: ESC                      +----------+----------+----------+----------+
  F12: Beginning of Line        |   GOLD   |Query Repl|  FNDNXT  |Del Ent L |
G-F12: Delete Other Windows     |   (PF1)  |   (PF2)  |   (PF3)  |   (PF4)  |
  F13: Delete to Begin of Word  |Mark Wisel|Other Wind|   FIND   |  UND L   |
 HELP: Keypad Help              +----------+----------+----------+----------+
G-HELP: Emacs Help              |   PAGE   |Scroll Win|Open Line |  DEL W   |
   DO: Execute extended command |    (7)   |    (8)   |    (9)   |   (-)    |
  C-a: Beginning of Line        |Ex Ext Cmd|Fill Parag|Elim Tabs |  UND W   |
  C-b: Backward Character       +----------+----------+----------+----------+
  C-d: Delete Character         |  ADVANCE |  BACKUP  | CUT/COPY |  DEL C   |
  C-e: End of Line              |    (4)   |    (5)   |    (6)   |   (,)    |
  C-f: Forward Character        |   BOTTOM |    TOP   |   Yank   |  UND C   |
  C-g: Keyboard Quit            +----------+----------+----------+----------+
G-C-g: Keyboard Quit            | Fwd Word |    EOL   | Bwd Word |  Change  |
  C-h: Electric Emacs Help      |    (1)   |    (2)   |    (3)   | Direction|
G-C-h: Emacs Help               | CHNGCASE |  DEL EOL |   COPY   |          |
  C-i: Indent for Tab           +---------------------+----------+  (ENTER) |
  C-j: Duplicate Word           |         LINE        |SELECT/RES|          |
  C-k: Define Key               |         (0)         |    (.)   |  Toggle  |
G-C-k: Restore Key              |      Open Line      |Center Lin|Insrt/Over|
  C-l: Learn                    +---------------------+----------+----------+
  C-n: Set Screen Width 80
  C-o: Open Line                       +----------+----------+----------+
  C-p: Fill Paragraph                  |  FNDNXT  |   Yank   |    CUT   |
  C-q: Quoted Insert                   |  (FIND)) | (INSERT) | (REMOVE) |
  C-r: Isearch Backward                |   FIND   |          |   COPY   |
  C-s: Isearch Forward                 +----------+----------+----------+
  C-t: Display the Time                |SELECT/RES|SECT BACKW|SECT FORWA|
  C-u: Universal Argument              | (SELECT) |(PREVIOUS)|  (NEXT)  |
  C-v: Redraw Display                  |          |          |          |
  C-w: Set Screen Width 132            +----------+----------+----------+
  C-z: Suspend Emacs
G-C-\\: Split Window

  G-a: Append to Kill Buffer
  G-b: Buffer Menu
  G-c: Compile
  G-d: Delete Window
  G-e: Exit
  G-f: Find File
  G-g: Find File Other Window
  G-h: Keypad Help
  G-i: Insert File
  G-k: Toggle Capitalization Word
  G-l: Lowercase Word or Region
  G-m: Save Some Buffers
  G-n: Next Error
  G-o: Switch Windows
  G-q: Quit
  G-r: Revert File
  G-s: Save Buffer
  G-u: Uppercase Word or Region
  G-v: Find File Other Window
  G-w: Write file
  G-y: EDT Emulation OFF
  G-z: Switch to Default EDT Key Bindings
  G-2: Split Window
  G-%: Go to Percentage
  G- : Undo  (GOLD Spacebar)
  G-=: Go to Line
  G-`: What line
  G-/: Query-Replace"

  (interactive)
  (describe-function 'edt-user-keypad-help))


                                        ;(add-to-list 'load-path "~/repos/dotfiles/emacs/site-lisp/cc-mode")

                                        ;Set load-path for libraries (Linux Only)
                                        ;In windows, the site-lisp direction should be
                                        ;in the emacs-23.3/ directory, i.e. same level
                                        ; as /bin
(add-to-list 'load-path "~/repos/dotfiles/emacs/site-lisp")

(setq inhibit-splash-screen t)

;(require 'template)
;(template-initialize)

(require 'color-theme)
(color-theme-initialize)
; Only enable the clarity theme if in window mode
(if (display-graphic-p)
    (color-theme-clarity)
  )

                                        ; Use a different color scheme when using terminal
                                        ;(when (display-graphic-p) (color-theme-dark-laptop))

;; Modify the default grep command when I type: M-x grep
;; -n : list line numbers
;; -H : print the filename for each match
;; -e : useful to protect patterns beginning with -.
(setq grep-command "grep -nHir -e ")

; ;; Enable ido-mode
; (require 'ido) ;enable ido-mode
;
; (setq ido-enable-flex-matching t)
; (setq ido-everywhere t)
; (ido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm)
(require 'helm-config)


;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(customize-set-variable 'helm-ff-lynx-style-map t)

(helm-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)
(global-set-key (kbd "C-x C-g") 'helm-projectile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* (
         (backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, ⁖ “C:”
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
         )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
    )
  )

(setq make-backup-file-name-function 'my-backup-file-name)

;; setup for template package

(setq user-mail-address "kevin.demarco@gmail.com")
(setq user-website "http://www.kevindemarco.com")
(setq user-full-name "Kevin DeMarco")

;; Line numbers on side of screen
(require 'linum)
(line-number-mode 1)
(column-number-mode 1)  ;; Line numbers on left most column
                                        ;(global-linum-mode 1)
(setq linum-format "%4d \u2502")

;; Set timestamp
;(add-hook 'before-save-hook 'time-stamp)
;(setq time-stamp-pattern nil)

                                        ; Use spaces instead of tabs
;(setq-default indent-tabs-mode nil)

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(setq c-default-style "Google")
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

(setq-default fill-column 79)
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (fci-mode t)))
(global-fci-mode t)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Matlab-emacs setup
(add-to-list 'load-path "~/repos/dotfiles/emacs/site-lisp/matlab-emacs")
(load-library "matlab-load")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Latex for Emacs
;;
;; Dependencies: okular, texlive-full, auctex
;;
;; Okular setup:
;; 1.) Open Okular and go to...
;; 2.) Settings -> Configure Okular -> Editor
;; 3.) Set Editor to "Emacs client"
;; 4.) Command should automatically set to:
;;             emacsclient -a emacs --no-wait +%l %f
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable the .ido.last question on exit
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(TeX-command-list
   (quote
    (("XeLaTeX_SyncteX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run XeLaTeX")
     ("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("View" "%V" TeX-run-discard-or-function nil t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("Jump to PDF" "%V" TeX-run-discard-or-function nil t :help "Run Viewer"))))
 '(TeX-modes
   (quote
    (tex-mode plain-tex-mode texinfo-mode latex-mode doctex-mode)))
 '(column-number-mode t)
 '(ido-enable-last-directory-history nil)
 '(ido-max-work-directory-list 0)
 '(ido-max-work-file-list 0)
 '(ido-record-commands nil)
 '(package-selected-packages
   (quote
    (magit yasnippet org-journal markdown-mode flymake-cppcheck flycheck cpputils-cmake company-c-headers cmake-project auto-complete))))

                                        ;(latex-preview-pane-enable)

(setq-default TeX-master nil) ; Query for master file.
                                        ;(setq-default TeX-master "master") ; All master files called "master".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard emacs config (http://emacswiki.org/emacs/AUCTeX)
(setq TeX-parse-self t)
(setq TeX-auto-save t)
(setq-default TeX-master nil)

; Enable visual-line-mode if using visual-fill-column-mode
(add-hook 'visual-fill-column-mode-hook #'visual-line-mode)

; Show line numbers next to text.
(add-hook 'visual-fill-column-mode-hook #'linum-mode)

; Disable fci-mode if using visual-fill-column-mode
(add-hook 'visual-fill-column-mode-hook #'(lambda () (fci-mode -1)))

; Have LaTeX mode use visual-fill-column-mode
(add-hook 'LaTeX-mode-hook 'visual-fill-column-mode)
;(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;(setq reftex-default-bibliography '("./*.bib"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Check to see if server-running-p function is bound and only start server if
; it isn't running yet
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;; only start server for okular comms in latex mode
;(add-hook 'LaTeX-mode-hook 'server-start) ;(server-start)
;(setq TeX-PDF-mode t) ;; use pdflatex instead of latex

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable synctex correlation
(setq TeX-source-correlate-method 'synctex)

;; Enable synctex generation. Even though the command shows
;; as "latex" pdflatex is actually called

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use Okular as the pdf viewer. Build okular command, so that
;; Okular jumps to the current line in the viewer.
(setq TeX-view-program-selection
      '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "okular --unique %o#src:%n%b")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun latex-word-count ()
  (interactive)
  (shell-command (concat "/usr/bin/texcount "
                         "uncomment then options go here "
                         (buffer-file-name))))


; Reference https://stackoverflow.com/questions/539984/how-do-i-get-emacs-to-fill-sentences-but-not-paragraphs
(defun fill-sentence ()
  (interactive)
  (save-excursion
    (or (eq (point) (point-max)) (forward-char))
    (forward-sentence -1)
    (indent-relative t)
    (let ((beg (point))
          (ix (string-match "LaTeX" mode-name)))
      (forward-sentence)
      (if (and ix (equal "LaTeX" (substring mode-name ix)))
          (LaTeX-fill-region-as-paragraph beg (point))
        (fill-region-as-paragraph beg (point))))))
(global-set-key (kbd "M-j") 'fill-sentence)

;; peep-dired
(setq peep-dired-cleanup-on-disable t)
;(setq peep-dired-cleanup-eagerly t)

;; Org Mode
(require 'org-install)

(setq org-startup-folded nil)

; org-journal
;(setq org-journal-dir "~/repos/private/org/journal/")
(setq org-journal-date-prefix "#+TITLE: ")
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-journal-dir "~/repos/private/org/roam")
(setq org-journal-date-format "%A, %d %B %Y")
(require 'org-journal)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(setq org-log-done 'time)

(setq org-agenda-files (list "~/repos/private/org/inbox.org"
                             "~/repos/private/org/gtd.org"
                             "~/repos/private/org/tickler.org"
                             ))

; Find all org-journal files
(setq org-agenda-file-regexp "\\`[^.].*\\.org\\'\\|\\`[0-9]+\\'")

;; Set to the location of your Org files on your local system
;(setq org-directory "~/Dropbox/org")
;; Set to the name of the file where new notes will be stored

;(setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
;(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "FEEDBACK" "|" "DONE" "DEFERRED" "MEETING")))

;;; define categories that should be excluded
;(setq org-export-exclude-category (list "google" "private"))

;;; Define the directory to which archive files should be saved
(setq org-archive-location "~/repos/private/org/archive/%s_archive::")

;; Make windmove work in Org mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;;; define filter. The filter is called on each entry in the agenda.
;;; It defines a regexp to search for two timestamps, gets the start
;;; and end point of the entry and does a regexp search. It also
;;; checks if the category of the entry is in an exclude list and
;;; returns either t or nil to skip or include the entry.
(defun org-mycal-export-limit ()
  "Limit the export to items that have a date, time and a range. Also exclude certain categories."
  (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\\)>")
  (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
  (save-excursion
                                        ; get categories
    (setq mycategory (org-get-category))
                                        ; get start and end of tree
    (org-back-to-heading t)
    (setq mystart    (point))
    (org-end-of-subtree)
    (setq myend      (point))
    (goto-char mystart)
                                        ; search for timerange
                                        ;(setq myresult (re-search-forward org-tstr-regexp myend t))
    (setq myresult t)
                                        ; search for categories to exclude
    (setq mycatp (member mycategory org-export-exclude-category))
                                        ; return t if ok, nil when not ok
    (if (and myresult (not mycatp)) t nil)))

;;; activate filter and call export function
(defun org-mycal-export ()
  "Exports Org Agenda to Google Calendar"
  (interactive)
  (let ((org-icalendar-verify-function 'org-mycal-export-limit))
    (org-export-icalendar-combine-agenda-files))
  (+ 1 2))

(setq org-icalendar-store-UID t)
                                        ;(setq org-icalendar-include-todo t)
(setq org-icalendar-use-scheduled '(event-if-not-todo todo-due))
(setq org-icalendar-use-deadline '(event-if-not-todo todo-due))

(require 'org-habit)

;; Setup org-roam
;(use-package org-roam
;      :after org
;      :hook
;      ((org-mode . org-roam-mode)
;       (after-init . org-roam--build-cache-async) ;; optional!
;       )
;      :straight (:host github :repo "jethrokuan/org-roam" :branch "develop")
;      :custom
;      (org-roam-directory "~/repos/private/org")
;      :bind
;      ("C-c n l" . org-roam)
;      ("C-c n t" . org-roam-today)
;      ("C-c n f" . org-roam-find-file)
;      ("C-c n i" . org-roam-insert)
;      ("C-c n g" . org-roam-show-graph))
;
;(setq org-agenda-custom-commands
;      '(("h" "Daily habits"
;         ((agenda ""))
;         ((org-agenda-show-log t)
;          (org-agenda-ndays 7)
;          (org-agenda-log-mode-items '(state))
;          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":HABIT:"))))
;        ;; other commands here
                                        ;        ))
(setq org-agenda-custom-commands
      '(("w" "Work" tags-todo "@work")
        ("h" "Home" tags-todo "@home")
        ("n" "Next" todo "NEXT")
        ("e" "Entrepreneur" tags-todo "@entrepreneur")))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/repos/private/org/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("m" "Meeting [inbox]" entry
                               (file+headline "~/repos/private/org/inbox.org" "Meetings")
                               "* MEETING %i%? %U")
                              ("T" "Tickler" entry
                               (file+headline "~/repos/private/org/tickler.org" "Tickler")
                               "* %i%? \n %U")))
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-refile-targets '(("~/repos/private/org/gtd.org" :maxlevel . 2)
                           ("~/repos/private/org/someday.org" :level . 1)
                           ("~/repos/private/org/tickler.org" :maxlevel . 1)))

(setq org-tag-alist '( ("PROJECT" . ?p)
                       ("@work" . ?w)
                       ("@home" . ?h)
                       ("@entrepreneur" . ?e)))

;; Debug / test functions
(defun doodlebug ()
  "Nonce function"
  (interactive)
  (message "Howdie-doodie fella"))

(add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.world\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.sdf\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xacro\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.urdf\\'" . nxml-mode))


(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

(global-set-key [f5] 'recompile)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;(add-to-list 'default-frame-alist '(height . 400))
;(add-to-list 'default-frame-alist '(width . 100))

;(add-to-list 'default-frame-alist
;            '(font . "DejaVu Sans Mono-14"))
;(add-to-list 'default-frame-alist
;             '(font . "Hack-12"))

;(set-face-attribute 'default nil :font "Hack"))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))


(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(package-initialize) ;; You might already have this line

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 1)

(setq tramp-default-method "ssh")

                                        ; ;; scroll one line at a time (less "jumpy" than defaults)
                                        ; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
                                        ; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
                                        ; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
                                        ; (setq scroll-step 1) ;; keyboard scroll one line at a time
                                        ; (setq scroll-conservatively 10000)

                                        ;(add-to-list 'company-backends 'company-c-headers)

                                        ;(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flymake)
(setq cppcm-write-flymake-makefile nil)

(add-hook 'after-init-hook 'global-company-mode)
                                        ;(require 'global-company-mode)
(eval-after-load 'company
  '(push 'company-c-headers company-backends))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cpputils-cmake
;; Source: https://github.com/redguardtoo/cpputils-cmake
(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)
              )))
;; OPTIONAL, somebody reported that they can use this package with Fortran
(add-hook 'c90-mode-hook (lambda () (cppcm-reload-all)))
;; OPTIONAL, avoid typing full path when starting gdb
(global-set-key (kbd "C-c C-g")
                '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))
;; OPTIONAL, some users need specify extra flags forwarded to compiler
(setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG"))

;; Scroll to first error in compilation output
(setq compilation-scroll-output 'first-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper for compilation. Close the compilation window if there was no error
;; at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy from emacs to other programs:
; http://emacs.stackexchange.com/questions/1051/copy-region-from-emacs-without-newlines
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


;; (setq yas-snippet-dirs
;;       '("~/repos/linux-setup/emacs/snippets"
;;         ))
;; (yas-global-mode 1)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(global-set-key (kbd "C-c r") 'revert-buffer)

(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix t))

(defun dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos t))

(defun mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-mac t))


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

; Source: http://stackoverflow.com/questions/2551632/how-to-format-all-files-under-a-dir-in-emacs
(defun format-marked-files ()
  "Format files marked in Dired."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (indent-region (point-min) (point-max))
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))
    (save-buffer)
    (kill-buffer nil)))

;; Source: http://www.cslab.pepperdine.edu/warford/BatchIndentationEmacs.html
;(defun emacs-format-buffer ()
;   "Format the whole buffer."
;   (c-set-style "k&r")
;   (setq-default c-basic-offset 4
;                 tab-width 4
;                 indent-tabs-mode nil)
;   (indent-region (point-min) (point-max))
;   (delete-trailing-whitespace)
;   (untabify (point-min) (point-max))
;   (save-buffer)
;)

;(and
; (require 'centered-cursor-mode)
; (global-centered-cursor-mode +1))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;
; Recipe stuff
;;;;;;;;;;;;;;;;;;;;;;;;
(defun food/recipe-template ()
  (interactive)
  (goto-line 0)
  (search-forward "* Recipes")
  (org-meta-return)
  (org-metaright)
  (setq recipe-name (read-string "Title: "))
  (insert recipe-name)
  (org-set-tags)
  (org-meta-return)
  (org-metaright)
  (insert "ingredients")
  (org-meta-return)
  (insert "preparation")
  (org-meta-return)
  (insert "notes")
  (search-backward recipe-name)
  (org-set-property "rating" "")
  (setq source (read-string "source: "))
  (org-set-property "source" source)
  (setq servings (read-string "servings: "))
  (org-set-property "servings" servings)
  )

(defun flush-blank-lines ()
  "Removes all blank lines from buffer or region"
  (interactive)
  (save-excursion
    (let (min max)
      (if (equal (region-active-p) nil)
          (mark-whole-buffer))
      (setq min (region-beginning) max (region-end))
      (flush-lines "^ *$" min max t))))

(defun food/clear-shopping-list ()
  (interactive)
  (save-excursion
    (goto-line 0)
    (let ((start-shopping-list (search-forward "* Shopping List" nil t)))
      (show-subtree)
      (outline-next-visible-heading 1)
      (previous-line)
      (end-of-line)
      (kill-region start-shopping-list (point)))))

(defun food/gen-shopping-list ()
  (interactive)
  (food/clear-shopping-list)
  (goto-line 0)
  (let ((start-shopping-list (search-forward "* Shopping List" nil t)))
    (while (search-forward "** TOCOOK" nil t)
      (show-subtree)
      (outline-next-visible-heading 1)
      (next-line)
      (let ((start (point)))
        (outline-next-visible-heading 1)
        ;;(previous-line)
        (copy-region-as-kill start (point)))
      (save-excursion
        (goto-char start-shopping-list)
        (newline)
        (yank)
        (show-subtree)
        (delete-blank-lines)))
    (goto-char start-shopping-list)
    (org-mark-subtree)
    (next-line)
    (flush-blank-lines))
  (org-table-align)
  (previous-line)
  (org-shifttab))

                                        ; Neotree
(setq neo-smart-open t)

; Delete trailing white space on save in all programming modes
(defun my:delete-trailing-whitespace-before-save ()
  "Add `delete-trailing-whitespace' to the local `after-save-hook'."
  (add-hook (make-local-variable 'before-save-hook)
            'delete-trailing-whitespace))

(add-hook 'prog-mode-hook 'my:delete-trailing-whitespace-before-save)
(add-hook 'text-mode-hook 'my:delete-trailing-whitespace-before-save)

(add-hook 'python-mode-hook '(lambda ()
                               (setq python-indent 4)))

; Enable python in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

; Don't ask to evaluate code blocks in org-mode
(custom-set-variables
 '(org-babel-load-languages (quote ((emacs-lisp . t) (python . t) (sh . t) (latex . t))))
 '(org-confirm-babel-evaluate nil))

; Enable changing size of code listings
(require 'ox-latex)
(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

(setq org-src-fontify-natively t) ; syntax highlighting in org-mode

(setq org-src-tab-acts-natively t) ; indent code in org-mode

;;; Use minted for code listings, Include minted in the default package list,
;;; Change the compilation switches to accommodate for minted:
;;(setq org-latex-listings 'minted
;;      org-latex-packages-alist '(("" "minted"))
;;      org-latex-pdf-process
;;      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-publish-project-alist
  '(("html"
     :base-directory "~/org/"
     :base-extension "org"
     :publishing-directory "~/org/exports"
     :publishing-function org-publish-org-to-html)
    ("pdf"
     :base-directory "~/org/"
     :base-extension "org"
     :publishing-directory "~/org/exports"
     :publishing-function org-publish-org-to-pdf)
    ("all" :components ("html" "pdf"))))

; Create an export_tex directory to hold pdf temporary files during org-mode
(defvar org-export-output-directory-prefix "export_" "prefix of directory used for org-mode export")
; latex -> pdf generation
(defadvice org-export-output-file-name (before org-add-export-dir activate)
  "Modifies org-export to place exported files in a different directory"
  (when (not pub-dir)
    (setq pub-dir (concat org-export-output-directory-prefix (substring extension 1)))
    (when (not (file-directory-p pub-dir))
      (make-directory pub-dir))))

(add-hook 'org-mode-hook 'flyspell-mode)
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

(setq ispell-personal-dictionary "~/repos/dotfiles/emacs/aspell.en.pws")

; Use org-ref with org-mode for references
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %o/%b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-logfiles-extensions (quote ("asdf")))

; org-ref configuration
(require 'org-ref)
(setq reftex-default-bibliography '("~/repos/private/org/roam/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/repos/private/org/roam/references-notes.org"
      org-ref-default-bibliography '("~/repos/private/org/roam/references.bib"))

; helm-bibtex configuration
(setq bibtex-completion-bibliography "~/repos/private/org/roam/references.bib")

(require 'org-ref-pdf)
(require 'org-ref-url-utils)
(require 'org-ref-latex)

;(setq org-ref-pdf-directory "~/Documents/pdfs")

(require 'taskjuggler-mode)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(tool-bar-mode -1)

;;; scrollers (scroll buffer not point)
(global-set-key "\M-n" "\C-u5\C-v")
(global-set-key "\M-p" "\C-u5\M-v")

;; multi-term configuration
(require 'multi-term)
(setq multi-term-program "/bin/bash")


(require 'helm-mt)
(global-set-key (kbd "C-x t") 'helm-mt)

;; Allow easier resizing of windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Reveal.js + org-mode
(require 'ox-reveal)
(setq org-reveal-root "file:///home/syllogismrxs/repos/3rd-party/reveal.js-3.8.0")
;(setq org-reveal-title-slide nil)

;; Setup textlint
;; Ref: https://www.macs.hw.ac.uk/~rs46/posts/2018-12-29-textlint-flycheck.html
(require 'flycheck)
(flycheck-define-checker textlint
                         "A linter for textlint."
                         :command ("npx" "textlint"
                                   "--config" "/home/syllogismrxs/repos/dotfiles/emacs/textlintrc"
                                   "--format" "unix"
                                   "--rule" "write-good"
                                   "--rule" "no-start-duplicated-conjunction"
                                   "--rule" "max-comma"
                                   "--rule" "terminology"
                                   "--rule" "period-in-list-item"
                                   "--rule" "abbr-within-parentheses"
                                   "--rule" "alex"
                                   "--rule" "common-misspellings"
                                   "--rule" "en-max-word-count"
                                   "--rule" "diacritics"
                                   "--rule" "stop-words"
                                   "--plugin"
                                   (eval
                                    (if (derived-mode-p 'tex-mode)
                                        "latex"
                                      "@textlint/text"))
                                   source-inplace)
                         :error-patterns
                         ((warning line-start (file-name) ":" line ":" column ": "
                                   (message (one-or-more not-newline)
                                            (zero-or-more "\n" (any " ") (one-or-more not-newline)))
                                   line-end))
                         :modes (text-mode latex-mode org-mode markdown-mode)
                         )
(add-to-list 'flycheck-checkers 'textlint)

(require 'pddl-mode)
(add-to-list 'auto-mode-alist '("\\.pddl\\'" . PDDL-mode))


;;; org-roam
;(add-to-list 'load-path "~/.emacs.d/repos/org-roam")
;(require 'org-roam)
;(setq org-roam-directory "~/repos/private/org/roam")
;(define-key org-roam-mode-map (kbd "C-c n l") #'org-roam)
;(define-key org-roam-mode-map (kbd "C-c n f") #'org-roam-find-file)
;(define-key org-roam-mode-map (kbd "C-c n b") #'org-roam-switch-to-buffer)
;(define-key org-roam-mode-map (kbd "C-c n g") #'org-roam-switch-to-buffer)
;(define-key org-roam-mode-map (kbd "C-c n t") #'org-roam-today)
;(define-key org-mode-map (kbd "C-c n i") #'org-roam-insert)
;(org-roam-mode +1)

(require 'deft)
(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/repos/private/org/roam")
(setq deft-recursive t)
