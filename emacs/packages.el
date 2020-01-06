(require 'package)

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")


; list the packages you want
(setq package-list
      '(flycheck company elpy
        yasnippet auto-complete popup centered-cursor-mode dash
        flymake markdown-mode org-journal vlf helm cpputils-cmake neotree
        visual-fill-column magit helm org-ref helm-projectile))

; activate all the packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
