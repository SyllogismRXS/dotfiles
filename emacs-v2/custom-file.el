(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (\` (("." \, (concat user-emacs-directory "backups")))))
 '(confirm-kill-processes nil)
 '(indent-tabs-mode nil)
 '(org-agenda-files
   (quote
    ("~/repos/private/org/inbox.org" "~/repos/private/org/gtd.org" "~/repos/private/org/schedule.org" "/home/syllogismrxs/repos/private/org/journal/2020-12-18.org")))
 '(package-archives
   (quote
    (("marmalade" . "https://marmalade-repo.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("elpa" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (helm-tramp lua-mode markdown-mode nxml-mode auctex LaTeX/P visual-fill-column helm-ag flycheck-google-cpplint google-c-style org-gcal helm-projectile projectile helm-mt multi-term org-journal deft company magit helm which-key smooth-scrolling doom-themes all-the-icons use-package)))
 '(use-package-always-defer t)
 '(use-package-always-ensure nil)
 '(use-package-verbose nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-builtin-face))))
 '(font-lock-comment-face ((t (:inherit font-lock-builtin-face)))))
