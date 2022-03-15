;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; run 'doom sync' after modifying this file!

;; ispell-configuration
(setq ispell-dictionary "german")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Benjamin Schichtholz"
      user-mail-address "benjami.schichtholz@mni.thm.de")

;; Font configuration
(setq doom-font (font-spec :family "Inconsolata" :size 21 :weight 'regular)
       doom-variable-pitch-font (font-spec :family "Roboto" :style "Regular" :size 12 :weight 'regular))

(add-hook 'text-mode-hook
          (lambda() (set-face-attribute 'italic nil :family "Liberation Mono" :height 0.9 :width 'condensed :slant 'italic)
))

;; ui theme
;;(setq doom-theme 'doom-monokai-spectrum)
;;(setq doom-theme 'doom-Iosvkem)
(setq doom-theme 'doom-peacock)

;; org-directory
(setq org-directory "~/Documents/Org/")

;; line numbers
(setq display-line-numbers-type 'relative)

;; tab width
(setq-default tab-width 2)

;; various hooks
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'org-mode-hook
           (lambda ()
             (org-superstar-mode 1)))


(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; hide markers for italic, bold (...) text in org mode
(setq org-hide-emphasis-markers t)

;; custom keybindings
(global-set-key (kbd "C-x x") 'kill-this-buffer)

;; doom modeline configuration
(setq doom-modeline-bar-width 7)
(setq doom-modeline-height 40)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-modification-icon t)
(setq doom-modeline-height 1)
(setq doom-modeline-modal-icon t)
(setq doom-modeline-enable-word-count t)
(after! doom-modeline
(doom-modeline-def-modeline 'main
'(bar matches buffer-info remote-host buffer-position parrot selection-info)
'(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  "))) ; <-- added padding here
(setq
   all-the-icons-scale-factor 1.1)

;; set monday as first day in calendar
(setq calendar-week-start-day 1)

;; custom splash image
(setq fancy-splash-image "~/Pictures/logos/black-hole-doom.png")

;; configure eshell
(map! :leader
      :desc "Launch Eshell"
      "o E" #'eshell)

(setq
 eshell-prompt-function (lambda nil
    (concat
     (propertize (eshell/pwd) 'face `(:foreground "#67e2e9"))
     (propertize " $ " 'face `(:foreground "#67ff7b"))))
  eshell-highlight-prompt nil
  eshell-banner-message
        '(format "%s %s\n"
        (propertize (format " %s " (string-trim (buffer-name)))
                'face 'mode-line-highlight)
        (propertize (current-time-string)
        'face 'font-lock-keyword-face))
)
;; mail configuration
(set-email-account! "thm"
                      '((mu4e-sent-folder       . "/THM/Sent")
                        (mu4e-drafts-folder     . "/THM/Drafts")
                        (mu4e-trash-folder      . "/THM/Trash")
                        (mu4e-refile-folder     . "/THM/Archive")
                        (smtpmail-smtp-user     . "bhlz54")
                        (user-mail-address      . "benjamin.schichhtolz@mni.thm.de")
                        (user-full-name         . "Benjamin Schichtholz"))
                t
)
;; Here are some additional functions/macros that could help you configure Doom:
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
