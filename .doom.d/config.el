;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; run 'doom sync' after modifying this file!

;; ispell-configuration
(setq ispell-dictionary "german")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Benjamin Schichtholz"
      user-mail-address "schichtholz@mailbox.org")
;; Font configuration
(setq doom-font (font-spec :family "Inconsolata" :size 21 :weight 'regular)
       doom-variable-pitch-font (font-spec :family "Liberation Sans" :style "Regular" :size 18 :weight 'regular))

(add-hook 'text-mode-hook
          (lambda() (set-face-attribute 'italic nil :family "Liberation Mono" :height 0.9 :width 'condensed :slant 'italic)
))

;; ui theme
;;(setq doom-theme 'doom-monokai-spectrum)
(setq doom-theme 'doom-Iosvkem)
;;(setq doom-theme 'doom-peacock)

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
(add-to-list 'auto-mode-alist '("\\rc\\'" . conf-mode))

;; hide markers for italic, bold (...) text in org mode
(setq org-hide-emphasis-markers t)

;; custom keybindings
(global-set-key (kbd "C-x x") 'kill-this-buffer)
(map! :leader
      :desc "Launch Eshell"
      "o E" #'eshell)
(map! :leader
      :desc "Launch Shell"
      "s h" #'shell)

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
                        (user-mail-address      . "benjamin.schichtholz@mni.thm.de")
                        (user-full-name         . "Benjamin Schichtholz")
                        (smtpmail-smtp-server   . "mailgate.thm.de")
                        (smtpmail-smtp-service  . 587)
                        (smtpmail-stream-type   . starttls)
                        (smtpmail-auth-supported . 'password)
                        )
                t
)
(set-email-account! "mailbox"
                      '((mu4e-sent-folder       . "/Mailbox/Sent")
                        (mu4e-drafts-folder     . "/Mailbox/Drafts")
                        (mu4e-trash-folder      . "/Mailbox/Trash")
                        (mu4e-refile-folder     . "/Mailbox/Archive")
                        (user-full-name         . "Benjamin Schichtholz")
                        (user-mail-address      . "schichtholz@mailbox.org")
                        (smtpmail-smtp-user     . "schichtholz@mailbox.org")
                        (smtpmail-smtp-server   . "smtp.mailbox.org")
                        )
                t
)

;;------------------------------------------------
;;------------MULTI SMTP CONFIGURATION------------
;;------------------------------------------------
;; Configure known SMTP servers. Emacs prompts for passwords and saves them in ~/.authinfo
(setq smtp-accounts          ;; Format: Sender Mail address - SMTP Server - Port - Username
      '(("schichtholz@mailbox.org" "smtp.mailbo.org" 587 "schichtholz@mailbox.org")
        ("benjamin.schichtholz@mni.thm.de" "mailgate.thm.de" 587 "bhlz54")
        ))

;; Set the SMTP Server according to the mail address we use for sending
(defun set-smtp-server-message-send-and-exit ()
  "Set SMTP server from list of multiple ones and send mail."
  (interactive)
  (message-remove-header "X-Message-SMTP-Method") ;; Remove. We always determine it by the From field
  (let ((sender
         (message-fetch-field "From")))
    (loop for (addr server port usr) in smtp-accounts
          when (string-match addr sender)
          do (message-add-header (format "X-Message-SMTP-Method: smtp %s %d %s" server port usr)))
    (let ((xmess
           (message-fetch-field "X-Message-SMTP-Method")))
      (if xmess
          (progn
            (message (format "Sending message using '%s' with config '%s'" sender xmess))
            (message-send-and-exit))
        (error "Could not find SMTP Server for this Sender address: %s. You might want to correct it or add it to the SMTP Server list 'smtp-accounts'" sender)))))

;; Send emails via multiple servers
(defun local-gnus-compose-mode ()
  "Keys."
  (local-set-key (kbd "C-c C-c")  'set-smtp-server-message-send-and-exit))

;; set in group mode hook
(add-hook 'gnus-message-setup-hook 'local-gnus-compose-mode)

;; Set the SMTP Server according to the mail address we use for sending
(defun set-smtp-server-message-send-and-exit ()
  "Set SMTP server from list of multiple ones and send mail."
  (interactive)
  (message-remove-header "X-Message-SMTP-Method") ;; Remove. We always determine it by the From field
  (let ((sender
         (message-fetch-field "From")))
    (loop for (addr server port usr) in smtp-accounts
          when (string-match addr sender)
          do (message-add-header (format "X-Message-SMTP-Method: smtp %s %d %s" server port usr)))
    (let ((xmess
           (message-fetch-field "X-Message-SMTP-Method")))
      (if xmess
          (progn
            (message (format "Sending message using '%s' with config '%s'" sender xmess))
            (message-send-and-exit))
        (error "Could not find SMTP Server for this Sender address: %s. You might want to correct it or add it to the SMTP Server list 'smtp-accounts'" sender)))))

;; Send emails via multiple servers
(defun local-gnus-compose-mode ()
  "Keys."
  (local-set-key (kbd "C-c C-c")  'set-smtp-server-message-send-and-exit))
;;------------------------------------------------

  ;; set in group mode hook
  (add-hook 'gnus-message-setup-hook 'local-gnus-compose-mode)
;; org-msg configuration
;; Documentation: https://github.com/jeremy-compostella/org-msg
(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-default-alternatives '((new		. (text))
				       (reply-to-html	. (text html))
				       (reply-to-text	. (text)))
	org-msg-convert-citation t
)
(setq message-signature-separator "^$")
(setq gnus-signature-separator "^$")
(setq mu4e-compose-signature "Mit freundlichen Grüßen,\nBenjamin Schichtholz")
(org-msg-mode)

(setq smtpmail-debug-info t)
(setq line-spacing 0.2)

;; org mode headings
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

;; Bash as default shell
(setq shell-file-name "/bin/bash")

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
