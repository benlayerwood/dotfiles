  (setq ispell-dictionary "german")

  (setq calendar-week-start-day 1)

  (setq org-beamer-outline-frame-title "Gliederung")

  (setq user-full-name "Benjamin Schichtholz"
      user-mail-address "schichtholz@mailbox.org")

  (setq doom-font (font-spec :family "Inconsolata" :size 21 :weight 'regular)
        doom-variable-pitch-font (font-spec :family "Source Sans Pro" :style "Regular" :size 21 :weight 'regular))

  (add-hook 'text-mode-hook
            (lambda() (set-face-attribute 'italic nil :family "Liberation Mono" :height 0.9 :width 'condensed :slant 'italic)
  ))

  ;;(setq doom-theme 'doom-monokai-spectrum)
  ;;(setq doom-theme 'doom-Iosvkem)
  ;;(setq doom-theme 'doom-peacock)
  ;;(setq doom-theme 'doom-material-dark)
  (setq doom-theme 'doom-old-hope)

  (setq display-line-numbers-type 'relative)
  (setq-default tab-width 2)
  (setq line-spacing 0.2)

  (setq org-directory "~/Documents/Org/")
  (setq org-hide-emphasis-markers t)

  (add-hook 'org-mode-hook
            (lambda ()
              (org-superstar-mode 1)))

  (custom-set-faces
    '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
    '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
    '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
    '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
    '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
  )

  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
  (add-to-list 'auto-mode-alist '("\\rc\\'" . conf-mode))
  (add-to-list 'auto-minor-mode-alist '("android-stack.png" . hide-mode-line-mode))

  (global-set-key (kbd "C-x x") 'kill-this-buffer)
  (map! :leader
        :desc "Launch Eshell"
        "o E" #'eshell)
  (map! :leader
        :desc "Launch Shell"
        "s h" #'shell)
  (map! :leader
        :desc "Export latex beamer to pdf"
        "l b" #'org-beamer-export-to-pdf)
  (map! :leader
        :desc "Hide Modeline"
        "m h" #'hide-mode-line-mode)

  (setq fancy-splash-image "~/Pictures/logos/black-hole-doom.png")

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

  (setq shell-file-name "/bin/bash")

  (require 'mu4e)

  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "mailbox"
            :match-func (lambda (msg)
                          (when msg
                            (string-prefix-p "/Mailbox" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "schichtholz@mailbox.org")
                    (mu4e-trash-folder . "/Mailbox/Trash")
                    (mu4e-refile-folder . "/Mailbox/Archive")
                    (mu4e-sent-folder . "/Mailbox/Sent")
                    (mu4e-drafts-folder . "/Mailbox/Drafts"))),

        (make-mu4e-context
            :name "kit"
            :match-func (lambda (msg)
                            (when msg
                            (string-prefix-p "/KIT" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "benjamin.schichtholz@student.kit.edu")
                    (mu4e-trash-folder . "/KIT/Gel&APY-schte Elemente")
                    (mu4e-refile-folder . "/KIT/Journal")
                    (mu4e-sent-folder . "/KIT/Gesendete Elemente")
                    (mu4e-drafts-folder . "/KIT/Entw&APw-rfe")))))

(defvar my-mu4e-account-alist
  '(("mailbox"
     (mu4e-sent-folder "/Mailbox/Sent")
     (mu4e-drafts-folder "/Mailbox/Drafts")
     (user-mail-address "schichtholz@mailbox.org")
     (smtpmail-default-smtp-server "smtp.mailbox.org")
     (smtpmail-local-domain "mailbox.org")
     (smtpmail-smtp-user "schichtholz@mailbox.org")
     (smtpmail-smtp-server "smtp.mailbox.org")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587))
    ("kit"
     (mu4e-sent-folder "/KIT/Gesendete Elemente")
     (mu4e-drafts-folder "/KIT/Entw&APw-rfe")
     (user-mail-address "benjamin.schichtholz@student.kit.edu")
     (smtpmail-default-smtp-server "smtp.kit.edu")
     (smtpmail-local-domain "smtp.kit.edu")
     (smtpmail-smtp-user "upqgd@student.kit.edu")
     (smtpmail-smtp-server "smtp.kit.edu")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

  (setq mu4e-get-mail-command "mbsync -a")

  (setq mu4e-update-interval 300)

(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
  org-msg-startup "hidestars indent inlineimages"
  org-msg-default-alternatives '((new . (text))
  (reply-to-html . (text html))
  (reply-to-text . (text)))
  org-msg-convert-citation t
)
(org-msg-mode)

  (setq mu4e-compose-signature "Mit freundlichen Grüßen,\nBenjamin Schichtholz")
