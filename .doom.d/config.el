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

(unless (boundp 'myconf-magit-hook?)
  ;; Only run this hook once, even if Emacs reloads configuration.
  (eval-after-load 'magit
    '(let ((myconf-path (expand-file-name "%HOME/.cfg/")))
       (when (and (file-exists-p myconf-path))
         ;; Insert git directory and working tree into magit's git
         ;; global arguments, while preserving magit's existing
         ;; command-line settings; `add-to-list' adds to the
         ;; beginning of the list.
         (add-to-list 'magit-git-global-arguments
                      (format "--work-tree=$HOME"
                              ;; Drop trailing slash.
                              (directory-file-name
                               ;; Get directory part (`dirname`).
                               (file-name-directory myconf-path))))
         (add-to-list 'magit-git-global-arguments
                      (format "--git-dir=$HOME/.cfg/" myconf-path)))))
  (setq myconf-magit-hook? t))

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

  (setq mu4e-get-mail-command "mbsync -a")

  (setq mu4e-update-interval 300)

  (setq mu4e-compose-signature "Mit freundlichen Grüßen,\nBenjamin Schichtholz")
