  (setq ispell-dictionary "english")

  (setq calendar-week-start-day 1)

  (setq org-beamer-outline-frame-title "Gliederung")

(add-hook 'org-mode 'custom-date-format)
(defun custom-date-format ()
  "Set larger-font"
  (interactive)
  (setq org-time-stamp-custom-formats '("%d.%m.%Y %a" . "%d.%m.%Y %a %H:%M"))
  )

  (setq user-full-name "Benjamin Schichtholz"
      user-mail-address "schichtholz@mailbox.org")

  (setq doom-font "IBM Plex Mono:pixelsize=18"
        doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :style "Regular" :size 19 :weight 'regular)
        doom-unicode-font "Source Code Pro:pixelsize=18")

(add-hook 'compilation-mode-hook
          (lambda()
              (text-scale-decrease 0)
))

  (setq doom-unicode-font "Iosevka")

  (setq doom-theme 'doom-old-hope)

  (setq display-line-numbers-type 'relative)
  (setq-default tab-width 2)
  (setq line-spacing 0.2)

(setq frame-title-format "%b [Emacs]")
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  (setq org-directory "~/Org/")
  (setq org-hide-emphasis-markers t)
  (setq org-image-actual-width '(300))
  (setq org-toggle-pretty-entitites t)

  (add-hook 'org-mode-hook
            (lambda ()
              (writeroom-mode)
              (org-superstar-mode 1)
              (org-appear-mode)))

   (setq org-superstar-headline-bullets-list '(?❂ ?✽ ?☉ ?✶))

  (custom-set-faces
    '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
    '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
    '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
    '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
    '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
  )

  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
  (add-to-list 'auto-mode-alist '("\\rc\\'" . conf-mode))
  (add-to-list 'auto-minor-mode-alist '("\\.pdf" . hide-mode-line-mode))

  (global-set-key (kbd "C-x x") 'kill-current-buffer)
  (global-set-key (kbd "<mouse-8>") 'switch-to-prev-buffer)
  (global-set-key (kbd "<mouse-9>") 'switch-to-next-buffer)
  (map! :leader
        :desc "Launch Eshell"
        "o E" #'eshell)

  (map! :leader
        :desc "Launch Mu4e"
        "o m" #'mu4e)
  (map! :leader
        :desc "Launch Shell"
        "s h" #'shell)
  (map! :leader
        :desc "Export latex beamer to pdf"
        "l b" #'org-beamer-export-to-pdf)
  (map! :leader
        :desc "Hide Modeline"
        "m h" #'hide-mode-line-mode)

  (map! :leader
        :desc "Deepl Translate"
        "d" #'txl-translate-region-or-paragraph)

  (map! :leader
        :desc "Toggle Writeroom"
        "t Z" #'toggle-writeroom-mode)


  ;;(add-hook 'PDFView
  ;;      (lambda () (local-set-key (kbd "b") #'pdf-history-backward))
  ;;)
(defun toggle-writeroom-mode ()
  "Toggle writeroom-mode on or off."
  (interactive)
  (if writeroom-mode
      (writeroom-mode -1) ; If writeroom-mode is active, disable it.
    (writeroom-mode 1))) ; If writeroom-mode is inactive, enable it.

  (setq doom-modeline-bar-width 7)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-height 1)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-buffer-state-icon nil)

  (after! doom-modeline
    (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  "))) ; <-- added padding here

(setq doom-fallback-buffer-name "*dashboard*")

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

  (setq shell-file-name "/run/current-system/sw/bin/bash")

(require 'txl)
(setq txl-languages '(FR . EN-GB))
(setq txl-deepl-api-key "424c308a-a3cb-343c-840d-9c905fbd640d:fx")
(setq txl-deepl-api-url "https://api-free.deepl.com/v2/translate")

  (add-to-list 'company-backends 'company-irony)

  (setq irony-server-install-prefix "$HOME/.nix-profile")

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "Welcome!")
(setq dashboard-startup-banner "~/Pictures/logos/Tux_78.png")
(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-navigator t)
(setq dashboard-agenda-time-string-format "%d.%m.%Y")

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 15)
                        (projects . 5)))

(setq all-the-icons-dired-monochrome nil)

(setq dired-listing-switches "-aghD --time-style=locale --group-directories-first")

(add-hook 'dired-mode-hook
          (lambda()
            (writeroom-mode)
))

(setq dired-find-subdir t)

(add-to-list '+lookup-provider-url-alist '("Oxford Dictionary" "https://www.oxfordlearnersdictionaries.com/definition/english/%s"))

(setq rfc-mode-directory (expand-file-name "~/rfc/"))

(add-hook 'rfc-mode-hook
          (lambda()
              (text-scale-increase 1)
))

(setq writeroom-border-width 10)
(setq writeroom-width 115)
(setq writeroom-fullscreen-effect 'maximized)
(setq writeroom-maximize-window nil)

(add-hook 'writeroom-mode-enable-hook
          (lambda()
            (text-scale-increase 1)
))

(add-hook 'writeroom-mode-disable-hook
          (lambda()
            (text-scale-decrease 1)
))

(setq bibtex-completion-bibliography '("~/Code/ixp_scrubber/paper/literature.bib"))

(setq! citar-bibliography '("~/Code/ixp_scrubber/paper/literature.bib" "~/Code/sdn-report/report/literature.bib"))

(require 'simplenote2)
(setq simplenote2-email "benholz@mailbox.org")
(setq simplenote2-password "Fi37b4f9&vx9*")
(simplenote2-setup)

(add-hook 'simplenote2-note-mode-hook
          (lambda ()
            (markdown-mode)))
