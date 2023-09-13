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

;;(if (facep 'mode-line-active)
;;    (set-face-attribute 'mode-line-active nil :family "IBM Plex Mono" :height 100) ; For 29+
;;    (set-face-attribute 'mode-line nil :family "IBM Plex Mono" :height 100))
;;    (set-face-attribute 'mode-line-inactive nil :family "IBM Plex Mono" :height 100)

;; (add-hook 'text-mode-hook
;;           (lambda() (set-face-attribute 'italic nil :family "Fira Sans" :width 'condensed :slant 'italic)
;; ))

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

;;(setq lean-rootdir "$HOME/.local/bin")

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

  ;;(add-hook 'PDFView
  ;;      (lambda () (local-set-key (kbd "b") #'pdf-history-backward))
  ;;)

  (setq fancy-splash-image "~/Pictures/logos/black-hole-doom.png")

  (setq doom-modeline-bar-width 7)
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

  ;;(setq  lsp-clients-kotlin-server-executable "~/Code/kotlin-language-server/server/build/install/server/bin/kotlin-language-server")

  (setq racer-rust-src-path "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust")

  (add-to-list 'company-backends 'company-irony)

  ;;(add-hook 'java-mode-hook #'lsp)

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

(setq dashboard-items '((recents  . 8)
                        (bookmarks . 15)
                        (projects . 5)))

;;(defun dashboard-insert-custom (list-size)
;;(insert "Custom text"))
;;(add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
;;(add-to-list 'dashboard-items '(Text) t)

(setq dired-listing-switches "-agGh --time-style=locale --group-directories-first")

(add-hook 'dired-mode 'dired-larger-font)
 (defun dired-larger-font ()
   "Set larger-font"
   (interactive)
   (setq buffer-face-mode-face (font-spec :family "Inconsolata" :height 100 :weight 'regular))
   ;(writeroom-mode)
   ;(dired-hide-details-mode)
   (buffer-face-mode))

(add-to-list '+lookup-provider-url-alist '("Oxford Dictionary" "https://www.oxfordlearnersdictionaries.com/definition/english/%s"))

(setq rfc-mode-directory (expand-file-name "~/rfc/"))

(add-hook 'rfc-mode-hook
          (lambda()
              (text-scale-increase 1)
))

(setq bibtex-completion-bibliography '("~/Code/ixp_scrubber/paper/literature.bib"))

(setq! citar-bibliography '("~/Code/ixp_scrubber/paper/literature.bib" "~/Code/sdn-report/report/literature.bib"))

(require 'simplenote2)
(setq simplenote2-email "benholz@mailbox.org")
(setq simplenote2-password "apHmY$K$7@54B")
(simplenote2-setup)

(add-hook 'simplenote2-note-mode-hook
          (lambda ()
            (markdown-mode)))
