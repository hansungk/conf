;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Hansung Kim"
      user-mail-address "hansung_kim@berkeley.edu")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font
      (if (eq system-type 'darwin)
          (font-spec :family "Menlo" :size 14)
          (font-spec :family "Hack" :size 10.0 :weight 'normal)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme (if (eq system-type 'darwin)
                     'doom-nord-light
                     'doom-tomorrow-night))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
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

;; No italic face, hopefully.
(set-face-italic-p 'italic nil)
;; Turn off cursorline highlight.
(add-hook! hl-line-mode
  :init (global-hl-line-mode -1))

(add-hook! prog-mode
  (smartparens-global-mode -1))

(add-hook! (c-mode c++-mode)
  (setq flycheck-disabled-checkers '(c/c++-clang c/c++-gcc c/c++-cppcheck))
  (smartparens-global-mode -1))
(add-hook! (c-mode c++-mode) #'lsp)

(use-package! lsp-mode
  :commands lsp
  :init
  (progn
    (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-sideline-enable nil)
    (setq lsp-diagnostic-package :flymake)
    (setq lsp-clients-clangd-args '("-j=7" "--background-index" "--cross-file-rename"))
    )
  :config
  (progn
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                      :major-modes '(c++-mode)
                      :remote? t
                      :server-id 'clangd-remote))
    ;; (setq lsp-enable-xref t)
    ;; (flycheck-add-mode 'lsp 'c++-mode)
    ;; (setq flycheck-checker 'lsp)
    ))

(after! smartparens
  (smartparens-global-mode -1))

(define-key doom-leader-map (kbd "=") 'lsp-format-region)
(define-key evil-motion-state-map (kbd "C-s") 'swiper)
(global-set-key (kbd "C-s") 'swiper)

;; mu4e configuration
;; Refs:
;; * Doom mu4e module manual
;; * mu4e for dummies [https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/?utm_source=share&utm_medium=web2x]
;; * https://www.chrislockard.net/posts/2019-11-14-notes-on-configuring-mu4e-and-doom-emacs/
(after! mu4e
  (setq! mu4e-maildir (expand-file-name "/home/hansung/.mutt/mailbox") ; the rest of the mu4e folders are RELATIVE to this one
         mu4e-bookmarks '(( :name "Unread messages"
                           :query "flag:unread AND NOT flag:trashed AND NOT maildir:/trash"
                           :key ?u)
                          ( :name "Today's messages"
                            :query "date:today..now"
                            :key ?t)
                          ( :name "Last 7 days"
                            :query "date:7d..now"
                            :hide-unread t
                            :key ?w))
         mu4e-sent-folder "/sent"
         mu4e-drafts-folder "/drafts"
         mu4e-trash-folder "/trash"
         mu4e-get-mail-command "mbsync -a"
         mu4e-index-update-in-background t
         mu4e-compose-signature-auto-include t
         mu4e-use-fancy-chars t
         mu4e-view-show-addresses t
         mu4e-view-show-images t
         mu4e-compose-format-flowed t
         ;mu4e-compose-in-new-frame t
         mu4e-change-filenames-when-moving t ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/

         ;; Message Formatting and sending
         message-send-mail-function 'smtpmail-send-it
         )
  ;; (add-to-list 'mu4e-bookmarks
  ;;              '( :name "Unread messages"
  ;;                 :query "flag:unread AND NOT flag:trashed AND NOT maildir:/trash"
  ;;                 :key ?u))
  (evil-set-initial-state 'mu4e-view-mode 'normal)
  ; Fixes switcing to insert state when entering the header area.
  ; See https://github.com/emacs-evil/evil-collection/issues/345 and check if
  ; fixed.
  (evil-set-initial-state 'mu4e-compose-mode 'normal))

(add-hook! mu4e-compose-mode
           (evil-normal-state))

(add-hook! org-mu4e-compose-org-mode
           (message "HIHIHIH")
           (evil-normal-state))

(set-email-account! "berkeley.edu"
  '((mu4e-sent-folder       . "/sent")
    ;; (mu4e-drafts-folder     . "/[Gmail]/Drafts")
    (mu4e-trash-folder      . "/trash")
    ;; (mu4e-refile-folder     . "/[Gmail]/All Mail")
    (smtpmail-smtp-user     . "hansung_kim@berkeley.edu")
    (smtpmail-smtp-server   . "smtp.gmail.com")
    (smtpmail-smtp-service  . 587)
    (smtpmail-stream-type   . starttls)
    (smtpmail-debug-info    . t))
  t)

; (add-to-list 'custom-theme-load-path "~/.doom.d/themes/")
