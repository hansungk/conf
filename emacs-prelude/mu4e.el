;; mu4e configuration
;;
;; Refs:
;; * Doom mu4e module manual
;; * mu4e for dummies [https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/?utm_source=share&utm_medium=web2x]
;; * https://www.chrislockard.net/posts/2019-11-14-notes-on-configuring-mu4e-and-doom-emacs/

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(defun my-mu4e-main-mode-hook ()
  (setq user-mail-address "hansung_kim@berkeley.edu")
  (setq mu4e-bookmarks '(( :name "Unread messages"
                                 :query "flag:unread AND NOT flag:trashed AND NOT maildir:/trash"
                                 :key ?u)
                         ( :name "Today's messages"
                                 :query "date:today..now"
                                 :key ?t)
                         ( :name "Last 7 days"
                                 :query "date:7d..now"
                                 :hide-unread t
                                 :key ?w)))
  (setq mu4e-sent-folder "/sent")
  (setq mu4e-drafts-folder "/drafts")
  (setq mu4e-trash-folder "/trash")
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-index-update-in-background t)
  (setq mu4e-compose-signature-auto-include t)
  (setq mu4e-use-fancy-chars nil)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-view-show-images t)
  (setq mu4e-compose-format-flowed t)
  ;; (setq mu4e-compose-in-new-frame t)

  ;; Fixes duplicate UID errors; see
  ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
  (setq mu4e-change-filenames-when-moving t)

  (setq message-send-mail-function 'smtpmail-send-it)
  ;; (add-to-list 'mu4e-bookmarks
  ;;              '( :name "Unread messages"
  ;;                 :query "flag:unread AND NOT flag:trashed AND NOT maildir:/trash"
  ;;                 :key ?u))
  )
(add-hook 'mu4e-main-mode-hook 'my-mu4e-main-mode-hook)

;; Evil stuff
;; (evil-set-initial-state 'mu4e-view-mode 'normal)
                                        ; Fixes switcing to insert state when entering the header area.
                                        ; See https://github.com/emacs-evil/evil-collection/issues/345 and check if
                                        ; fixed.
;; (evil-set-initial-state 'mu4e-compose-mode 'normal)
;; (add-hook! mu4e-compose-mode
;;            (evil-normal-state))

;; (add-hook! org-mu4e-compose-org-mode
;;            (message "HIHIHIH")
;;            (evil-normal-state))

;; (set-email-account! "berkeley.edu"
;;   '((mu4e-sent-folder       . "/sent")
;;     ;; (mu4e-drafts-folder     . "/[Gmail]/Drafts")
;;     (mu4e-trash-folder      . "/trash")
;;     ;; (mu4e-refile-folder     . "/[Gmail]/All Mail")
;;     (smtpmail-smtp-user     . "hansung_kim@berkeley.edu")
;;     (smtpmail-smtp-server   . "smtp.gmail.com")
;;     (smtpmail-smtp-service  . 587)
;;     (smtpmail-stream-type   . starttls)
;;     (smtpmail-debug-info    . t))
;;   t)
