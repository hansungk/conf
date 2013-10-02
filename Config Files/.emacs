;;; PACKAGE
; Use Marmalade
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-mode-hook (quote (turn-on-haskell-indent turn-on-haskell-doc-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; DISPLAY
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

; Set korean font
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                  '("NanumGothicCoding" . "iso10646-1"))
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
                  '("NanumGothicCoding" . "iso10646-1"))

;;; MISC
; Enable external X clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;; EVIL mode
(evil-mode -1)

; Change ESC key
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-normal-state-map "kj" 'evil-force-normal-state)
(key-chord-define evil-visual-state-map "kj" 'evil-change-to-previous-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-define evil-replace-state-map "kj" 'evil-normal-state)
