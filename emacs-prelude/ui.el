(setq prelude-minimalistic-ui t)
(scroll-bar-mode -1)
(setq prelude-whitespace nil)
(setq mouse-wheel-progressive-speed nil)
(setq prelude-theme nil)
(setq prelude-flyspell nil)

;; Dired: default program to use when using "&" on a file.
(setq dired-guess-shell-alist-user '(("\\.pdf\\'" "zathura")))

(setq fill-column 80)

;; AUCTeX preview font size
(set-default 'preview-scale-function 1.3)
(global-hl-line-mode -1)
(setq global-hl-line-mode -1)

(global-set-key (kbd "C-c j") 'counsel-rg)
(global-set-key (kbd "C-c '") 'avy-goto-char-2)