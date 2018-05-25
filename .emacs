(package-initialize)

(if (eq system-type 'darwin)
  ;; Fix keys for railwaycat/homebrew-emacsmacport
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)

  ;; Fix wrong "ko_Kore_KR.UTF-8")
  (setenv "LANG" "ko_KR.UTF-8")
  ;; (set-language-environment "Korean") ; does not work
)

(setenv "PATH" (concat (getenv "PATH")
                       ":/Library/TeX/texbin"
                       ":/usr/local/bin"))
;; exec-path is customized

;; Open .emacs quietly
(setq vc-follow-symlinks t)

(windmove-default-keybindings)

;; Format: ISO 8601 (e.g. 2017-05-17)
(defun insert-date ()
  (interactive)
  (insert
   (format-time-string "%Y-%m-%d" (current-time))))
(global-set-key (kbd "C-c C-d") 'insert-date)

;; ;; Ivy / Swiper / Counsel
;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "(%d/%d) ")

;; (global-set-key (kbd "C-s") 'swiper)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;; Keys
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-v") 'clipboard-yank)

;; Fix Tramp
(setq-default shell-file-name "/bin/bash")

;; Always use the better auto completion
(require 'auto-complete)
(global-set-key (kbd "M-TAB") 'auto-complete)

;; LLVM coding style guidelines in emacs
;; (llvm/utils/emacs/emacs.el)
(c-add-style "han"
             '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 4)
	       (c-basic-offset . 4)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)))))
(defun my-c++-mode-hook ()
  (c-set-style "han")
  (setq indent-tabs-mode nil))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(setq-default indent-tabs-mode nil)

;; (load "~/src/llvm-git/tools/clang/tools/clang-format/clang-format.el")
(global-set-key [C-M-tab] 'clang-format-region)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq tab-width 8)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(c-set-offset 'case-label '+)
