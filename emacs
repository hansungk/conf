;; -*- emacs-lisp -*-

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Start Emacs server
(server-start)

(add-hook 'after-make-frame-functions
  (lambda ()
    (when (not (display-graphic-p))
      (set-face-background 'vertical-border "gray")
      (set-face-foreground 'vertical-border (face-background 'vertical-border)))))

;; Global modes enabled for default
(show-paren-mode 1)
(savehist-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(setq column-number-indicator-zero-based nil)

;; Do not clutter PWD with *~ files
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

(if (eq system-type 'darwin)
    (progn
      ;; Fix keys for railwaycat/homebrew-emacsmacport
      (setq mac-option-modifier 'meta)
      (setq mac-command-modifier 'super)

      ;; Transparent titlebar
      ;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

      ;; Fix wrong "ko_Kore_KR.UTF-8")
      (setenv "LANG" "ko_KR.UTF-8")
      ;; (set-language-environment "Korean") ; does not work

      (setenv "PATH" (concat (getenv "PATH")
                             ":/Library/TeX/texbin"))
      ;; Clang
      (setenv "CXX" "/usr/local/opt/llvm/bin/clang++")
      (setenv "CC" "/usr/local/opt/llvm/bin/clang++")))

;; Platform-indepedent PATH
(setenv "PATH" (concat (getenv "PATH")
                       ":/usr/local/bin"))
;; exec-path is customized

(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
;; (global-set-key (kbd "<S-space>") 'toggle-input-method)

;; shift+arrow for window moving
(windmove-default-keybindings)

;; Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper-backward)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c k") 'counsel-rg)

;; Keys
(global-set-key (kbd "C-x g") 'magit-status)
;; (global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-v") 'clipboard-yank)
; Counsel
(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-c n") 'counsel-fzf)

;; Evil
(setq evil-want-C-u-scroll t)

;;; Language-specific options --------------------------------------------------

;; (setq ccls-executable "/home/stephen/build/ccls/Release/ccls")
(setq ccls-args '("--log-file=/tmp/ccls.log"))
(defun lsp-ccls ()
  (interactive)
  (require 'ccls)
  (lsp))
(setq lsp-enable-snippet nil)
;; (setq lsp-prefer-flymake :none)

;; CC mode
(defun my-c-mode-common-hook ()
  (define-key c-mode-base-map [tab] 'clang-format))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; C
(defun my-c-mode-hook ()
  (c-set-style "han")
  (c-toggle-comment-style -1))
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; Highlight TODO, FIXME, etc.
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|XXX\\)" 1
                                       font-lock-warning-face t)))))

;; C++
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
  (lsp-ccls))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd" "-j=7" "--background-index" "--cross-file-rename"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(setq-default indent-tabs-mode nil)

;; (setq-default tab-width 4)
;; (defvaralias 'c-basic-offset 'tab-width)
;; (c-set-offset 'case-label '+)

;; Python
(defun my-python-mode-hook ()
  (set-fill-column 79))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Verilog
(defun my-verilog-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq verilog-indent-level 2
        verilog-indent-level-module 0
        verilog-indent-level-declaration 0
        verilog-auto-newline nil)
  (set-fill-column 80))
(add-hook 'verilog-mode-hook 'my-verilog-mode-hook)

;; LaTeX
(defun my-latex-mode-hook ()
  (set-fill-column 80))
(add-hook `latex-mode-hook `my-latex-mode-hook)

;; Custom functions ------------------------------------------------------------

(defun goto-local-definition (&optional col-indent)
  (interactive "p")
  (let ((word (current-word)))
    (push-mark)
    (beginning-of-defun)
    (setq regexp-search-ring (cons (concat "\\_<" word "\\_>") regexp-search-ring))
    (search-forward-regexp (concat "\\_<" word "\\_>"))))

;; Startup layout
(setq inhibit-startup-screen t)
(toggle-frame-maximized)
(split-window-right)
(other-window 1)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
