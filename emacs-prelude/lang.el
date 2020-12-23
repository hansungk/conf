;; General
(global-flycheck-mode -1)
(smartparens-global-mode -1)

(defun my-prog-mode-hook ()
  (global-flycheck-mode -1)
  (smartparens-mode -1)
  (global-hl-line-mode -1))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;; C
(defun my-c-mode-hook ()
  (c-set-style "linux")
  (c-toggle-comment-style -1)
  (define-key c-mode-map (kbd "TAB") 'clang-format-region)
  (define-key c-mode-map (kbd "M-q")
    (lambda () (interactive) (if (use-region-p)
                                 (fill-region (region-beginning) (region-end))
                               (c-fill-paragraph)))))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd" "-j=7" "--background-index" "--cross-file-rename"))
;; (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1)))
(setq eldoc-echo-area-use-multiline-p nil)

;; Verilog
(defun my-verilog-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq verilog-indent-level 2
        verilog-indent-level-behavioral 2
        verilog-indent-level-module 2
        verilog-indent-level-declaration 2
        verilog-auto-newline nil
        verilog-tab-always-indent t)
  (set-fill-column 80)
  ;; (define-key verilog-mode-map (kbd "RET") 'electric-indent-just-newline)
  ;; (define-key verilog-mode-map (kbd "TAB") 'tab-to-tab-stop)
  )
(add-hook 'verilog-mode-hook 'my-verilog-mode-hook)

(defun hammer-check-error ()
  (interactive)
  (occur "src/.*\\.v, "))
