;; General
(global-flycheck-mode -1)
(smartparens-global-mode -1)

(defun my-prog-mode-hook ()
  (global-flycheck-mode -1)
  (smartparens-mode -1)
  (global-hl-line-mode -1))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

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
