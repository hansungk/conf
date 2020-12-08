(global-flycheck-mode -1)
(smartparens-global-mode -1)
(defun my-prog-mode-hook ()
  (global-flycheck-mode -1)
  (smartparens-mode -1)
  (global-hl-line-mode -1))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(setq verilog-auto-newline nil)
