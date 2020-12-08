(global-flycheck-mode -1)
(defun disable-flycheck-mode ()
  (global-flycheck-mode -1))
(add-hook 'prog-mode-hook 'disable-flycheck-mode)

(setq verilog-auto-newline nil)
