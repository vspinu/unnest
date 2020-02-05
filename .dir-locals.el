
((nil
  (require-final-newline . t)
  ;; not tabs in code
  (indent-tabs-mode)
  ;; remove trailing whitespace
  (comment-add . 0)
  (eval . (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))
 (makefile-gmake-mode
  (indent-tabs-mode . t))
 (ess-mode
  (ess-indent-with-fancy-comments)
  (ess-indent-offset . 2)))
