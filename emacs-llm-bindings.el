;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 13:01:39>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-bindings.el

;;;###autoload
(defun --el-setup-key-bindings
    ()
  "Set up key bindings for emacs-llm functions."
  (interactive)
  (global-set-key
   (kbd "C-c l q")
   #'--el-quick-menu)
  (global-set-key
   (kbd "C-c l o")
   #'--el-on-region)
  (global-set-key
   (kbd "C-c l s")
   #'--el-switch-provider)
  (global-set-key
   (kbd "C-c l h")
   #'--el-show-history)
  (global-set-key
   (kbd "C-c l c")
   #'--el-copy-last-response)
  (message "emacs-llm key bindings set up."))

(global-set-key
 (kbd "C-M-S-g")
 #'--el-on-region)

(provide 'emacs-llm-bindings)

(when
    (not load-file-name)
  (message "emacs-llm-bindings.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))