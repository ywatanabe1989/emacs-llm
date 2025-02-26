;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 09:29:33>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-keybindings.el

;;;###autoload
(defun --el-setup-key-bindings
    ()
  "Set up key bindings for emacs-llm functions."
  (interactive)

  ;; Main
  (global-set-key
   (kbd "C-M-g")
   #'el-run)

  ;; (global-set-key
  ;;  (kbd "C-c l o")
  ;;  #'el-run)

  (global-set-key
   (kbd "C-c l s")
   #'el-switch)

  (global-set-key
   (kbd "C-c l h")
   #'el-history-show)

  (global-set-key
   (kbd "C-c l n")
   #'--el-next-code-block)

  (global-set-key
   (kbd "C-c l p")
   #'--el-previous-code-block)

  (message "emacs-llm key bindings set up."))

;; Define key bindings for the LLM buffer
(with-eval-after-load 'markdown-mode
  (let
      ((map markdown-mode-map))
    (define-key map
                (kbd "M-n")
                #'--el-next-code-block)
    (define-key map
                (kbd "M-p")
                #'--el-previous-code-block)
    (define-key map
                (kbd "C-c C-a")
                #'--el-copy-all-code-blocks)
    (define-key map
                (kbd "C-c .")
                #'--el-copy-current-code-block)
    ))

(provide 'emacs-llm-keybindings)

(when
    (not load-file-name)
  (message "emacs-llm-keybindings.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))