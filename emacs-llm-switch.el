;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 14:19:04>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-switch.el

;;;###autoload
(defun el-switch
    (provider)
  "Switch the LLM provider and select a engine."
  (interactive
   (list
    (completing-read "Select LLM provider: "
                     '("openai" "anthropic" "google" "deepseek")
                     nil t)))
  (setq --el-provider provider)

  (let*
      ((provider-engines
        (cond
         ((string= provider "openai")
          --el-openai-engines)
         ((string= provider "anthropic")
          --el-anthropic-engines)
         ((string= provider "google")
          --el-google-engines)
         ((string= provider "deepseek")
          --el-deepseek-engines)))
       (selected-engine
        (completing-read "Select engine: " provider-engines nil t)))

    (cond
     ((string= provider "openai")
      (setq --el-openai-engine selected-engine))
     ((string= provider "anthropic")
      (setq --el-anthropic-engine selected-engine))
     ((string= provider "google")
      (setq --el-google-engine selected-engine))
     ((string= provider "deepseek")
      (setq --el-deepseek-engine selected-engine))
     )

    (message "Switched to %s using engine: %s" provider selected-engine)))

(provide 'emacs-llm-switch)

(when
    (not load-file-name)
  (message "emacs-llm-switch.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))