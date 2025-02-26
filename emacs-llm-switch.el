;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 14:19:04>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-switch.el

;;;###autoload
(defun el-switch
    (provider)
  "Switch the LLM provider and select a model."
  (interactive
   (list
    (completing-read "Select LLM provider: "
                     '("openai" "anthropic" "google" "deepseek")
                     nil t)))
  (setq --el-provider provider)

  (let*
      ((provider-models
        (cond
         ((string= provider "openai")
          --el-openai-models)
         ((string= provider "anthropic")
          --el-anthropic-models)
         ((string= provider "google")
          --el-google-models)
         ((string= provider "deepseek")
          --el-deepseek-models)))
       (selected-model
        (completing-read "Select model: " provider-models nil t)))

    (cond
     ((string= provider "openai")
      (setq --el-openai-model selected-model))
     ((string= provider "anthropic")
      (setq --el-anthropic-model selected-model))
     ((string= provider "google")
      (setq --el-google-model selected-model))
     ((string= provider "deepseek")
      (setq --el-deepseek-model selected-model))
     )

    (message "Switched to %s using model: %s" provider selected-model)))

(provide 'emacs-llm-switch)

(when
    (not load-file-name)
  (message "emacs-llm-switch.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))