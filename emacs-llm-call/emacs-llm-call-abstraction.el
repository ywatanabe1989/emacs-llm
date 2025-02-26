;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-27 09:42:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-abstraction.el

(require 'emacs-llm-call-variables)
(require 'emacs-llm-call-openai)
(require 'emacs-llm-call-google)
(require 'emacs-llm-call-anthropic)
(require 'emacs-llm-call-deepseek)

(defun el-llm-call
    (prompt &optional provider template-name)
  "Stream LLM response for PROMPT using PROVIDER.
Optional TEMPLATE-NAME is the name of the template used.

If PROVIDER is nil, use value of --el-actual-provider.
Returns the process object for the streaming request."
  (let*
      ((actual-provider
        (or provider --el-actual-provider))
       (provider-function
        (intern
         (format "--el-%s-stream" actual-provider))))

    ;; Check if the provider function exists
    (if
        (fboundp provider-function)
        (progn
          (message "Actual provider: %s" actual-provider)
          (funcall provider-function prompt template-name))

      ;; If provider function doesn't exist, try to fallback
      (message "Provider %s not supported, falling back to default" actual-provider)
      (let
          ((fallback-provider
            (or --el-default-provider "openai"))
           (fallback-function nil))
        (setq fallback-function
              (intern
               (format "--el-%s-stream" fallback-provider)))
        (if
            (fboundp fallback-function)
            (progn
              (message "Falling back to %s" fallback-provider)
              (funcall fallback-function prompt template-name))
          (error "No working LLM provider found"))))))

(provide 'emacs-llm-call-abstraction)

(when
    (not load-file-name)
  (message "emacs-llm-call-abstraction.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))