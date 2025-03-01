;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 17:15:27>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-any.el

(require 'emacs-llm-process)
(require 'emacs-llm-call-variables)
(require 'emacs-llm-call-openai)
(require 'emacs-llm-call-google)
(require 'emacs-llm-call-anthropic)
(require 'emacs-llm-call-deepseek)

(defun emacs-llm-call-core
    (prompt &optional provider engine template-name callback)
  "Send PROMPT to specified PROVIDER API via streaming.
PROVIDER defaults to the configured default provider.
ENGINE specifies which model to use.
TEMPLATE-NAME is the name of the template used to format the prompt.
CALLBACK is a function called with each chunk of output text.

Returns the process object."
  (let*
      ((actual-provider
        (or provider emacs-llm-default-provider))
       ;; Get the engine name
       (engine-var
        (intern
         (format "--el-%s-engine" actual-provider)))
       (default-engine-var
        (intern
         (format "--el-default-engine-%s" actual-provider)))
       (engine-name
        (if
            (boundp engine-var)
            (or
             (symbol-value engine-var)
             (and
              (boundp default-engine-var)
              (symbol-value default-engine-var)))
          nil))
       ;; Get the full prompt with template
       (full-prompt
        (--el-template-embed prompt template-name))
       ;; Get curl command and filter
       (curl-func-name
        (format "--el-construct-%s-curl-command" actual-provider))
       (curl-func
        (if
            (fboundp
             (intern curl-func-name))
            (intern curl-func-name)
          nil))
       (curl-command
        (and curl-func
             (funcall curl-func full-prompt)))
       (filter-func-name
        (format "--el-%s-filter" actual-provider))
       (filter-func
        (if
            (fboundp
             (intern filter-func-name))
            (intern filter-func-name)
          nil))
       ;; Create temp buffer for process output
       (temp-buffer
        (generate-new-buffer
         (format " *%s-temp-output*" actual-provider))))

    ;; Start process
    (let
        ((proc
          (start-process-shell-command
           (format "emacs-llm-%s-stream" actual-provider)
           temp-buffer
           curl-command)))

      ;; Set process properties
      (process-put proc 'temp-buffer temp-buffer)
      (process-put proc 'content "")
      (process-put proc 'prompt prompt)
      (process-put proc 'provider actual-provider)
      (process-put proc 'engine engine-name)
      (process-put proc 'template template-name)
      (process-put proc 'callback callback)

      ;; Set filter and sentinel
      (set-process-filter proc filter-func)
      (set-process-sentinel proc #'emacs-llm--process-sentinel-core)

      ;; Return the process
      proc)))

(provide 'emacs-llm-call-core)

(when
    (not load-file-name)
  (message "emacs-llm-call-any.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
