;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 09:42:08>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-any.el

(require 'emacs-llm-process)
(require 'emacs-llm-call-variables)
(require 'emacs-llm-call-openai)
(require 'emacs-llm-call-google)
(require 'emacs-llm-call-anthropic)
(require 'emacs-llm-call-deepseek)

;; (el-llm-call "hi" nil "defualt")

(defun el-llm-call
    (prompt &optional provider template-name)
  "Send PROMPT to specified provider API via streaming.
Optional TEMPLATE-NAME is the name of the template used."
  (let*
      ((--el-scroll-to-last-separator)
       (actual-provider
        (or provider --el-default-provider))
       (temp-buffer
        (generate-new-buffer
         (format " *%s-temp-output*" actual-provider)))
       (full-prompt
        (--el-template-embed prompt template-name))
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
          nil)))
    ;; Create buffer and start process
    (let*
        ((buffer-name
          (--el-display-buffer prompt actual-provider engine-name template-name))
         (proc
          (start-process-shell-command
           (format "--el-%s-stream" actual-provider)
           temp-buffer
           curl-command)))
      ;; Set process properties
      (process-put proc 'target-buffer buffer-name)
      (process-put proc 'temp-buffer temp-buffer)
      (process-put proc 'content "")
      (process-put proc 'prompt prompt)
      (process-put proc 'provider actual-provider)
      (process-put proc 'engine engine-name)
      (process-put proc 'template template-name)
      ;; Set filter and sentinel
      (set-process-filter proc filter-func)
      (set-process-sentinel proc #'--el-process-sentinel)
      ;; Start spinner and append to history
      (--el-start-spinner)
      (--el-history-append "user" prompt template-name)
      ;; Return the process
      proc)))

(provide 'emacs-llm-call-any)

(when
    (not load-file-name)
  (message "emacs-llm-call-any.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))