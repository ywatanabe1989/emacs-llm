;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-28 10:10:19>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-call/emacs-llm-call-any.el

(require 'emacs-llm-process)
(require 'emacs-llm-call-variables)
(require 'emacs-llm-call-openai)
(require 'emacs-llm-call-google)
(require 'emacs-llm-call-anthropic)
(require 'emacs-llm-call-deepseek)

;; (el-llm-call "hi" "openai")

(defun el-llm-call
    (prompt &optional provider template-name)
  "Send PROMPT to specified provider API via streaming.
Optional PROVIDER specifies which LLM provider to use (falls back to --el-actual-provider).
Optional TEMPLATE-NAME is the name of the template used."
  (let*
      ((actual-provider
        (or provider --el-actual-provider))
       (temp-buffer
        (generate-new-buffer
         (format " *%s-temp-output*" actual-provider)))
       (full-prompt
        (--el-template-apply prompt template-name))
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
       ;; Get appropriate payloads and commands
       (payload-func-name
        (format "--el-construct-%s-payload" actual-provider))
       (payload-func
        (if
            (fboundp
             (intern payload-func-name))
            (intern payload-func-name)
          nil))
       (payload
        (and payload-func
             (funcall payload-func full-prompt)))
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
             (funcall curl-func payload)))
       (filter-func-name
        (format "--el-%s-filter" actual-provider))
       (filter-func
        (if
            (fboundp
             (intern filter-func-name))
            (intern filter-func-name)
          nil)))
    ;; Check if we have all required functions for this provider
    (if
        (and payload-func curl-func filter-func engine-name)
        (let*
            ((buffer-name
              (--el-prepare-llm-buffer prompt actual-provider engine-name template-name))
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
          ;; Return the process
          proc)
      ;; If provider function doesn't exist, try to fallback
      (message "Provider %s not supported, falling back to default" actual-provider)
      (let
          ((fallback-provider
            (or --el-default-provider "openai")))
        (if
            (eq fallback-provider actual-provider)
            (error "No working LLM provider found")
          (el-llm-call prompt fallback-provider template-name))))))

;; (defun el-llm-call
;;     (prompt &optional provider template-name)
;;   "Send PROMPT to specified provider API via streaming.
;; Optional PROVIDER specifies which LLM provider to use (falls back to --el-actual-provider).
;; Optional TEMPLATE-NAME is the name of the template used."
;;   (let*
;;       ((actual-provider
;;         (or provider --el-actual-provider))
;;        (temp-buffer
;;         (generate-new-buffer
;;          (format " *%s-temp-output*" actual-provider)))
;;        (full-prompt
;;         (--el-template-apply prompt template-name))
;;        ;; Get the engine name
;;        (engine-var
;;         (intern
;;          (format "--el-%s-engine" actual-provider)))
;;        (default-engine-var
;;         (intern
;;          (format "--el-default-engine-%s" actual-provider)))
;;        (engine-name
;;         (if
;;             (boundp engine-var)
;;             (or
;;              (symbol-value engine-var)
;;              (and
;;               (boundp default-engine-var)
;;               (symbol-value default-engine-var)))
;;           nil))
;;        ;; Get appropriate payloads and commands
;;        (payload-func-name
;;         (format "--el-construct-%s-payload" actual-provider))
;;        (payload-func
;;         (if
;;             (fboundp
;;              (intern payload-func-name))
;;             (intern payload-func-name)
;;           nil))
;;        (payload
;;         (and payload-func
;;              (funcall payload-func full-prompt)))
;;        (curl-func-name
;;         (format "--el-construct-%s-curl-command" actual-provider))
;;        (curl-func
;;         (if
;;             (fboundp
;;              (intern curl-func-name))
;;             (intern curl-func-name)
;;           nil))
;;        (curl-command
;;         (and curl-func
;;              (funcall curl-func payload)))
;;        (filter-func-name
;;         (format "--el-%s-filter" actual-provider))
;;        (filter-func
;;         (if
;;             (fboundp
;;              (intern filter-func-name))
;;             (intern filter-func-name)
;;           nil)))

;;     ;; Check if we have all required functions for this provider
;;     (if
;;         (and payload-func curl-func filter-func engine-name)
;;         (let*
;;             ((buffer-name
;;               (--el-prepare-llm-buffer prompt actual-provider engine-name template-name))
;;              (proc
;;               (start-process-shell-command
;;                (format "--el-%s-stream" actual-provider)
;;                temp-buffer
;;                curl-command)))

;;           ;; Set process properties
;;           (process-put proc 'target-buffer buffer-name)
;;           (process-put proc 'temp-buffer temp-buffer)
;;           (process-put proc 'content "")
;;           (process-put proc 'prompt prompt)
;;           (process-put proc 'provider actual-provider)
;;           (process-put proc 'engine engine-name)
;;           (process-put proc 'template template-name)

;;           ;; Set filter and sentinel
;;           (set-process-filter proc filter-func)
;;           (set-process-sentinel proc #'--el-process-sentinel)

;;           ;; Return the process
;;           proc)

;;       ;; If provider function doesn't exist, try to fallback
;;       (message "Provider %s not supported, falling back to default" actual-provider)
;;       (let
;;           ((fallback-provider
;;             (or --el-default-provider "openai")))
;;         (if
;;             (eq fallback-provider actual-provider)
;;             (error "No working LLM provider found")
;;           (el-llm-call prompt fallback-provider template-name))))))

(defun --el-llm-start-process
    (provider prompt template-name)
  "Start a process for PROVIDER with PROMPT and TEMPLATE-NAME.
Returns the process object for the streaming request."
  (let*
      ((temp-buffer
        (generate-new-buffer
         (format " *%s-temp-output*" provider)))
       (full-prompt
        (--el-template-apply prompt template-name))
       ;; Get appropriate functions based on provider
       (payload-func
        (intern
         (format "--el-construct-%s-payload" provider)))
       (curl-func
        (intern
         (format "--el-construct-%s-curl-command" provider)))
       (filter-func
        (intern
         (format "--el-%s-filter" provider)))
       ;; Get the engine name
       (engine-var
        (intern
         (format "--el-%s-engine" provider)))
       (default-engine-var
        (intern
         (format "--el-default-engine-%s" provider)))
       (engine-name
        (or
         (symbol-value engine-var)
         (symbol-value default-engine-var)))
       ;; Construct payload and curl command
       (payload
        (funcall payload-func full-prompt))
       (curl-command
        (funcall curl-func payload))
       ;; Create buffer for display
       (buffer-name
        (--el-prepare-llm-buffer prompt provider engine-name template-name))
       ;; Start the process
       (proc
        (start-process-shell-command
         (format "--el-%s-stream" provider)
         temp-buffer
         curl-command)))

    ;; Set process properties
    (process-put proc 'target-buffer buffer-name)
    (process-put proc 'temp-buffer temp-buffer)
    (process-put proc 'content "")
    (process-put proc 'prompt prompt)
    (process-put proc 'provider provider)
    (process-put proc 'engine engine-name)
    (process-put proc 'template template-name)

    ;; Set filter and sentinel
    (set-process-filter proc filter-func)
    (set-process-sentinel proc #'--el-process-sentinel)

    ;; ;; it is written here so we need to avoid duplication
    ;; ;; Start spinner and append to history
    ;; (--el-start-spinner)
    ;; (--el-history-append "user" prompt template-name)

    ;; Return the process
    proc))

;; (el-llm-call "hi")
;; process --el-google-stream
;; but it is not shown on the *Emacs-LLM* buffer

(provide 'emacs-llm-call-any)

(when
    (not load-file-name)
  (message "emacs-llm-call-any.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))