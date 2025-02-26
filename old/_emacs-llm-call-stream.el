;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 09:27:49>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-01-llm-llm/el-call-stream.el

;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 09:25:10>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-01-llm-llm/el-stream-helpers.el
;;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'json)
(require 'cl-lib)
(require 'lle-path-system-workspace)
(require 'lle-prompt)
(require 'lle-log-loggers)
(require 'lle-char)
(require 'el-variables)

;; Provider-specific stream modules
(require 'el-openai-stream)
(require 'el-anthropic-stream)
(require 'el-google-stream)
(require 'el-groq-stream)
(require 'el-deepseek-stream)

(require 'el-call-streaming)

(defvar el-stream-buffer-name "*LLM Stream Output*"
  "Name of the buffer used for displaying streaming LLM outputs.")

(defun el-stream-to-buffer
    (prompt &optional buffer-name provider engine)
  "Stream LLM response to PROMPT into a dedicated buffer.
BUFFER-NAME is the name of the output buffer (defaults to `el-stream-buffer-name`).
PROVIDER and ENGINE can be specified to override defaults."
  (interactive "sPrompt: ")
  (let*
      ((output-buffer-name
        (or buffer-name el-stream-buffer-name))
       (buffer
        (get-buffer-create output-buffer-name))
       (output-window
        (display-buffer buffer)))
    ;; Initialize buffer
    (with-current-buffer buffer
      (let
          ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (format "Prompt: %s\n\n" prompt))
        (insert "Response:\n")
        (let
            ((response-start-position
              (point)))
          (put-text-property response-start-position
                             (point-max)
                             'read-only t)
          (set
           (make-local-variable 'response-marker)
           (point-marker)))))

    ;; Create callback function that inserts text into buffer
    (let
        ((stream-callback
          (lambda
            (chunk)
            (with-current-buffer buffer
              (let
                  ((inhibit-read-only t))
                (save-excursion
                  (goto-char
                   (point-max))
                  (insert chunk)
                  (goto-char
                   (point-max))))
              ;; Keep the view scrolled to show latest text
              (when-let
                  ((win
                    (get-buffer-window buffer t)))
                (with-selected-window win
                  (goto-char
                   (point-max))))))))

      ;; Call the streaming function
      (el-stream prompt nil stream-callback nil provider engine)
      buffer)))

(defun el-stream-interrupt
    ()
  "Interrupt the currently running LLM stream request."
  (interactive)
  (when-let
      ((process
        (get-buffer-process " *openai-stream*")))
    (interrupt-process process))
  (when-let
      ((process
        (get-buffer-process " *anthropic-stream*")))
    (interrupt-process process))
  (when-let
      ((process
        (get-buffer-process " *google-stream*")))
    (interrupt-process process))
  (when-let
      ((process
        (get-buffer-process " *groq-stream*")))
    (interrupt-process process))
  (when-let
      ((process
        (get-buffer-process " *deepseek-stream*")))
    (interrupt-process process))
  (message "LLM stream interrupted"))

;; (el-stream-to-buffer "Hi" "*openai-stream*" "openai" "gpt-4o") ;; does not work

;; ;; 1. Basic streaming to a buffer:
;; (el-stream-to-buffer "Explain quantum computing in simple terms")
;; ;; 2. Using a specific provider and engine:
;; (el-stream-to-buffer "Write a short poem about coding" nil "openai" "gpt-4")
;; ;; 3. To interrupt a running stream:
;; (el-stream-interrupt)

;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-02-26 09:15:48>
;; ;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-01-llm-llm/el-call-stream.el

;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-02-26 09:10:57>
;; ;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-01-llm-llm/el-call-streaming.el
;; ;;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;; ;; Main stream API interface
;; ;; ----------------------------------------

;; ;; (el-stream "Hi")
;; ;; [ERROR] [2025-0225-155104] [OPENAI API STREAM] Request failed: Wrong number of arguments: (2 . 5), 6
;; ;; nil

;; (defun el-stream
;;     (prompt-or-tag-pairs &optional recipe-id callback compile-p provider engine)
;;   "Process PROMPT using LLM provider with streaming output.
;; Uses CALLBACK function that receives each chunk as it arrives.
;; If CALLBACK is nil, inserts text into current buffer."
;;   (interactive "sPrompt: ")
;;   (lle-log-debug "el-stream called")
;;   (let*
;;       ((compiled-prompt
;;         (--el-tag-pairs-to-compiled-prompt prompt-or-tag-pairs recipe-id compile-p))
;;        (_
;;         (setq --prompt compiled-prompt))
;;        (engine-provider
;;         (if recipe-id
;;             (--el-get-engine-and-provider recipe-id)
;;           (cons engine provider)))
;;        (engine
;;         (or
;;          (car engine-provider)
;;          --el-default-engine))
;;        (_
;;         (setq --engine engine))
;;        (provider
;;         (or
;;          (cdr engine-provider)
;;          --el-default-provider))
;;        (_
;;         (setq --provider provider))
;;        (output-buffer
;;         (current-buffer))
;;        (default-callback
;;         (lambda
;;           (chunk)
;;           (with-current-buffer output-buffer
;;             (goto-char
;;              (point-max))
;;             (insert chunk)))))
;;     ;; Add debugging
;;     (lle-log-debug "LLM Stream Call - Provider: %s, Engine: %s" provider engine)
;;     (--lle-log-prompt
;;      (lle-char-escape-perc compiled-prompt))
;;     (condition-case err
;;         (pcase provider
;;           ("google"
;;            (--el-google-stream compiled-prompt
;;                                     (or callback default-callback)
;;                                     engine))
;;           ("anthropic"
;;            (--el-anthropic-stream compiled-prompt
;;                                        (or callback default-callback)
;;                                        engine))
;;           ("openai"
;;            (--el-openai-stream compiled-prompt
;;                                     (or callback default-callback)
;;                                     engine))
;;           ("deepseek"
;;            (--el-deepseek-stream compiled-prompt
;;                                       (or callback default-callback)
;;                                       engine))
;;           ("groq"
;;            (--el-groq-stream compiled-prompt
;;                                   (or callback default-callback)
;;                                   engine))
;;           (_
;;            (error "Unknown provider: %s" provider)))
;;       (error
;;        (lle-log-error "LLM stream call failed: %s" err)))))

;; (provide 'el-call-stream)

;; (when
;;     (not load-file-name)
;;   (message "el-call-stream.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))

(provide 'el-call-stream)

(when
    (not load-file-name)
  (message "el-call-stream.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))