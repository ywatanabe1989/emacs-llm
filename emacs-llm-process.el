;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-28 09:29:23>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-process.el

(require 'emacs-llm-dired)

(defun --el-cancel-timer
    ()
  "Cancel the LLM spinner timer if active."
  (when --el-spinner-timer
    (cancel-timer --el-spinner-timer)
    (setq --el-spinner-timer nil)))

(defun --el-process-chunk
    (proc chunk parse-func)
  "Process CHUNK from PROC using PARSE-FUNC to extract content."
  ;; (message "Raw chunk received: %s"
  ;;          (substring chunk 0
  ;;                     (min 300
  ;;                          (length chunk))))
  (let*
      ((partial
        (or
         (process-get proc 'partial-data)
         ""))
       (combined
        (concat partial chunk))
       (lines
        (split-string combined "\n"))
       (incomplete
        (if
            (string-suffix-p "\n" combined)
            ""
          (car
           (last lines)))))
    (unless
        (string= incomplete "")
      (setq lines
            (butlast lines)))
    (process-put proc 'partial-data incomplete)
    ;; Process each complete line
    (dolist
        (line lines)
      (when
          (string-prefix-p "data: " line)
        (let*
            ((json-data
              (substring line 6))
             (text
              (when
                  (not
                   (string= json-data "[DONE]"))
                (progn
                  (funcall parse-func json-data)))))
          (when text
            ;; (message "Text extracted: %s"
            ;;          (substring text 0
            ;;                     (min 30
            ;;                          (length text))))
            (with-current-buffer
                (process-get proc 'target-buffer)
              (goto-char
               (point-max))
              (insert text))
            ;; Accumulate content
            (process-put proc 'content
                         (concat
                          (or
                           (process-get proc 'content)
                           "")
                          text))))))))

(defun --el-process-sentinel
    (proc event)
  "Process sentinel for PROC handling EVENT."
  (when
      (string-match-p "\\(finished\\|exited\\|failed\\)" event)
    (--el-stop-spinner)
    (let
        ((prompt
          (process-get proc 'prompt))
         (provider
          (process-get proc 'provider))
         (engine
          (process-get proc 'engine))
         (template
          (process-get proc 'template))
         (content
          (process-get proc 'content))
         (error-output
          (with-current-buffer
              (process-get proc 'temp-buffer)
            (buffer-string))))
      ;; Debug logging to help identify issues
      (message "Process for %s %s with content length: %s"
               provider event
               (if content
                   (length content)
                 0))
      ;; Show error output if process failed and no content was generated
      (when
          (and
           (string-match-p "\\(exited abnormally\\|failed\\)" event)
           (or
            (null content)
            (string-empty-p content)))
        (message "Error from %s: %s" provider
                 (substring error-output 0
                            (min 500
                                 (length error-output)))))
      (when
          (and prompt provider)
        ;; Only append to history after we have a complete response
        (--el-history-append "user" prompt template)
        (when
            (and content
                 (not
                  (string-empty-p content)))
          (--el-history-append "assistant" content template)))
      ;; Clean up temp buffer
      (when-let
          ((tb
            (process-get proc 'temp-buffer)))
        (when
            (buffer-live-p tb)
          (kill-buffer tb))))))

(provide 'emacs-llm-process)

(when
    (not load-file-name)
  (message "emacs-llm-process.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))