;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 21:22:14>
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
                (funcall parse-func json-data))))

          (when text
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
  (message "Process %s received event: %s"
           (process-name proc)
           event)
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
         ;; Template is intentionally not used in history
         ;; (template
         ;;  (process-get proc 'template))
         (response
          (process-get proc 'content)))
      (when
          (and prompt provider)
        ;; Only append to history after we have a complete response
        ;; Don't pass template to history
        (--el-append-to-history "user" prompt)
        (when
            (not
             (string-empty-p response))
          ;; Don't pass template to history for assistant responses either
          (--el-append-to-history engine response)))
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