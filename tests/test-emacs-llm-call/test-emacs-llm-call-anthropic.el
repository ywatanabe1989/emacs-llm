;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 08:41:18>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-call/test-emacs-llm-call-anthropic.el

(require 'ert)

(ert-deftest test-parse-anthropic-chunk-text
    ()
  "Tests parsing text content from Anthropic chunk."
  (let
      ((chunk "{\"delta\":{\"text\":\"test\"}}"))
    (should
     (string=
      (--el-parse-anthropic-chunk chunk)
      "test"))))

(ert-deftest test-parse-anthropic-chunk-thinking
    ()
  "Tests parsing thinking content from Anthropic chunk."
  (let
      ((chunk "{\"delta\":{\"thinking\":\"processing\"}}"))
    (should
     (string=
      (--el-parse-anthropic-chunk chunk)
      "processing"))))

(ert-deftest test-parse-anthropic-chunk-empty
    ()
  "Tests parsing empty Anthropic chunk."
  (should
   (null
    (--el-parse-anthropic-chunk "[DONE]"))))

(ert-deftest test-anthropic-curl-command-execution
    ()
  "Tests if Anthropic curl command executes successfully."
  (let*
      ((--el-api-key-anthropic
        (getenv "ANTHROPIC_API_KEY"))
       (curl-cmd
        (--el-construct-anthropic-curl-command "test"))
       (temp-buffer
        (generate-new-buffer "*test-curl-output*")))
    (unwind-protect
        (progn
          (should --el-api-key-anthropic)
          (let
              ((proc
                (start-process-shell-command
                 "test-curl" temp-buffer curl-cmd)))
            (set-process-sentinel
             proc
             (lambda
               (proc _event)
               (with-current-buffer
                   (process-buffer proc)
                 (goto-char
                  (point-min))
                 (should
                  (search-forward "\"delta\":" nil t)))))
            (accept-process-output proc 5)))
      (kill-buffer temp-buffer))))

(provide 'test-emacs-llm-call-anthropic)

(when
    (not load-file-name)
  (message "test-emacs-llm-call-anthropic.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))