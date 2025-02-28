;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 16:42:09>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-switch.el

;;; test-emacs-llm-switch.el --- Tests for emacs-llm provider switching -*- lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 15:16:44>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-switch.el

(require 'ert)
(require 'emacs-llm-switch)

(ert-deftest test-emacs-llm-switch-provider-functionality
    ()
  "Test provider switching functionality."
  (let
      ((original-provider --el-default-provider)
       (--el-openai-engines
        '("gpt-4o" "gpt-4"))
       (--el-anthropic-engines
        '("claude-3"))
       (--el-google-engines
        '("gemini-pro"))
       (--el-deepseek-engines
        '("deepseek-chat"))
       (--el-default-provider nil)
       (--el-openai-engine nil)
       (--el-anthropic-engine nil)
       (--el-google-engine nil)
       (--el-deepseek-engine nil)
       (completing-read-args nil)
       (completing-read-result nil))

    ;; Mock completing-read to return predictable values
    (cl-letf
        (((symbol-function 'completing-read)
          (lambda
            (&rest args)
            (setq completing-read-args args)
            (cond
             ((string=
               (car args)
               "Select engine: ")
              (cond
               ((string= --el-default-provider "openai")
                "gpt-4o")
               ((string= --el-default-provider "anthropic")
                "claude-3")
               ((string= --el-default-provider "google")
                "gemini-pro")
               ((string= --el-default-provider "deepseek")
                "deepseek-chat")
               (t "unknown")))
             (t completing-read-result)))))

      ;; Test with openai
      (setq completing-read-result "openai")
      (call-interactively 'el-switch)
      (should
       (string= --el-default-provider "openai"))
      (should
       (string= --el-openai-engine "gpt-4o"))

      ;; Test with anthropic
      (setq completing-read-result "anthropic")
      (call-interactively 'el-switch)
      (should
       (string= --el-default-provider "anthropic"))
      (should
       (string= --el-anthropic-engine "claude-3"))

      ;; Test with google
      (setq completing-read-result "google")
      (call-interactively 'el-switch)
      (should
       (string= --el-default-provider "google"))
      (should
       (string= --el-google-engine "gemini-pro"))

      ;; Test with deepseek
      (setq completing-read-result "deepseek")
      (call-interactively 'el-switch)
      (should
       (string= --el-default-provider "deepseek"))
      (should
       (string= --el-deepseek-engine "deepseek-chat")))

    ;; Restore original provider
    (setq --el-default-provider original-provider)))

(provide 'test-emacs-llm-switch)

(provide 'test-emacs-llm-switch)

(when
    (not load-file-name)
  (message "test-emacs-llm-switch.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))