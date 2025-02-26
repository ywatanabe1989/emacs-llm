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
      ((original-provider --el-provider)
       (--el-openai-models
        '("gpt-4o" "gpt-4"))
       (--el-anthropic-models
        '("claude-3"))
       (--el-google-models
        '("gemini-pro"))
       (--el-deepseek-models
        '("deepseek-chat"))
       (--el-provider nil)
       (--el-openai-model nil)
       (--el-anthropic-model nil)
       (--el-google-model nil)
       (--el-deepseek-model nil)
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
               "Select model: ")
              (cond
               ((string= --el-provider "openai")
                "gpt-4o")
               ((string= --el-provider "anthropic")
                "claude-3")
               ((string= --el-provider "google")
                "gemini-pro")
               ((string= --el-provider "deepseek")
                "deepseek-chat")
               (t "unknown")))
             (t completing-read-result)))))

      ;; Test with openai
      (setq completing-read-result "openai")
      (call-interactively 'el-switch)
      (should
       (string= --el-provider "openai"))
      (should
       (string= --el-openai-model "gpt-4o"))

      ;; Test with anthropic
      (setq completing-read-result "anthropic")
      (call-interactively 'el-switch)
      (should
       (string= --el-provider "anthropic"))
      (should
       (string= --el-anthropic-model "claude-3"))

      ;; Test with google
      (setq completing-read-result "google")
      (call-interactively 'el-switch)
      (should
       (string= --el-provider "google"))
      (should
       (string= --el-google-model "gemini-pro"))

      ;; Test with deepseek
      (setq completing-read-result "deepseek")
      (call-interactively 'el-switch)
      (should
       (string= --el-provider "deepseek"))
      (should
       (string= --el-deepseek-model "deepseek-chat")))

    ;; Restore original provider
    (setq --el-provider original-provider)))

(provide 'test-emacs-llm-switch)

(provide 'test-emacs-llm-switch)

(when
    (not load-file-name)
  (message "test-emacs-llm-switch.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))