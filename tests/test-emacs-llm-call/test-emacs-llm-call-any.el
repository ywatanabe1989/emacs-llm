;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 08:44:32>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-call/test-emacs-llm-call-core.el

;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'ert)
(require 'emacs-llm-call-core)

(ert-deftest test-el-llm-call-openai
    ()
  "Test el-llm-call with OpenAI provider."
  (let*
      ((--el-api-key-openai "test-key")
       (--el-default-engine-openai "gpt-4")
       (proc
        (el-llm-call "test" "openai")))
    (should proc)
    (should
     (process-live-p proc))
    (should
     (string=
      (process-get proc 'provider)
      "openai"))
    (should
     (string=
      (process-get proc 'prompt)
      "test"))
    (should
     (string=
      (process-get proc 'engine)
      "gpt-4"))))

(ert-deftest test-el-llm-call-anthropic
    ()
  "Test el-llm-call with Anthropic provider."
  (let*
      ((--el-api-key-anthropic "test-key")
       (proc
        (el-llm-call "test" "anthropic")))
    (should proc)
    (should
     (process-live-p proc))
    (should
     (string=
      (process-get proc 'provider)
      "anthropic"))
    (should
     (string=
      (process-get proc 'prompt)
      "test"))
    (should
     (string=
      (process-get proc 'engine)
      "claude-3-7-sonnet-20250219"))))

(ert-deftest test-el-llm-call-default-provider
    ()
  "Test el-llm-call with default provider."
  (let*
      ((--el-default-provider "openai")
       (--el-api-key-openai "test-key")
       (proc
        (el-llm-call "test")))
    (should proc)
    (should
     (process-live-p proc))
    (should
     (string=
      (process-get proc 'provider)
      "openai"))))

(ert-deftest test-el-llm-call-with-template
    ()
  "Test el-llm-call with template."
  (let*
      ((--el-default-provider "openai")
       (--el-api-key-openai "test-key")
       (--el-templates
        '(("test-template" . "Template: %s")))
       (proc
        (el-llm-call "test" "openai" "test-template")))
    (should proc)
    (should
     (process-live-p proc))
    (should
     (string=
      (process-get proc 'template)
      "test-template"))))

(provide 'test-emacs-llm-call-core)

(provide 'test-emacs-llm-call-core)

(when
    (not load-file-name)
  (message "test-emacs-llm-call-core.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))