;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 16:49:12>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-call/test-emacs-llm-call.el

;;; test-emacs-llm-call.el --- Tests for emacs-llm providers -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for the emacs-llm provider modules.

;;; Code:

(require 'ert)

;; Load the package to test
(when
    (require 'emacs-llm-call nil t)
  (message "emacs-llm-call loaded successfully for testing"))

(ert-deftest test-emacs-llm-call-openai-loadable
    ()
  "Test that emacs-llm-call-openai module can be loaded."
  (should
   (featurep 'emacs-llm-call-openai)))

(ert-deftest test-emacs-llm-call-anthropic-loadable
    ()
  "Test that emacs-llm-call-anthropic module can be loaded."
  (should
   (featurep 'emacs-llm-call-anthropic)))

(ert-deftest test-emacs-llm-call-google-loadable
    ()
  "Test that emacs-llm-call-google module can be loaded."
  (should
   (featurep 'emacs-llm-call-google)))

(ert-deftest test-emacs-llm-call-deepseek-loadable
    ()
  "Test that emacs-llm-call-deepseek module can be loaded."
  (should
   (featurep 'emacs-llm-call-deepseek)))

(ert-deftest test-emacs-llm-parse-invalid-chunk
    ()
  "Test parsing invalid JSON chunks for all providers."
  (let
      ((invalid-chunk "{invalid-json}"))
    (should-not
     (--el-parse-openai-chunk invalid-chunk))
    (should-not
     (--el-parse-anthropic-chunk invalid-chunk))
    (should-not
     (--el-parse-google-chunk invalid-chunk))
    (should-not
     (--el-parse-deepseek-chunk invalid-chunk))))

(provide 'test-emacs-llm-call)

(when
    (not load-file-name)
  (message "test-emacs-llm-call.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))