;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 16:20:30>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-providers/test-emacs-llm-providers.el

;;; test-emacs-llm-providers.el --- Tests for emacs-llm providers -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for the emacs-llm provider modules.

;;; Code:

(require 'ert)

;; Load the package to test
(when
    (require 'emacs-llm-providers nil t)
  (message "emacs-llm-providers loaded successfully for testing"))

(ert-deftest test-emacs-llm-providers-openai-loadable
    ()
  "Test that emacs-llm-providers-openai module can be loaded."
  (should
   (featurep 'emacs-llm-providers-openai)))

(ert-deftest test-emacs-llm-providers-anthropic-loadable
    ()
  "Test that emacs-llm-providers-anthropic module can be loaded."
  (should
   (featurep 'emacs-llm-providers-anthropic)))

(ert-deftest test-emacs-llm-providers-google-loadable
    ()
  "Test that emacs-llm-providers-google module can be loaded."
  (should
   (featurep 'emacs-llm-providers-google)))

(ert-deftest test-emacs-llm-providers-deepseek-loadable
    ()
  "Test that emacs-llm-providers-deepseek module can be loaded."
  (should
   (featurep 'emacs-llm-providers-deepseek)))

(ert-deftest test-emacs-llm-parse-invalid-chunk
    ()
  "Test parsing invalid JSON chunks for all providers."
  (let
      ((invalid-chunk "{invalid-json}"))
    (should-not
     (--el--parse-openai-chunk invalid-chunk))
    (should-not
     (--el--parse-anthropic-chunk invalid-chunk))
    (should-not
     (--el--parse-google-chunk invalid-chunk))
    (should-not
     (--el--parse-deepseek-chunk invalid-chunk))))

(provide 'test-emacs-llm-providers)

(when
    (not load-file-name)
  (message "test-emacs-llm-providers.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))