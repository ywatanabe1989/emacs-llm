;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 16:02:37>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-call/test-emacs-llm-call-variables.el

(require 'ert)
(require 'emacs-llm-call-variables)

;; Test loading
(ert-deftest test-emacs-llm-call-variables-loadable
    ()
  (should
   (featurep 'emacs-llm-call-variables)))

;; Test default provider variable
(ert-deftest test-el-default-provider-existence
    ()
  (should
   (boundp '--el-default-provider)))

;; Test default engine variable
(ert-deftest test-el-default-engine-existence
    ()
  (should
   (boundp '--el-default-engine)))

;; Test temperature setting
(ert-deftest test-el-temperature-existence
    ()
  (should
   (boundp '--el-temperature)))

;; Test max tokens setting
(ert-deftest test-el-max-tokens-existence
    ()
  (should
   (boundp '--el-max-tokens)))

;; Test api keys existence
(ert-deftest test-el-api-key-openai-existence
    ()
  (should
   (boundp '--el-api-key-openai)))

(ert-deftest test-el-api-key-anthropic-existence
    ()
  (should
   (boundp '--el-api-key-anthropic)))

(ert-deftest test-el-api-key-google-existence
    ()
  (should
   (boundp '--el-api-key-google)))

(ert-deftest test-el-api-key-deepseek-existence
    ()
  (should
   (boundp '--el-api-key-deepseek)))

(ert-deftest test-el-api-key-groq-existence
    ()
  (should
   (boundp '--el-api-key-groq)))

;; Test default engine variables
(ert-deftest test-el-default-engine-openai-existence
    ()
  (should
   (boundp '--el-default-engine-openai)))

(ert-deftest test-el-default-engine-anthropic-existence
    ()
  (should
   (boundp '--el-default-engine-anthropic)))

(ert-deftest test-el-default-engine-google-existence
    ()
  (should
   (boundp '--el-default-engine-google)))

(ert-deftest test-el-default-engine-deepseek-existence
    ()
  (should
   (boundp '--el-default-engine-deepseek)))

(ert-deftest test-el-default-engine-groq-existence
    ()
  (should
   (boundp '--el-default-engine-groq)))

;; Test engine max tokens alists existence
(ert-deftest test-el-openai-engine-max-tokens-alist-existence
    ()
  (should
   (boundp '--el-openai-engine-max-tokens-alist)))

(ert-deftest test-el-anthropic-engine-max-tokens-alist-existence
    ()
  (should
   (boundp '--el-anthropic-engine-max-tokens-alist)))

(ert-deftest test-el-google-engine-max-tokens-alist-existence
    ()
  (should
   (boundp '--el-google-engine-max-tokens-alist)))

(ert-deftest test-el-deepseek-engine-max-tokens-alist-existence
    ()
  (should
   (boundp '--el-deepseek-engine-max-tokens-alist)))

(ert-deftest test-el-groq-engine-max-tokens-alist-existence
    ()
  (should
   (boundp '--el-groq-engine-max-tokens-alist)))

;; Test available engines lists existence
(ert-deftest test-el-openai-engines-existence
    ()
  (should
   (boundp '--el-openai-engines)))

(ert-deftest test-el-anthropic-engines-existence
    ()
  (should
   (boundp '--el-anthropic-engines)))

(ert-deftest test-el-google-engines-existence
    ()
  (should
   (boundp '--el-google-engines)))

(ert-deftest test-el-deepseek-engines-existence
    ()
  (should
   (boundp '--el-deepseek-engines)))

(ert-deftest test-el-groq-engines-existence
    ()
  (should
   (boundp '--el-groq-engines)))

;; Test available engines content
(ert-deftest test-el-openai-engines-content
    ()
  (should
   (equal --el-openai-engines
          (mapcar #'car --el-openai-engine-max-tokens-alist))))

(ert-deftest test-el-anthropic-engines-content
    ()
  (should
   (equal --el-anthropic-engines
          (mapcar #'car --el-anthropic-engine-max-tokens-alist))))

(ert-deftest test-el-google-engines-content
    ()
  (should
   (equal --el-google-engines
          (mapcar #'car --el-google-engine-max-tokens-alist))))

(ert-deftest test-el-deepseek-engines-content
    ()
  (should
   (equal --el-deepseek-engines
          (mapcar #'car --el-deepseek-engine-max-tokens-alist))))

(ert-deftest test-el-groq-engines-content
    ()
  (should
   (equal --el-groq-engines
          (mapcar #'car --el-groq-engine-max-tokens-alist))))

;; Test specific engine max tokens values
(ert-deftest test-el-google-engine-max-tokens-gemini-2-flash
    ()
  (should
   (=
    (alist-get "gemini-2.0-flash" --el-google-engine-max-tokens-alist nil nil 'string=)
    100000)))

(ert-deftest test-el-openai-engine-max-tokens-gpt4o
    ()
  (should
   (=
    (alist-get "gpt-4o" --el-openai-engine-max-tokens-alist nil nil 'string=)
    8192)))

(provide 'test-emacs-llm-call-variables)

(provide 'test-emacs-llm-call-variables)

(when
    (not load-file-name)
  (message "test-emacs-llm-call-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))