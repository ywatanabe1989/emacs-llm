;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 16:02:37>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-providers/test-emacs-llm-providers-variables.el

(require 'ert)
(require 'emacs-llm-providers-variables)

;; Test loading
(ert-deftest test-emacs-llm-providers-variables-loadable
    ()
  (should
   (featurep 'emacs-llm-providers-variables)))

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
(ert-deftest test-el-openai-models-existence
    ()
  (should
   (boundp '--el-openai-models)))

(ert-deftest test-el-anthropic-models-existence
    ()
  (should
   (boundp '--el-anthropic-models)))

(ert-deftest test-el-google-models-existence
    ()
  (should
   (boundp '--el-google-models)))

(ert-deftest test-el-deepseek-models-existence
    ()
  (should
   (boundp '--el-deepseek-models)))

(ert-deftest test-el-groq-models-existence
    ()
  (should
   (boundp '--el-groq-models)))

;; Test available engines content
(ert-deftest test-el-openai-models-content
    ()
  (should
   (equal --el-openai-models
          (mapcar #'car --el-openai-engine-max-tokens-alist))))

(ert-deftest test-el-anthropic-models-content
    ()
  (should
   (equal --el-anthropic-models
          (mapcar #'car --el-anthropic-engine-max-tokens-alist))))

(ert-deftest test-el-google-models-content
    ()
  (should
   (equal --el-google-models
          (mapcar #'car --el-google-engine-max-tokens-alist))))

(ert-deftest test-el-deepseek-models-content
    ()
  (should
   (equal --el-deepseek-models
          (mapcar #'car --el-deepseek-engine-max-tokens-alist))))

(ert-deftest test-el-groq-models-content
    ()
  (should
   (equal --el-groq-models
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

(provide 'test-emacs-llm-providers-variables)

(provide 'test-emacs-llm-providers-variables)

(when
    (not load-file-name)
  (message "test-emacs-llm-providers-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))