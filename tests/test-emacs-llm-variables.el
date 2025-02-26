;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 15:52:03>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-variables.el

(require 'ert)
(require 'emacs-llm-variables)

(ert-deftest test-emacs-llm-buffer-name-defined
    ()
  "Test that --el-buffer-name is defined."
  (should
   (boundp '--el-buffer-name))
  (should
   (stringp --el-buffer-name)))

(ert-deftest test-emacs-llm-use-stream-defined
    ()
  "Test that --el-flag-use-stream is defined."
  (should
   (boundp '--el-flag-use-stream))
  (should
   (booleanp --el-flag-use-stream)))

(ert-deftest test-emacs-llm-separator-defined
    ()
  "Test that --el-separator is defined."
  (should
   (boundp '--el-separator))
  (should
   (stringp --el-separator)))

(ert-deftest test-emacs-llm-customization-group
    ()
  "Test that emacs-llm customization group is defined."
  (should
   (get 'emacs-llm 'custom-group))
  (should
   (string=
    (get 'emacs-llm 'group-documentation)
    "Interface to interact with LLMs from Emacs.")))

(ert-deftest test-emacs-llm-variables-dependencies
    ()
  "Test that all required dependencies are loaded."
  (should
   (featurep 'json))
  (should
   (featurep 'markdown-mode))
  (should
   (featurep 'cl-lib)))

(provide 'test-emacs-llm-variables)

(provide 'test-emacs-llm-variables)

(when
    (not load-file-name)
  (message "test-emacs-llm-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))