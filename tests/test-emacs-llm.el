;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 15:15:43>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/-test-emacs-llm.el

;;; test-emacs-llm.el --- Tests for emacs-llm package -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for the emacs-llm package.

;;; Code:

(require 'ert)

;; Load the package to test
(when
    (require 'emacs-llm nil t)
  (message "emacs-llm loaded successfully for testing"))

;; Basic loadability tests
(ert-deftest test-emacs-llm-loadable
    ()
  "Test that emacs-llm package can be loaded."
  (should
   (featurep 'emacs-llm)))

(ert-deftest test-emacs-llm-variables-loadable
    ()
  "Test that emacs-llm-variables module can be loaded."
  (should
   (featurep 'emacs-llm-variables)))

(ert-deftest test-emacs-llm-bindings-loadable
    ()
  "Test that emacs-llm-bindings module can be loaded."
  (should
   (featurep 'emacs-llm-bindings)))

(ert-deftest test-emacs-llm-display-loadable
    ()
  "Test that emacs-llm-display module can be loaded."
  (should
   (featurep 'emacs-llm-display)))

(ert-deftest test-emacs-llm-history-loadable
    ()
  "Test that emacs-llm-history module can be loaded."
  (should
   (featurep 'emacs-llm-history)))

(ert-deftest test-emacs-llm-providers-loadable
    ()
  "Test that emacs-llm-providers module can be loaded."
  (should
   (featurep 'emacs-llm-providers)))

(ert-deftest test-emacs-llm-run-loadable
    ()
  "Test that emacs-llm-run module can be loaded."
  (should
   (featurep 'emacs-llm-run)))

(ert-deftest test-emacs-llm-spinner-loadable
    ()
  "Test that emacs-llm-spinner module can be loaded."
  (should
   (featurep 'emacs-llm-spinner)))

(ert-deftest test-emacs-llm-switch-loadable
    ()
  "Test that emacs-llm-switch module can be loaded."
  (should
   (featurep 'emacs-llm-switch)))

(provide '-test-emacs-llm)

(when
    (not load-file-name)
  (message "-test-emacs-llm.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))