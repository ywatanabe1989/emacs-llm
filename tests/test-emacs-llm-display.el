;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 15:20:42>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-display.el

;; Test display functionality
(ert-deftest test-emacs-llm-prepare-buffer
    ()
  "Test that the LLM buffer can be prepared correctly."
  (let
      ((buffer-name
        (--el--prepare-llm-buffer "Test prompt" "TEST" "test-model")))
    (should
     (get-buffer buffer-name))
    (with-current-buffer buffer-name
      (should
       (derived-mode-p 'markdown-mode))
      (should
       (string-match-p "Test prompt"
                       (buffer-string)))
      (should
       (string-match-p "Provider: TEST | Model: test-model"
                       (buffer-string))))
    (kill-buffer buffer-name)))

;; Test preparation of LLM buffer
(ert-deftest test-emacs-llm-prepare-buffer-basic
    ()
  "Test basic buffer preparation without template."
  (let*
      ((--el-buffer-name "*Test-LLM*")
       (prompt "Test prompt")
       (provider "TEST-PROVIDER")
       (model "test-model")
       (buffer-name
        (--el--prepare-llm-buffer prompt provider model)))

    (should
     (get-buffer buffer-name))
    (with-current-buffer buffer-name
      ;; Verify markdown mode
      (should
       (derived-mode-p 'markdown-mode))

      ;; Verify content
      (let
          ((content
            (buffer-string)))
        (should
         (string-match-p
          (regexp-quote --el-separator)
          content))
        (should
         (string-match-p
          (regexp-quote
           (format "Provider: %s | Model: %s" provider model))
          content))
        (should
         (string-match-p
          (regexp-quote "User:")
          content))
        (should
         (string-match-p
          (regexp-quote prompt)
          content))
        (should
         (string-match-p
          (regexp-quote "AI:")
          content))
        (should-not
         (string-match-p "Template" content))))

    (kill-buffer buffer-name)))

(ert-deftest test-emacs-llm-prepare-buffer-with-template
    ()
  "Test buffer preparation with template."
  (let*
      ((--el-buffer-name "*Test-LLM*")
       (prompt "Test prompt")
       (provider "TEST-PROVIDER")
       (model "test-model")
       (template "test-template")
       (buffer-name
        (--el--prepare-llm-buffer prompt provider model template)))

    (should
     (get-buffer buffer-name))
    (with-current-buffer buffer-name
      ;; Verify content with template
      (let
          ((content
            (buffer-string)))
        (should
         (string-match-p
          (regexp-quote
           (format "[Template: %s]" template))
          content))))

    (kill-buffer buffer-name)))

(ert-deftest test-emacs-llm-prepare-buffer-appends-to-existing
    ()
  "Test that preparing buffer appends to existing content."
  (let*
      ((--el-buffer-name "*Test-LLM*")
       (buffer
        (get-buffer-create --el-buffer-name)))

    ;; Add initial content
    (with-current-buffer buffer
      (insert "Initial content\n"))

    ;; Prepare buffer
    (--el--prepare-llm-buffer "Test prompt" "TEST" "test-model")

    (with-current-buffer buffer
      (should
       (string-match-p "Initial content"
                       (buffer-string)))
      (should
       (string-match-p "Test prompt"
                       (buffer-string))))

    (kill-buffer buffer)))

(ert-deftest test-emacs-llm-display-with-context
    ()
  "Test displaying full context including response."
  (let*
      ((--el-buffer-name "*Test-LLM*")
       (prompt "Test prompt")
       (response "Test response")
       (provider "TEST-PROVIDER")
       (model "test-model"))

    ;; Display with context
    (--el--display-with-context prompt nil response provider model)

    (with-current-buffer --el-buffer-name
      ;; Verify content
      (let
          ((content
            (buffer-string)))
        (should
         (string-match-p
          (regexp-quote prompt)
          content))
        (should
         (string-match-p
          (regexp-quote response)
          content))))

    (kill-buffer --el-buffer-name)))

(ert-deftest test-emacs-llm-display-with-context-and-template
    ()
  "Test displaying full context with template and response."
  (let*
      ((--el-buffer-name "*Test-LLM*")
       (prompt "Test prompt")
       (template "test-template")
       (response "Test response")
       (provider "TEST-PROVIDER")
       (model "test-model"))

    ;; Display with context including template
    (--el--display-with-context prompt template response provider model)

    (with-current-buffer --el-buffer-name
      ;; Verify content with template
      (let
          ((content
            (buffer-string)))
        (should
         (string-match-p
          (regexp-quote template)
          content))
        (should
         (string-match-p
          (regexp-quote prompt)
          content))
        (should
         (string-match-p
          (regexp-quote response)
          content))))

    (kill-buffer --el-buffer-name)))

(provide 'test-emacs-llm-display)

(when
    (not load-file-name)
  (message "test-emacs-llm-display.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))