;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 22:00:29>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-display.el

(ert-deftest test-emacs-llm-prepare-buffer
    ()
  "Test that the LLM buffer can be prepared correctly."
  (let
      ((buffer-name
        (--el-display-buffer "Test prompt" "TEST" "test-engine")))
    (should
     (get-buffer buffer-name))
    (with-current-buffer buffer-name
      (should
       (derived-mode-p 'markdown-mode))
      (should
       (string-match-p "Test prompt"
                       (buffer-string)))
      (should
       (string-match-p "> test-engine"
                       (buffer-string)))
      )
    (kill-buffer buffer-name)))

(ert-deftest test-emacs-llm-prepare-buffer-basic
    ()
  "Test basic buffer preparation without template."
  (let*
      ((--el-buffer-name "*Test-LLM*")
       (prompt "Test prompt")
       (provider "TEST-PROVIDER")
       (engine "test-engine")
       (buffer-name
        (--el-display-buffer prompt provider engine)))

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
        (sleep-for 1)
        (should
         (string-match-p
          (regexp-quote
           (format "\> %s" engine))
          content))
        (should
         (string-match-p
          (regexp-quote prompt)
          content))
        ;; (should-not
        ;;  (string-match-p "Template" content)
        ;;  )
        ))

    (kill-buffer buffer-name)))

(ert-deftest test-emacs-llm-prepare-buffer-with-template
    ()
  "Test buffer preparation with template."
  (let*
      ((--el-buffer-name "*Test-LLM*")
       (prompt "Test prompt")
       (provider "TEST-PROVIDER")
       (engine "test-engine")
       (template "test-template")
       (buffer-name
        (--el-display-buffer prompt provider engine template)))

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
    (--el-display-buffer "Test prompt" "TEST" "test-engine")

    (with-current-buffer buffer
      (should
       (string-match-p "Initial content"
                       (buffer-string)))
      (should
       (string-match-p "Test prompt"
                       (buffer-string))))

    (kill-buffer buffer)))

(provide 'test-emacs-llm-display)

(when
    (not load-file-name)
  (message "test-emacs-llm-display.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))