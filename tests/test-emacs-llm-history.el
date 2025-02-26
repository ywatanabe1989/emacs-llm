;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 22:21:26>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-history.el

(ert-deftest test-emacs-llm-history-append
    ()
  "Test adding entries to the history."
  (let
      ((--el-history nil)
       (--el-history-file
        (make-temp-file "emacs-llm-test-history" nil ".json")))
    (--el-history-append "user" "Test user message")
    (should
     (=
      (length --el-history)
      1))
    (should
     (string=
      (alist-get 'role
                 (car --el-history))
      "user"))
    (should
     (string=
      (alist-get 'content
                 (car --el-history))
      "Test user message"))

    (--el-history-append "assistant" "Test assistant response")
    (should
     (=
      (length --el-history)
      2))
    (should
     (string=
      (alist-get 'role
                 (nth 1 --el-history))
      "assistant"))
    (should
     (string=
      (alist-get 'content
                 (nth 1 --el-history))
      "Test assistant response"))

    (delete-file --el-history-file)))

(ert-deftest test-emacs-llm-history-with-template
    ()
  "Test that templates are properly recorded in history."
  (let
      ((--el-history nil)
       (--el-history-file
        (make-temp-file "emacs-llm-test-history" nil ".json")))
    (--el-history-append "user" "Test with template" "test-template")
    (should
     (=
      (length --el-history)
      1))
    (should
     (string=
      (alist-get 'template
                 (car --el-history))
      "test-template"))

    (delete-file --el-history-file)))

;;; test-emacs-llm-history.el --- Tests for emacs-llm history functionality -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for the emacs-llm history module.

;;; Code:

(require 'ert)

;; Load the package to test
(when
    (require 'emacs-llm-history nil t)
  (message "emacs-llm-history loaded successfully for testing"))

;; Helper function for temporary test environment
(defun setup-test-history-env
    ()
  "Set up a clean test environment for history tests."
  (let
      ((temp-dir
        (make-temp-file "emacs-llm-test-" t))
       (temp-file
        (make-temp-name "history.json")))
    (list :dir temp-dir :file
          (expand-file-name temp-file temp-dir))))

(defun cleanup-test-history-env
    (env)
  "Clean up the test environment created by setup-test-history-env."
  (when
      (file-exists-p
       (plist-get env :file))
    (delete-file
     (plist-get env :file)))
  (when
      (file-exists-p
       (plist-get env :dir))
    (delete-directory
     (plist-get env :dir)
     t)))

;; Basic history file operations
(ert-deftest test-emacs-llm-history-dir-creation
    ()
  "Test that history directory is created if it doesn't exist."
  (let*
      ((env
        (setup-test-history-env))
       (--el-history-dir
        (plist-get env :dir)))
    (delete-directory --el-history-dir t)
    (should-not
     (file-exists-p --el-history-dir))
    (--el-history-ensure-file)
    (should
     (file-exists-p --el-history-dir))
    (cleanup-test-history-env env)))

(ert-deftest test-emacs-llm-history-save-load
    ()
  "Test saving and loading history."
  (let*
      ((env
        (setup-test-history-env))
       (--el-history-dir
        (plist-get env :dir))
       (--el-history-file
        (plist-get env :file))
       (--el-history
        '()))

    ;; Add some entries
    (--el-history-append "user" "Test user message 1")
    (--el-history-append "assistant" "Test assistant response 1")
    (--el-history-append "user" "Test user message 2")
    (--el-history-append "assistant" "Test assistant response 2")

    ;; Verify they were saved
    (should
     (file-exists-p --el-history-file))

    ;; Clear in-memory history and reload from file
    (setq --el-history
          '())
    (should
     (=
      (length --el-history)
      0))
    (--el-history-load)

    ;; Verify history was loaded correctly
    (should
     (=
      (length --el-history)
      4))
    (should
     (string=
      (alist-get 'content
                 (nth 0 --el-history))
      "Test user message 1"))
    (should
     (string=
      (alist-get 'content
                 (nth 1 --el-history))
      "Test assistant response 1"))
    (should
     (string=
      (alist-get 'content
                 (nth 2 --el-history))
      "Test user message 2"))
    (should
     (string=
      (alist-get 'content
                 (nth 3 --el-history))
      "Test assistant response 2"))

    (cleanup-test-history-env env)))

(ert-deftest test-emacs-llm-history-backup
    ()
  "Test history file backup mechanism when it exceeds the size limit."
  (let*
      ((env
        (setup-test-history-env))
       (--el-history-dir
        (plist-get env :dir))
       (--el-history-file
        (plist-get env :file))
       (--el-history-max-size 50)
       ;; Small size to trigger backup
       (--el-history
        '()))

    ;; Create a large history entry that will exceed the max size
    (--el-history-append "user"
                            (make-string 100 ?x))

    ;; Save to ensure the file exists
    (--el-history-save)
    (should
     (file-exists-p --el-history-file))

    ;; Check if backup was created when loading
    (--el-history-load)

    ;; Should find at least one backup file
    (let
        ((backup-files
          (directory-files --el-history-dir t "history-.*\\.json")))
      (should
       (>
        (length backup-files)
        0)))

    (cleanup-test-history-env env)))

(ert-deftest test-emacs-llm-clear-history
    ()
  "Test clearing the history."
  (let*
      ((env
        (setup-test-history-env))
       (--el-history-dir
        (plist-get env :dir))
       (--el-history-file
        (plist-get env :file))
       (--el-history
        '()))

    ;; Add some entries
    (--el-history-append "user" "Test message")
    (should
     (=
      (length --el-history)
      1))

    ;; Clear history
    (el-history-clear)
    (should
     (=
      (length --el-history)
      0))

    ;; Load from file to verify it was saved as empty
    (--el-history-load)
    (should
     (=
      (length --el-history)
      0))

    (cleanup-test-history-env env)))

(ert-deftest test-emacs-llm-get-recent-history
    ()
  "Test getting recent history limited by max interactions."
  (let*
      ((env
        (setup-test-history-env))
       (--el-history-dir
        (plist-get env :dir))
       (--el-history-file
        (plist-get env :file))
       (--el-n-histories 2)
       (--el-history
        '()))
    ;; Add several entries
    (--el-history-append "user" "Message 1")
    (--el-history-append "assistant" "Response 1")
    (--el-history-append "user" "Message 2")
    (--el-history-append "assistant" "Response 2")
    (--el-history-append "user" "Message 3")
    (--el-history-append "assistant" "Response 3")
    ;; Get recent history
    (let
        ((recent
          (--el-history-get-recent)))
      (should
       (=
        (length recent)
        2))
      ;; Should have 2 messages (last 2 entries)
      (should
       (string=
        (alist-get 'content
                   (nth 0 recent))
        "Message 3"))
      (should
       (string=
        (alist-get 'content
                   (nth 1 recent))
        "Response 3")))
    (cleanup-test-history-env env)))

(defun --el-history-load
    ()
  "Load conversation history from `--el-history-file`."
  (--el-history-backup-file-if)
  (when
      (file-exists-p --el-history-file)
    (with-temp-buffer
      (insert-file-contents --el-history-file)
      (let
          ((json-array-type 'list))
        (setq --el-history
              (json-read-from-string
               (buffer-string)))))))

(provide 'test-emacs-llm-history)

(when
    (not load-file-name)
  (message "test-emacs-llm-history.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))