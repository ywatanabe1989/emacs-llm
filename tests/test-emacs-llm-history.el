;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-28 08:19:20>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-history.el

;;; test-emacs-llm-history.el --- Tests for emacs-llm-history.el -*- lexical-binding: t -*-

(require 'ert)
(require 'emacs-llm-history)

(ert-deftest test-history-directory-existence
    ()
  (should
   (file-directory-p --el-history-dir)))

(ert-deftest test-history-file-creation
    ()
  (--el-history-ensure-file)
  (should
   (file-exists-p --el-history-file)))

(ert-deftest test-history-append
    ()
  (let
      ((test-content "Test content")
       (test-role "user")
       (original-history
        (when
            (file-exists-p --el-history-file)
          (with-temp-buffer
            (insert-file-contents --el-history-file)
            (buffer-string)))))
    (unwind-protect
        (progn
          (--el-history-append test-role test-content)
          (--el-history-load)
          (should
           (seq-some
            (lambda
              (entry)
              (and
               (equal
                (alist-get 'role entry)
                test-role)
               (equal
                (alist-get 'content entry)
                test-content)))
            --el-history)))
      ;; Cleanup
      (when original-history
        (with-temp-file --el-history-file
          (insert original-history))))))

(ert-deftest test-history-append-with-template
    ()
  (let
      ((test-content "Test with template")
       (test-role "user")
       (test-template "code")
       (original-history
        (when
            (file-exists-p --el-history-file)
          (with-temp-buffer
            (insert-file-contents --el-history-file)
            (buffer-string)))))
    (unwind-protect
        (progn
          (--el-history-append test-role test-content test-template)
          (--el-history-load)
          (should
           (seq-some
            (lambda
              (entry)
              (and
               (equal
                (alist-get 'role entry)
                test-role)
               (equal
                (alist-get 'content entry)
                test-content)
               (equal
                (alist-get 'template entry)
                test-template)))
            --el-history)))
      ;; Cleanup
      (when original-history
        (with-temp-file --el-history-file
          (insert original-history))))))

(ert-deftest test-history-clear
    ()
  (let
      ((original-history
        (when
            (file-exists-p --el-history-file)
          (with-temp-buffer
            (insert-file-contents --el-history-file)
            (buffer-string)))))
    (unwind-protect
        (progn
          ;; First ensure we have some content
          (--el-history-append "assistant" "Some content")
          (el-history-clear)
          (--el-history-load)
          (should
           (null --el-history)))
      ;; Cleanup
      (when original-history
        (with-temp-file --el-history-file
          (insert original-history))))))

(ert-deftest test-history-load-recent
    ()
  (let
      ((original-history
        (when
            (file-exists-p --el-history-file)
          (with-temp-buffer
            (insert-file-contents --el-history-file)
            (buffer-string)))))
    (unwind-protect
        (progn
          ;; Create a fresh test history
          (with-temp-file --el-history-file
            (insert "[]"))

          ;; Directly create a JSON history with known content
          (let
              ((test-history
                (make-list
                 (+ --el-n-histories 5)
                 nil)))
            (dotimes
                (idx
                 (length test-history))
              (setf
               (nth idx test-history)
               `(("role" . "user")
                 ("content" . ,(format "Message %d" idx)))))

            ;; Write the test history to file
            (with-temp-file --el-history-file
              (insert
               (json-encode test-history)))

            ;; Now test the load recent function
            (let*
                ((recent-history
                  (--el-history-load-recent))
                 (first-entry
                  (car recent-history)))
              (should
               (=
                (length recent-history)
                --el-n-histories))
              (should first-entry)
              ;; Check the structure matches what we expect
              (should
               (assoc "content" first-entry))
              (should
               (string-match-p "Message [0-9]+"
                               (cdr
                                (assoc "content" first-entry)))))))

      ;; Cleanup
      (when original-history
        (with-temp-file --el-history-file
          (insert original-history))))))

(ert-deftest test-history-rotation
    ()
  (let
      ((original-history
        (when
            (file-exists-p --el-history-file)
          (with-temp-buffer
            (insert-file-contents --el-history-file)
            (buffer-string))))
       (original-max-size --el-history-max-size)
       (test-max-size 100))
    (unwind-protect
        (progn
          ;; Temporarily set lower max-size for testing
          (setq --el-history-max-size test-max-size)

          ;; Create a history file larger than test-max-size
          (with-temp-file --el-history-file
            (insert "[")
            (dotimes
                (idx 20)
              (unless
                  (= idx 0)
                (insert ","))
              (insert
               (format "{\"role\":\"user\",\"content\":\"Test message %d with extra padding to make the file larger\"}" idx)))
            (insert "]"))

          ;; Count initial backup files
          (let
              ((initial-backups
                (length
                 (directory-files --el-history-dir nil "history-.*\\.json"))))
            ;; Trigger rotation
            (--el-history-ensure-rotation)

            ;; Check if new backup was created
            (should
             (>
              (length
               (directory-files --el-history-dir nil "history-.*\\.json"))
              initial-backups))))
      ;; Cleanup
      (when original-history
        (with-temp-file --el-history-file
          (insert original-history)))
      (setq --el-history-max-size original-max-size))))

(ert-deftest test-history-show-buffer-creation
    ()
  ;; Just test that the buffer gets created with minimal assumptions
  (unwind-protect
      (progn
        ;; Kill buffer if it exists
        (when
            (get-buffer "*Emacs-LLM History*")
          (kill-buffer "*Emacs-LLM History*"))

        ;; Add a basic entry to history (without relying on file operations)
        (let
            ((--el-history
              '(((role . "user")
                 (content . "Test query")))))
          ;; Show history
          (cl-letf
              (((symbol-function '--el-history-load)
                (lambda
                  ()
                  --el-history)))
            (el-history-show)))

        ;; Check buffer was created
        (should
         (get-buffer "*Emacs-LLM History*")))

    ;; Cleanup
    (when
        (get-buffer "*Emacs-LLM History*")
      (kill-buffer "*Emacs-LLM History*"))))

(provide 'test-emacs-llm-history)

(when
    (not load-file-name)
  (message "test-emacs-llm-history.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))