;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 16:49:11>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-call/test-emacs-llm-call-google.el

(require 'emacs-llm-call-google)
;; Test: test-emacs-llm-construct-google-payload
;; Status: ABORTED
;; Error: (wrong-type-argument listp [((role . "user") (content . "hi")) ((role . "assistant") (content . "")) ((role . "user") (content . "hi")) ((role . "assistant") (content . "")) ((role . "user") (content . "hi")) ((role . "assistant") (content . "")) ((role . "user") (content . "hi")) ((role . "assistant") (content . "")) ((role . "user") (content . "g")) ((role . "assistant") (content . ""))])

;; Test Google chunk parsing
(ert-deftest test-el-parse-google-chunk
    ()
  (let
      ((valid-chunk "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Hello world\"}]}}]}"))
    (should
     (string=
      (--el-parse-google-chunk valid-chunk)
      "Hello world"))))

(ert-deftest test-el-parse-google-chunk-empty
    ()
  (should
   (eq
    (--el-parse-google-chunk "[DONE]")
    nil)))

(ert-deftest test-el-parse-google-chunk-invalid-json
    ()
  (should
   (eq
    (--el-parse-google-chunk "{invalid json}")
    nil)))

(ert-deftest test-el-parse-google-chunk-missing-content
    ()
  (let
      ((chunk-no-content "{\"candidates\":[{}]}"))
    (should
     (eq
      (--el-parse-google-chunk chunk-no-content)
      nil))))

(ert-deftest test-el-construct-google-payload
    ()
  (let
      ((--el-default-engine-google "gemini-pro")
       (--el-google-engine-max-tokens-alist
        '(("gemini-pro" . 8192)))
       ;; Mock history function to return empty result
       (--el-history-get-recent
        (lambda
          ()
          '())))
    (let
        ((payload
          (--el-construct-google-payload "Test prompt")))
      (should
       (string-match-p "\"role\"\\s-*:\\s-*\"user\"" payload))
      (should
       (string-match-p "\"text\"\\s-*:\\s-*\"Test prompt\"" payload))
      (should
       (string-match-p "\"maxOutputTokens\"\\s-*:\\s-*8192" payload)))))

(ert-deftest test-emacs-llm-parse-google-chunk
    ()
  "Test parsing Google API chunks."
  (let
      ((chunk "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Hello\"}]}}]}"))
    (should
     (string=
      (--el-parse-google-chunk chunk)
      "Hello"))))

(ert-deftest test-el-google-filter
    ()
  (let*
      ((target-buffer
        (generate-new-buffer "*test-target*"))
       (proc
        (start-process "test-proc" nil "echo" "test"))
       (valid-chunk "data: {\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Hello world\"}]}}]}\n"))
    (process-put proc 'target-buffer target-buffer)
    (process-put proc 'partial-data "")
    (process-put proc 'content "")

    ;; Run filter
    (--el-google-filter proc valid-chunk)

    ;; Check results
    (with-current-buffer target-buffer
      (should
       (string=
        (buffer-string)
        "Hello world")))
    (should
     (string=
      (process-get proc 'content)
      "Hello world"))

    ;; Cleanup
    (kill-buffer target-buffer)))

(ert-deftest test-el-google-stream
    ()
  ;; Mocking required functions and variables
  (let
      ((--el-default-engine-google "gemini-pro")
       (--el-api-key-google "test-api-key")
       (constructed-payload nil)
       (prepared-buffer nil)
       (mock-proc nil))

    ;; Mock functions with cl-letf to properly handle the process
    (cl-letf
        (((symbol-function 'start-process)
          (lambda
            (&rest args)
            ;; Create a real process that will be returned
            (setq mock-proc
                  (make-process :name
                                (car args)
                                :buffer
                                (cadr args)
                                :command
                                '("sleep" "0.1")
                                :noquery t))
            mock-proc))
         ((symbol-function '--el-construct-google-payload)
          (lambda
            (prompt)
            (setq constructed-payload prompt)
            "{\"mock\":\"payload\"}"))
         ((symbol-function '--el-prepare-llm-buffer)
          (lambda
            (prompt engine engine template)
            (setq prepared-buffer
                  (list prompt engine engine template))
            "*mock-buffer*"))
         ((symbol-function '--el-start-spinner)
          (lambda
            ()
            nil))
         ((symbol-function '--el-history-append)
          (lambda
            (&rest args)
            nil)))

      ;; Call the function
      (let
          ((result
            (--el-google-stream "Test prompt" "Test template")))
        ;; Check results
        (should
         (string= constructed-payload "Test prompt"))
        (should
         (equal
          (nth 0 prepared-buffer)
          "Test prompt"))
        (should
         (equal
          (nth 1 prepared-buffer)
          "GOOGLE"))
        (should
         (equal
          (nth 2 prepared-buffer)
          "gemini-pro"))
        (should
         (equal
          (nth 3 prepared-buffer)
          "Test template"))
        (should
         (processp result)))

      ;; Cleanup
      (when
          (and mock-proc
               (process-live-p mock-proc))
        (delete-process mock-proc)))))

(provide 'test-emacs-llm-call-google)

(when
    (not load-file-name)
  (message "test-emacs-llm-call-google.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))