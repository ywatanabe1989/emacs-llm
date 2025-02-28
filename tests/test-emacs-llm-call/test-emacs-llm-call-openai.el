;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 08:32:00>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-call/test-emacs-llm-call-openai.el

(require 'ert)

(ert-deftest test-openai-curl-command-execution
    ()
  "Tests if OpenAI curl command executes successfully."
  (let*
      ((--el-default-engine-openai "o3-mini")
       (--el-api-key-openai
        (getenv "OPENAI_API_KEY"))
       (curl-cmd
        (--el-construct-openai-curl-command "test"))
       (temp-buffer
        (generate-new-buffer "*test-curl-output*")))
    (unwind-protect
        (progn
          (should --el-api-key-openai)
          (let
              ((proc
                (start-process-shell-command
                 "test-curl" temp-buffer curl-cmd)))
            (set-process-sentinel
             proc
             (lambda
               (proc _event)
               (with-current-buffer
                   (process-buffer proc)
                 (goto-char
                  (point-min))
                 (should
                  (search-forward "\"content\":" nil t)))))
            (accept-process-output proc 5)))
      (kill-buffer temp-buffer))))

(ert-deftest test-openai-curl-command-format
    ()
  "Tests if OpenAI curl command is properly formatted."
  (let*
      ((--el-default-engine-openai "o3-mini")
       (--el-api-key-openai "test-key")
       (curl-cmd
        (--el-construct-openai-curl-command "test")))
    (should
     (string-match-p "^curl -N https://api.openai.com/v1/chat/completions" curl-cmd))
    (should
     (string-match-p "-H \"Content-Type: application/json\"" curl-cmd))
    (should
     (string-match-p "-H \"Authorization: Bearer test-key\"" curl-cmd))
    (should
     (string-match-p "-d '[{]" curl-cmd))))

(ert-deftest test-parse-openai-engine-o3-mini
    ()
  "Tests parsing o3-mini engine variants."
  (should
   (equal
    (--el-parse-openai-engine "o3-mini")
    '("o3-mini" . nil))))

(ert-deftest test-parse-openai-engine-o3-mini-low
    ()
  "Tests parsing o3-mini-low engine variant."
  (should
   (equal
    (--el-parse-openai-engine "o3-mini-low")
    '("o3-mini" . "low"))))

(ert-deftest test-parse-openai-engine-o1
    ()
  "Tests parsing o1 engine variants."
  (should
   (equal
    (--el-parse-openai-engine "o1")
    '("o1" . nil))))

(ert-deftest test-parse-openai-engine-o1-high
    ()
  "Tests parsing o1-high engine variant."
  (should
   (equal
    (--el-parse-openai-engine "o1-high")
    '("o1" . "high"))))

(ert-deftest test-parse-openai-engine-gpt4o
    ()
  "Tests parsing gpt-4o engine."
  (should
   (equal
    (--el-parse-openai-engine "gpt-4o")
    '("gpt-4o" . nil))))

(ert-deftest test-parse-openai-engine-fallback
    ()
  "Tests fallback behavior for unknown engines."
  (should
   (equal
    (--el-parse-openai-engine "unknown-engine")
    '("unknown-engine" . nil))))

(ert-deftest test-parse-openai-chunk-content
    ()
  "Tests parsing content from OpenAI chunk."
  (let
      ((chunk "{\"choices\":[{\"delta\":{\"content\":\"test\"}}]}"))
    (should
     (string=
      (--el-parse-openai-chunk chunk)
      "test"))))

(ert-deftest test-parse-openai-chunk-reasoning
    ()
  "Tests parsing reasoning efforts from OpenAI chunk."
  (let
      ((chunk "{\"choices\":[{\"delta\":{\"reasoning_efforts\":\"thinking\"}}]}"))
    (should
     (string=
      (--el-parse-openai-chunk chunk)
      "thinking"))))

(ert-deftest test-parse-openai-chunk-empty
    ()
  "Tests parsing empty OpenAI chunk."
  (should
   (null
    (--el-parse-openai-chunk "[DONE]"))))

(ert-deftest test-construct-openai-payload-basic
    ()
  "Tests constructing basic OpenAI payload."
  (let
      ((--el-default-engine-openai "o3-mini")
       (json-payload
        (--el-construct-openai-payload "test")))
    (should
     (stringp json-payload))
    (let*
        ((json-object-type 'alist)
         (data
          (json-read-from-string json-payload)))
      (should
       (string=
        (alist-get 'model data)
        "o3-mini"))
      (should
       (eq
        (alist-get 'stream data)
        t)))))

(ert-deftest test-emacs-llm-parse-openai-chunk
    ()
  "Test parsing OpenAI API chunks."
  (let
      ((chunk "{\"id\":\"test-id\",\"object\":\"chat.completion.chunk\",\"created\":1677825464,\"engine\":\"gpt-4o\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Hello\"},\"finish_reason\":null}]}"))
    (should
     (string=
      (--el-parse-openai-chunk chunk)
      "Hello"))))

(provide 'test-emacs-llm-call-openai)

(when
    (not load-file-name)
  (message "test-emacs-llm-call-openai.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))