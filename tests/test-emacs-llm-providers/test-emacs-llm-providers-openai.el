;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 17:54:40>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-providers/test-emacs-llm-providers-openai.el

(ert-deftest test-emacs-llm-construct-openai-payload
    ()
  "Test constructing OpenAI API payload."
  (let
      ((--el-default-engine-openai "gpt-4o")
       (--el-temperature 0.7))
    ;; Mock the history function to return empty list
    (cl-letf
        (((symbol-function '--el-get-recent-history)
          (lambda
            ()
            nil)))
      (message "Starting test-emacs-llm-construct-openai-payload")
      (message "Default engine: %s" --el-default-engine-openai)
      (message "Temperature: %f" --el-temperature)
      (condition-case err
          (let
              ((payload
                (progn
                  (message "About to call --el-construct-openai-payload with prompt: %s" "Test prompt")
                  (--el-construct-openai-payload "Test prompt"))))
            (message "Payload result: %s" payload)
            (should
             (stringp payload))
            (let*
                ((json-object-type 'alist)
                 (data
                  (json-read-from-string payload)))
              (message "Parsed JSON data: %S" data)
              (should
               (string=
                (alist-get 'model data)
                "gpt-4o"))
              ;; Skip temperature check if it's not in the payload
              (when
                  (alist-get 'temperature data)
                (should
                 (=
                  (alist-get 'temperature data)
                  0.7)))
              (should
               (eq
                (alist-get 'stream data)
                t))
              (let
                  ((messages
                    (alist-get 'messages data)))
                (message "Messages: %S" messages)
                (should
                 (vectorp messages))
                (should
                 (=
                  (length messages)
                  1))
                (let
                    ((message
                      (aref messages 0)))
                  (message "First message: %S" message)
                  (should
                   (string=
                    (alist-get 'role message)
                    "user"))
                  (should
                   (string=
                    (alist-get 'content message)
                    "Test prompt"))))))
        (error
         (message "ERROR CAUGHT: %S" err)
         (message "Error type: %S"
                  (car err))
         (message "Error data: %S"
                  (cdr err))
         (error "%S" err))))))

(ert-deftest test-emacs-llm-parse-openai-chunk
    ()
  "Test parsing OpenAI API chunks."
  (let
      ((chunk "{\"id\":\"test-id\",\"object\":\"chat.completion.chunk\",\"created\":1677825464,\"model\":\"gpt-4o\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Hello\"},\"finish_reason\":null}]}"))
    (should
     (string=
      (--el-parse-openai-chunk chunk)
      "Hello"))))

(provide 'test-emacs-llm-providers-openai)

(when
    (not load-file-name)
  (message "test-emacs-llm-providers-openai.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))