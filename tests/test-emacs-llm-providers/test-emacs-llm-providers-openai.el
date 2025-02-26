;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 16:49:11>
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
      (let
          ((payload
            (--el-construct-openai-payload "Test prompt")))
        (should
         (stringp payload))
        (let*
            ((json-object-type 'alist)
             (data
              (json-read-from-string payload)))
          (should
           (string=
            (alist-get 'model data)
            "gpt-4o"))
          (should
           (=
            (alist-get 'temperature data)
            0.7))
          (should
           (eq
            (alist-get 'stream data)
            t))
          (let
              ((messages
                (alist-get 'messages data)))
            (should
             (vectorp messages))
            (should
             (=
              (length messages)
              1))
            (let
                ((message
                  (aref messages 0)))
              (should
               (string=
                (alist-get 'role message)
                "user"))
              (should
               (string=
                (alist-get 'content message)
                "Test prompt")))))))))

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