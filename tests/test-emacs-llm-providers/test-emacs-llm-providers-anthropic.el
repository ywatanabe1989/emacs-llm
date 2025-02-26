;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 15:18:41>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-providers/test-emacs-llm-providers-anthropic.el

(ert-deftest test-emacs-llm-construct-anthropic-payload
    ()
  "Test constructing Anthropic API payload."
  (let
      ((--el-default-engine-anthropic "claude-3-5-sonnet-20241022")
       (--el-temperature 0.7))
    (let
        ((payload
          (--el-construct-anthropic-payload "Test prompt")))
      (should
       (stringp payload))
      (let*
          ((json-object-type 'alist)
           (data
            (json-read-from-string payload)))
        (should
         (string=
          (alist-get 'model data)
          "claude-3-5-sonnet-20241022"))
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
              "Test prompt"))))))))

(ert-deftest test-emacs-llm-parse-anthropic-chunk
    ()
  "Test parsing Anthropic API chunks."
  (let
      ((chunk "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"text\":\"Hello\"}}"))
    (should
     (string=
      (--el-parse-anthropic-chunk chunk)
      "Hello"))))

(provide 'test-emacs-llm-providers-anthropic)

(when
    (not load-file-name)
  (message "test-emacs-llm-providers-anthropic.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))