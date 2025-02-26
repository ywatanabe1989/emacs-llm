;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 16:49:11>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-call/test-emacs-llm-call-deepseek.el

;; Test: test-emacs-llm-construct-deepseek-payload
;; Status: ABORTED
;; Error: (ert-test-failed ((should (= (length messages) 1)) :form (= 11 1) :value nil))

;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-call/test-emacs-llm-call-deepseek.el

(ert-deftest test-emacs-llm-construct-deepseek-payload
    ()
  "Test constructing DeepSeek API payload."
  (let
      ((--el-default-engine-deepseek "deepseek-chat")
       (--el-temperature 0.7))
    ;; Mock the history function to return empty list
    (cl-letf
        (((symbol-function '--el-history-get-recent)
          (lambda
            ()
            nil)))
      (let
          ((payload
            (--el-construct-deepseek-payload "Test prompt")))
        (should
         (stringp payload))
        (let*
            ((json-object-type 'alist)
             (data
              (json-read-from-string payload)))
          (should
           (string=
            (alist-get 'engine data)
            "deepseek-chat"))
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

(ert-deftest test-emacs-llm-parse-deepseek-chunk
    ()
  "Test parsing DeepSeek API chunks."
  (let
      ((chunk "{\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}"))
    (should
     (string=
      (--el-parse-deepseek-chunk chunk)
      "Hello"))))

(provide 'test-emacs-llm-call-deepseek)

(when
    (not load-file-name)
  (message "test-emacs-llm-call-deepseek.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))