;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 22:12:08>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/-test-emacs-llm-process.el

(require 'ert)
(require 'emacs-llm-process)

(ert-deftest test-el-cancel-timer-loadable
    ()
  (should
   (fboundp '--el-cancel-timer)))

(ert-deftest test-el-process-chunk-loadable
    ()
  (should
   (fboundp '--el-process-chunk)))

(ert-deftest test-el-process-sentinel-loadable
    ()
  (should
   (fboundp '--el-process-sentinel)))

(ert-deftest test-el-cancel-timer-function
    ()
  (let
      ((--el-spinner-timer
        (run-with-timer 100 nil #'ignore)))
    (unwind-protect
        (progn
          (should --el-spinner-timer)
          (--el-cancel-timer)
          (should
           (null --el-spinner-timer)))
      (when --el-spinner-timer
        (cancel-timer --el-spinner-timer)))))

(ert-deftest test-el-process-chunk-with-complete-data
    ()
  (let*
      ((test-buffer
        (generate-new-buffer "*test-process-chunk*"))
       (test-proc
        (make-process :name "test-process"
                      :buffer nil
                      :command
                      '("true"))))
    (unwind-protect
        (progn
          (process-put test-proc 'target-buffer test-buffer)
          (process-put test-proc 'partial-data "")

          (with-current-buffer test-buffer
            (erase-buffer))

          ;; Test with complete data
          (let
              ((parse-func
                (lambda
                  (json-data)
                  (if
                      (string= json-data "[DONE]")
                      nil
                    "parsed content"))))
            (--el-process-chunk test-proc "data: {\"content\":\"test\"}\n" parse-func)

            (with-current-buffer test-buffer
              (should
               (string=
                (buffer-string)
                "parsed content")))))
      (kill-buffer test-buffer)
      (delete-process test-proc))))

(ert-deftest test-el-process-chunk-with-partial-data
    ()
  (let*
      ((test-buffer
        (generate-new-buffer "*test-process-chunk*"))
       (test-proc
        (make-process :name "test-process"
                      :buffer nil
                      :command
                      '("true"))))
    (unwind-protect
        (progn
          (process-put test-proc 'target-buffer test-buffer)
          (process-put test-proc 'partial-data "")

          (with-current-buffer test-buffer
            (erase-buffer))

          ;; First partial chunk
          (let
              ((parse-func
                (lambda
                  (json-data)
                  (if
                      (string= json-data "[DONE]")
                      nil
                    "parsed content"))))
            (--el-process-chunk test-proc "data: {\"content\":\"pa" parse-func)

            ;; Check that partial data is stored
            (should
             (string=
              (process-get test-proc 'partial-data)
              "data: {\"content\":\"pa"))

            ;; Check that nothing was inserted yet
            (with-current-buffer test-buffer
              (should
               (string=
                (buffer-string)
                "")))

            ;; Second chunk completing the data
            (--el-process-chunk test-proc "rt\"}\n" parse-func)

            ;; Check that buffer now has content
            (with-current-buffer test-buffer
              (should
               (string=
                (buffer-string)
                "parsed content")))))
      (kill-buffer test-buffer)
      (delete-process test-proc))))

(ert-deftest test-el-process-chunk-with-multiple-lines
    ()
  (let*
      ((test-buffer
        (generate-new-buffer "*test-process-chunk*"))
       (test-proc
        (make-process :name "test-process"
                      :buffer nil
                      :command
                      '("true"))))
    (unwind-protect
        (progn
          (process-put test-proc 'target-buffer test-buffer)
          (process-put test-proc 'partial-data "")
          (process-put test-proc 'content "")

          (with-current-buffer test-buffer
            (erase-buffer))

          ;; Test with multiple data lines
          (let
              ((parse-func
                (lambda
                  (json-data)
                  (if
                      (string= json-data "[DONE]")
                      nil
                    (format "parsed-%s-" json-data)))))
            (--el-process-chunk test-proc "data: line1\ndata: line2\ndata: line3\n" parse-func)

            (with-current-buffer test-buffer
              (should
               (string=
                (buffer-string)
                "parsed-line1-parsed-line2-parsed-line3-")))

            ;; Check accumulated content
            (should
             (string=
              (process-get test-proc 'content)
              "parsed-line1-parsed-line2-parsed-line3-"))))
      (kill-buffer test-buffer)
      (delete-process test-proc))))

(ert-deftest test-el-process-sentinel-function
    ()
  (let*
      ((test-buffer
        (generate-new-buffer "*test-process-sentinel*"))
       (test-proc
        (make-process :name "test-process"
                      :buffer nil
                      :command
                      '("true"))))
    (unwind-protect
        (progn
          ;; Setup process properties
          (process-put test-proc 'target-buffer test-buffer)
          (process-put test-proc 'prompt "Test prompt")
          (process-put test-proc 'provider "test-provider")
          (process-put test-proc 'engine "test-engine")
          (process-put test-proc 'content "Test response")

          ;; Mock functions used by the sentinel
          (cl-letf
              (((symbol-function '--el-stop-spinner)
                (lambda
                  ()
                  t))
               ((symbol-function '--el-history-append)
                (lambda
                  (role content)
                  (with-current-buffer test-buffer
                    (insert
                     (format "HISTORY: %s - %s\n" role content))))))

            ;; Test sentinel with finished event
            (--el-process-sentinel test-proc "finished\n")

            ;; Check that history was updated
            (with-current-buffer test-buffer
              (should
               (string-match-p "HISTORY: user - Test prompt"
                               (buffer-string)))
              (should
               (string-match-p "HISTORY: test-engine - Test response"
                               (buffer-string))))))
      (kill-buffer test-buffer)
      (delete-process test-proc))))

(ert-deftest test-el-process-sentinel-with-temp-buffer
    ()
  (let*
      ((test-buffer
        (generate-new-buffer "*test-process-sentinel*"))
       (temp-buffer
        (generate-new-buffer "*test-temp*"))
       (test-proc
        (make-process :name "test-process"
                      :buffer nil
                      :command
                      '("true"))))
    (unwind-protect
        (progn
          ;; Setup process properties
          (process-put test-proc 'target-buffer test-buffer)
          (process-put test-proc 'temp-buffer temp-buffer)
          (process-put test-proc 'prompt "Test prompt")
          (process-put test-proc 'provider "test-provider")
          (process-put test-proc 'engine "test-engine")
          (process-put test-proc 'content "Test response")

          ;; Mock functions used by the sentinel
          (cl-letf
              (((symbol-function '--el-stop-spinner)
                (lambda
                  ()
                  t))
               ((symbol-function '--el-history-append)
                (lambda
                  (&rest _)
                  t)))

            ;; Test sentinel with finished event
            (--el-process-sentinel test-proc "finished\n")

            ;; Check that temp buffer was killed
            (should-not
             (buffer-live-p temp-buffer))))
      (when
          (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (when
          (buffer-live-p temp-buffer)
        (kill-buffer temp-buffer))
      (delete-process test-proc))))

(provide 'test-emacs-llm-process)

(provide '-test-emacs-llm-process)

(when
    (not load-file-name)
  (message "-test-emacs-llm-process.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))