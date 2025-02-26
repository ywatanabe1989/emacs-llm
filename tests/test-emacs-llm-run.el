;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 15:17:12>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-run.el

(ert-deftest test-emacs-llm-process-chunk
    ()
  "Test processing of API response chunks."
  (let*
      ((temp-buffer
        (generate-new-buffer "*temp-output*"))
       (target-buffer
        (generate-new-buffer "*target-buffer*"))
       (proc
        (start-process "test-proc" temp-buffer "echo" "test"))
       (test-parse-func
        (lambda
          (data)
          (if
              (string= data "test-data")
              "parsed-text" nil))))

    (process-put proc 'target-buffer target-buffer)
    (process-put proc 'content "")
    (process-put proc 'partial-data "")

    ;; Test with complete data chunk
    (--el--process-chunk proc "data: test-data\n" test-parse-func)
    (with-current-buffer target-buffer
      (should
       (string=
        (buffer-string)
        "parsed-text")))
    (should
     (string=
      (process-get proc 'content)
      "parsed-text"))

    ;; Test with partial/incomplete data
    (process-put proc 'content "")
    (process-put proc 'partial-data "")
    (--el--process-chunk proc "da" test-parse-func)
    (should
     (string=
      (process-get proc 'partial-data)
      "da"))
    (should
     (string=
      (process-get proc 'content)
      ""))

    (--el--process-chunk proc "ta: test-data\n" test-parse-func)
    (with-current-buffer target-buffer
      (should
       (string=
        (buffer-string)
        "parsed-textparsed-text")))
    (should
     (string=
      (process-get proc 'content)
      "parsed-text"))

    ;; Cleanup
    (kill-buffer temp-buffer)
    (kill-buffer target-buffer)))

(ert-deftest test-emacs-llm-process-sentinel
    ()
  "Test process sentinel function."
  (let*
      ((temp-buffer
        (generate-new-buffer "*temp-output*"))
       (target-buffer
        (generate-new-buffer "*target-buffer*"))
       (--el-spinner-timer
        (run-with-timer 1 nil
                        (lambda
                          ()
                          nil)))
       (--el-history nil)
       (--el-history-file
        (make-temp-file "emacs-llm-test-history" nil ".json"))
       (proc
        (start-process "test-proc" temp-buffer "echo" "test")))

    (process-put proc 'target-buffer target-buffer)
    (process-put proc 'temp-buffer temp-buffer)
    (process-put proc 'content "Test content")

    ;; Run the sentinel function
    (--el--process-sentinel proc "finished\n")

    ;; Verify the spinner was cancelled
    (should-not --el-spinner-timer)

    ;; Verify temp buffer was killed
    (should-not
     (buffer-live-p temp-buffer))

    ;; Verify content was added to history
    (should
     (=
      (length --el-history)
      1))
    (should
     (string=
      (alist-get 'role
                 (car --el-history))
      "assistant"))
    (should
     (string=
      (alist-get 'content
                 (car --el-history))
      "Test content"))

    ;; Cleanup
    (when
        (buffer-live-p target-buffer)
      (kill-buffer target-buffer))
    (delete-file --el-history-file)))

(provide 'test-emacs-llm-run)

(when
    (not load-file-name)
  (message "test-emacs-llm-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))