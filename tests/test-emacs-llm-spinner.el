;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 15:52:14>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/tests/test-emacs-llm-spinner.el

(require 'ert)
(require 'emacs-llm-spinner)

(ert-deftest test-emacs-llm-spinner-creation
    ()
  "Test spinner creation."
  (let
      ((--el-buffer-name
        (generate-new-buffer-name "*Test-Spinner*"))
       (--el-spinner-timer nil)
       (--el-spinner-marker nil))
    (get-buffer-create --el-buffer-name)
    (--el--start-spinner)
    (should --el-spinner-timer)
    (should --el-spinner-marker)
    (--el--stop-spinner)
    (kill-buffer --el-buffer-name)))

(ert-deftest test-emacs-llm-spinner-cancellation
    ()
  "Test spinner cancellation."
  (let
      ((--el-buffer-name
        (generate-new-buffer-name "*Test-Spinner*"))
       (--el-spinner-timer nil)
       (--el-spinner-marker nil))
    (get-buffer-create --el-buffer-name)
    (--el--start-spinner)
    (--el--stop-spinner)
    (should-not --el-spinner-timer)
    (kill-buffer --el-buffer-name)))

;; Test: test-emacs-llm-spinner-marker-position
;; Status: ABORTED
;; Error:
;; (ert-test-failed
;;  ((should
;;    (=
;;     (marker-position --el-spinner-marker)
;;     (point-max)))
;;   :form
;;   (= 13 14)
;;   :value nil))

(ert-deftest test-emacs-llm-spinner-marker-position
    ()
  "Test spinner marker position."
  (let
      ((--el-buffer-name
        (generate-new-buffer-name "*Test-Spinner*"))
       (--el-spinner-timer nil)
       (--el-spinner-marker nil))
    (with-current-buffer
        (get-buffer-create --el-buffer-name)
      (insert "Initial text")
      (--el--start-spinner)
      (should
       (=
        (marker-position --el-spinner-marker)
        (1-
         (point-max))))
      (--el--stop-spinner)
      (kill-buffer --el-buffer-name))))

(provide 'test-emacs-llm-spinner)

(when
    (not load-file-name)
  (message "test-emacs-llm-spinner.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))