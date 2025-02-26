;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-26 16:49:13>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-spinner.el

;;;; Stream processing

(defvar --el-spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames for the spinner animation.")

(defvar --el-spinner-timer nil
  "Timer for the spinner animation.")

(defvar --el-spinner-index 0
  "Current index in the spinner animation.")

(defvar --el-spinner-marker nil
  "Marker for spinner position.")

(defun --el-start-spinner
    ()
  "Start the spinner animation in the LLM buffer."
  (when
      (and
       (get-buffer --el-buffer-name)
       (not --el-spinner-timer))
    (with-current-buffer --el-buffer-name
      (goto-char
       (point-max))
      (setq --el-spinner-marker
            (point-marker))
      (insert " ")
      (setq --el-spinner-timer
            (run-with-timer 0 0.1
                            (lambda
                              ()
                              (when
                                  (buffer-live-p
                                   (marker-buffer --el-spinner-marker))
                                (with-current-buffer
                                    (marker-buffer --el-spinner-marker)
                                  (save-excursion
                                    (goto-char --el-spinner-marker)
                                    (delete-char 1)
                                    (insert
                                     (propertize
                                      (nth --el-spinner-index --el-spinner-frames)
                                      'face
                                      '(:foreground "blue")))
                                    (setq --el-spinner-index
                                          (mod
                                           (1+ --el-spinner-index)
                                           (length --el-spinner-frames))))))))))))

;; (defun --el-stop-spinner
;;     ()
;;   "Stop the spinner animation."
;;   (when --el-spinner-timer
;;     (canc--el-timer --el-spinner-timer)
;;     (setq --el-spinner-timer nil)
;;     (when
;;         (and --el-spinner-marker
;;              (marker-buffer --el-spinner-marker))
;;       (with-current-buffer
;;           (marker-buffer --el-spinner-marker)
;;         (save-excursion
;;           (goto-char --el-spinner-marker)
;;           (delete-char 1))))))

(defun --el-stop-spinner
    ()
  "Stop the spinner animation."
  (when --el-spinner-timer
    (cancel-timer --el-spinner-timer)
    (setq --el-spinner-timer nil)
    (when
        (and --el-spinner-marker
             (marker-buffer --el-spinner-marker))
      (with-current-buffer
          (marker-buffer --el-spinner-marker)
        (save-excursion
          (goto-char --el-spinner-marker)
          (delete-char 1))))))

(provide 'emacs-llm-spinner)

(when
    (not load-file-name)
  (message "emacs-llm-spinner.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))