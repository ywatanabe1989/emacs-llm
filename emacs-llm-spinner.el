;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 18:23:15>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-spinner.el

(defvar --el-spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames for the spinner animation.")

(defvar --el-spinner-timer nil
  "Timer for the spinner animation.")

(defvar --el-spinner-index 0
  "Current index in the spinner animation.")

(defvar --el-spinner-marker nil
  "Marker for spinner position.")

;; in this version, spinner is not shown, raising this error
;; --el-start-spinner
;; Prompting user for input
;; Error running timer: (end-of-buffer) [77 times]
;; save-excursion: End of buffer

;; (defun --el-start-spinner
;;     ()
;;   "Start the spinner animation in the LLM buffer."
;;   (when
;;       (and
;;        (get-buffer --el-buffer-name)
;;        (not --el-spinner-timer))
;;     (with-current-buffer --el-buffer-name
;;       ;; Insert initial space at marker position
;;       (when
;;           (and --el-spinner-marker
;;                (marker-buffer --el-spinner-marker))
;;         (save-excursion
;;           (goto-char --el-spinner-marker)
;;           (insert " ")))

;;       ;; Start the timer to animate the spinner
;;       (setq --el-spinner-timer
;;             (run-with-timer 0 0.1
;;                             (lambda
;;                               ()
;;                               (when
;;                                   (and --el-spinner-marker
;;                                        (buffer-live-p
;;                                         (marker-buffer --el-spinner-marker)))
;;                                 (with-current-buffer
;;                                     (marker-buffer --el-spinner-marker)
;;                                   (save-excursion
;;                                     (goto-char --el-spinner-marker)
;;                                     (delete-char 1)
;;                                     (insert
;;                                      (propertize
;;                                       (nth --el-spinner-index --el-spinner-frames)
;;                                       'face
;;                                       '(:foreground "blue")))
;;                                     (setq --el-spinner-index
;;                                           (mod
;;                                            (1+ --el-spinner-index)
;;                                            (length --el-spinner-frames))))))))))))

;; ;; in this version, spinner is shown, but at the start of LLM's response text, instead of the right of engine name

;; (defun --el-start-spinner
;;     ()
;;   "Start the spinner animation in the LLM buffer."
;;   (when
;;       (and
;;        (get-buffer --el-buffer-name)
;;        (not --el-spinner-timer))
;;     (with-current-buffer --el-buffer-name
;;       (goto-char
;;        (point-max))
;;       (setq --el-spinner-marker
;;             (point-marker))
;;       (insert " ")
;;       (setq --el-spinner-timer
;;             (run-with-timer 0 0.1
;;                             (lambda
;;                               ()
;;                               (when
;;                                   (buffer-live-p
;;                                    (marker-buffer --el-spinner-marker))
;;                                 (with-current-buffer
;;                                     (marker-buffer --el-spinner-marker)
;;                                   (save-excursion
;;                                     (goto-char --el-spinner-marker)
;;                                     (delete-char 1)
;;                                     (insert
;;                                      (propertize
;;                                       (nth --el-spinner-index --el-spinner-frames)
;;                                       'face
;;                                       '(:foreground "blue")))
;;                                     (setq --el-spinner-index
;;                                           (mod
;;                                            (1+ --el-spinner-index)
;;                                            (length --el-spinner-frames))))))))))))

;; Method 1: Set the marker at the current point
(setq --el-spinner-marker
      (point-marker))
;; Method 2: Create a marker at a specific position
(setq --el-spinner-marker
      (copy-marker
       (point)))
(set-marker --el-spinner-marker
            (point))

;; Method 3: Create a marker and set insertion type
(setq --el-spinner-marker
      (point-marker))
(set-marker-insertion-type --el-spinner-marker t)

;; Method 4: Position after specific text (example)
(save-excursion
  (goto-char
   (point-min))
  (when
      (search-forward "ENGINE-NAME" nil t)
    (setq --el-spinner-marker
          (point-marker))))

(defun --el-start-spinner
    (&optional marker-position)
  "Start the spinner animation in the LLM buffer."
  (when
      (and
       (get-buffer --el-buffer-name)
       (not --el-spinner-timer))
    (with-current-buffer --el-buffer-name
      (goto-char
       (or marker-position --el-spinner-marker))
      (insert " ")
      (setq --el-spinner-timer
            (run-with-timer 0 0.1
                            (lambda
                              ()
                              (when
                                  (and --el-spinner-marker
                                       (buffer-live-p
                                        (marker-buffer --el-spinner-marker)))
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