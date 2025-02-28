;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-01 03:55:28>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-llm/emacs-llm-dired.el

;; (defun --el-dired-get-contents
;;     (&optional include-hidden-p)
;;   "Get contents of marked files recursively, handling only safe files. If no files specified, just call ordinal el-on-region"
;;   (interactive)
;;   (let*
;;       ((marked-files
;;         (dired-get-marked-files nil nil
;;                                 (lambda
;;                                   (f)
;;                                   (dired-file-marker f))))
;;        (size-limit
;;         (* 1024 1024 1024))
;;        (contents ""))
;;     (if
;;         (null marked-files)
;;         (read-string "Enter prompt: " "")
;;       (cl-labels
;;           ((process-file
;;              (file)
;;              (cond
;;               ((file-directory-p file)
;;                (let
;;                    ((dir-files
;;                      (directory-files file t)))
;;                  ;; Filter the directory contents using our filter function
;;                  (setq dir-files
;;                        (el-filter-files dir-files
;;                                         el-whitelist-extensions
;;                                         el-whitelist-expressions
;;                                         el-blacklist-extensions
;;                                         el-blacklist-expressions
;;                                         include-hidden-p))
;;                  (dolist
;;                      (f dir-files)
;;                    (process-file f))))
;;               ((and
;;                 (file-regular-p file)
;;                 (<
;;                  (file-attribute-size
;;                   (file-attributes file))
;;                  size-limit))
;;                ;; Check if the file passes our filters
;;                (when
;;                    (car
;;                     (el-filter-files
;;                      (list file)
;;                      el-whitelist-extensions
;;                      el-whitelist-expressions
;;                      el-blacklist-extensions
;;                      el-blacklist-expressions
;;                      include-hidden-p))
;;                  (setq contents
;;                        (concat contents
;;                                (format "\n\n;;; ----- %s -----\n\n" file)
;;                                (with-temp-buffer
;;                                  (insert-file-contents file)
;;                                  (buffer-string)))))))))
;;         (dolist
;;             (file marked-files)
;;           (process-file file)))
;;       contents)))

(defun --el-dired-get-contents
    (&optional include-hidden-p max-depth)
  "Get contents of marked files recursively up to MAX-DEPTH levels deep.
If MAX-DEPTH is nil, a default of 10 is used."
  (interactive)
  (let*
      ((marked-files
        (dired-get-marked-files nil nil
                                (lambda
                                  (f)
                                  (dired-file-marker f))))
       (max-depth
        (or max-depth 10))
       ;; Default max depth
       (size-limit
        (* 1024 1024 1024))
       (contents ""))
    (if
        (null marked-files)
        (read-string "Enter prompt: " "")
      (cl-labels
          ((process-file
             (file depth)
             (cond
              ;; Only process directory if we haven't reached max depth
              ((and
                (file-directory-p file)
                (< depth max-depth))
               (let
                   ((dir-files
                     (directory-files file t)))
                 (setq dir-files
                       (el-filter-files dir-files
                                        el-whitelist-extensions
                                        el-whitelist-expressions
                                        el-blacklist-extensions
                                        el-blacklist-expressions
                                        include-hidden-p))
                 (dolist
                     (f dir-files)
                   (process-file f
                                 (1+ depth)))))
              ;; Process regular file
              ((and
                (file-regular-p file)
                (<
                 (file-attribute-size
                  (file-attributes file))
                 size-limit))
               ;; Check if the file passes our filters
               (when
                   (car
                    (el-filter-files
                     (list file)
                     el-whitelist-extensions
                     el-whitelist-expressions
                     el-blacklist-extensions
                     el-blacklist-expressions
                     include-hidden-p))
                 (setq contents
                       (concat contents
                               (format "\n\n;;; ----- %s -----\n\n" file)
                               (with-temp-buffer
                                 (insert-file-contents file)
                                 (buffer-string)))))))))
        (dolist
            (file marked-files)
          (process-file file 0)))
      contents)))

(provide 'emacs-llm-dired)

(when
    (not load-file-name)
  (message "emacs-llm-dired.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))