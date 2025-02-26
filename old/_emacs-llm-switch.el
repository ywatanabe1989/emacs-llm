;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-19 11:51:47>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-01-llm-llm/el-switch.el

;;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'el-variables)
(require 'lle-log-loggers)

(defun el-switch
    (provider engine)
  "Switch the LLM provider and engine."
  (interactive
   (let*
       ((provider
         (completing-read
          (format "Switch from \"%s\"?: " --el-default-provider)
          '("deepseek" "openai" "anthropic" "google" "groq")
          nil t nil nil nil))
        (provider-models
         (cond
          ((string= provider "anthropic")
           --el-anthropic-available-engines)
          ((string= provider "google")
           --el-google-available-engines)
          ((string= provider "deepseek")
           --el-deepseek-available-engines)
          ((string= provider "groq")
           --el-groq-available-engines)
          ((string= provider "openai")
           --el-openai-available-engines)))
        (engine
         (completing-read
          (format "Select engine for %s: " provider)
          provider-models nil t nil nil nil)))
     (list provider engine)))
  (setq --el-default-provider provider)
  (setq --el-default-engine engine)
  (message "Switched to: %s with engine: %s" provider engine))

(defun --el-get-engine-and-provider
    (recipe-id)
  "Get engine and provider from recipe if specified, otherwise return (nil . nil) when weights are non-positive."
  (let*
      ((recipe-path
        (expand-file-name
         (concat recipe-id ".yaml")
         (--lle-path-get-system-resources-prompts-recipes-dir)))
       (recipe
        (--lle-utils-read-yaml-file recipe-path))
       (engines
        (alist-get 'engine recipe))
       (random-value
        (/
         (float
          (random 100))
         100.0)))
    (cond
     ((or
       (null engines)
       (not
        (listp
         (car engines))))
      '(nil . nil))
     ((=
       (length engines)
       1)
      (let*
          ((eng
            (car engines))
           (weight
            (car eng))
           (prov
            (nth 1 eng))
           (eng-name
            (nth 2 eng)))
        (if
            (<= weight 0.0)
            '(nil . nil)
          (lle-log-debug "single engine=%s provider=%s" eng-name prov)
          (cons eng-name prov))))
     (t
      (let*
          ((sum-weights
            (cl-loop for eng in engines sum
                     (car eng))))
        (if
            (<= sum-weights 0.0)
            '(nil . nil)
          (lle-log-debug
           "random-value=%.2f"
           random-value)
          (let
              (range-list
               (start 0.0)
               selected)
            (dolist
                (eng engines)
              (let*
                  ((weight
                    (car eng))
                   (prov
                    (nth 1 eng))
                   (eng-name
                    (nth 2 eng))
                   (fraction
                    (/
                     (float weight)
                     sum-weights))
                   (end
                    (+ start fraction)))
                (push
                 (list start end eng-name prov)
                 range-list)
                (setq start end)))
            (setq range-list
                  (nreverse range-list))
            (dolist
                (r range-list)
              (lle-log-debug
               "range=[%.2f, %.2f) engine=%s provider=%s"
               (nth 0 r)
               (nth 1 r)
               (nth 2 r)
               (nth 3 r)))
            (dolist
                (r range-list)
              (when
                  (and
                   (>= random-value
                       (nth 0 r))
                   (< random-value
                      (nth 1 r)))
                (lle-log-debug "selected-provider=%s  selected-engine=%s"
                               (nth 3 r)
                               (nth 2 r))
                (setq selected
                      (cons
                       (nth 2 r)
                       (nth 3 r)))
                (cl-return)))
            (if selected
                selected
              (let*
                  ((last-eng
                    (car
                     (last engines)))
                   (last-prov
                    (nth 1 last-eng))
                   (last-name
                    (nth 2 last-eng)))
                (lle-log-debug
                 "selected-provider=%s  selected-engine=%s"
                 last-prov last-name)
                (cons last-name last-prov))))))))))

(provide 'el-switch)

(when
    (not load-file-name)
  (message "el-switch.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))