;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-05 08:53:24>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-gif-screencast/tests/test-emacs-gif-screencast.el

;;; test-emacs-gif-screencast.el --- Test for emacs-gif-screencast.el -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for emacs-gif-screencast.el functions and features

;;; Code:

(require 'ert)
(require 'emacs-gif-screencast)

(ert-deftest test-emacs-gif-screencast-loadable
    ()
  "Test if emacs-gif-screencast can be required."
  (should
   (featurep 'emacs-gif-screencast)))

(ert-deftest test-emacs-gif-screencast-output-directory
    ()
  "Test if emacs-gif-screencast-get-output-directory returns correct values."
  (let
      ((original-value emacs-gif-screencast-output-directory)
       (test-dir "/tmp/test-gif-dir"))
    (unwind-protect
        (progn
          ;; Test with nil (should return default-directory)
          (setq emacs-gif-screencast-output-directory nil)
          (should
           (equal
            (emacs-gif-screencast-get-output-directory)
            default-directory))

          ;; Test with custom directory
          (setq emacs-gif-screencast-output-directory test-dir)
          (should
           (equal
            (emacs-gif-screencast-get-output-directory)
            test-dir)))
      ;; Cleanup
      (setq emacs-gif-screencast-output-directory original-value))))

(ert-deftest test-emacs-gif-screencast-detect-wsl
    ()
  "Test WSL detection function."
  (let
      ((original-fn
        (symbol-function 'shell-command-to-string)))
    (unwind-protect
        (progn
          ;; Mock shell-command-to-string for WSL detection
          (cl-letf
              (((symbol-function 'shell-command-to-string)
                (lambda
                  (command)
                  (if
                      (string= command "uname -r")
                      "5.15.90.1-microsoft-standard-WSL2"
                    (funcall original-fn command)))))
            ;; Should detect WSL when on Linux with Microsoft in kernel
            (let
                ((system-type 'gnu/linux))
              (should
               (emacs-gif-screencast-detect-wsl))))

          ;; Should not detect WSL when not on Linux
          (cl-letf
              (((symbol-function 'shell-command-to-string)
                (lambda
                  (command)
                  (if
                      (string= command "uname -r")
                      "5.15.90.1-microsoft-standard-WSL2"
                    (funcall original-fn command)))))
            (let
                ((system-type 'darwin))
              (should-not
               (emacs-gif-screencast-detect-wsl)))))

      ;; Restore original function
      (fset 'shell-command-to-string original-fn))))

(ert-deftest test-emacs-gif-screencast-mode
    ()
  "Test if emacs-gif-screencast-mode toggles correctly."
  (unwind-protect
      (progn
        ;; Test enabling the mode
        (emacs-gif-screencast-mode 1)
        (should emacs-gif-screencast-mode)

        ;; Test disabling the mode
        (emacs-gif-screencast-mode -1)
        (should-not emacs-gif-screencast-mode))
    ;; Cleanup
    (emacs-gif-screencast-mode -1)))

(ert-deftest test-emacs-gif-screencast-variables
    ()
  "Test if all required variables are defined."
  (should
   (boundp 'emacs-gif-screencast-program))
  (should
   (boundp 'emacs-gif-screencast-args))
  (should
   (boundp 'emacs-gif-screencast-log))
  (should
   (boundp 'emacs-gif-screencast-convert-program))
  (should
   (boundp 'emacs-gif-screencast-convert-args))
  (should
   (boundp 'emacs-gif-screencast-resolution))
  (should
   (boundp 'emacs-gif-screencast-screenshot-directory)))

(ert-deftest test-emacs-gif-screencast-cropping-region
    ()
  "Test cropping region calculation."
  (let
      ((frame-position-orig
        (symbol-function 'frame-position))
       (frame-geometry-orig
        (symbol-function 'frame-geometry))
       (frame-pixel-height-orig
        (symbol-function 'frame-pixel-height)))
    (unwind-protect
        (progn
          ;; Mock required functions
          (cl-letf
              (((symbol-function 'frame-position)
                (lambda
                  ()
                  '(100 . 200)))
               ((symbol-function 'frame-geometry)
                (lambda
                  ()
                  '((outer-size .
                                (800 . 0))
                    (title-bar-size .
                                    (0 . 30))
                    (tool-bar-size .
                                   (0 . 40)))))
               ((symbol-function 'frame-pixel-height)
                (lambda
                  ()
                  600)))

            (let
                ((emacs-gif-screencast-title-bar-pixel-height 30))
              (should
               (equal
                (emacs-gif-screencast--cropping-region)
                "800x670+100+200")))))

      ;; Restore original functions
      (fset 'frame-position frame-position-orig)
      (fset 'frame-geometry frame-geometry-orig)
      (fset 'frame-pixel-height frame-pixel-height-orig))))

(ert-deftest test-emacs-gif-screencast-system-dependent-defaults
    ()
  "Test system-dependent default settings."
  (should
   (or
    (and
     (eq system-type 'darwin)
     (equal emacs-gif-screencast-capture-format "ppm"))
    (and
     (not
      (eq system-type 'darwin))
     (equal emacs-gif-screencast-capture-format "png")))))

(provide 'test-emacs-gif-screencast)
;;; test-emacs-gif-screencast.el ends here

(provide 'test-emacs-gif-screencast)

(when
    (not load-file-name)
  (message "test-emacs-gif-screencast.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))