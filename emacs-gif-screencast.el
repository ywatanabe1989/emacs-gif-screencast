;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-05 08:57:07>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-gif-screencast/emacs-gif-screencast.el

;; Copyright (C) 2025 Yusuke Watanabe <ywatanabe@alumni.u-tokyo.ac>

;;; Commentary:
;; Call `emacs-gif-screencast' to start a recording.
;;
;; A screenshot is taken for every user action.
;;
;; Call `emacs-gif-screencast-stop' (<f9> by default) to finish recording and create
;; the GIF result.

;;; Group and Custom Variables

(defgroup emacs-gif-screencast nil
  "Predefined configurations for `emacs-gif-screencast'."
  :group 'multimedia)

(defcustom emacs-gif-screencast-program
  (if
      (eq system-type 'darwin)
      "screencapture" "scrot")
  "A program for taking screenshots.
See also `emacs-gif-screencast-capture-format'."
  :group 'emacs-gif-screencast
  :type 'string)

(defcustom emacs-gif-screencast-args
  '("--quality" "25" "--focused")
  "Arguments to `screencast-program'.
\"scrot\" can use `--focused' to restrict the capture to the Emacs frame."
  :group 'emacs-gif-screencast
  :type
  '(repeat string))

(defcustom emacs-gif-screencast-log "*emacs-gif-screencast-log*"
  "Name of the buffer logging the actions.
The log is made of the standard output and standard error of the
various programs run here."
  :group 'emacs-gif-screencast
  :type 'string)

(defvar emacs-gif-screencast-convert-program "convert"
  "A program for converting the screenshots to a GIF.")

(defcustom emacs-gif-screencast-convert-args
  '("-delay" "100" "-loop" "0" "-dither" "None" "-colors" "80" "-fuzz" "40%" "-layers" "OptimizeFrame")
  "Arguments to `emacs-gif-screencast-convert-program'."
  :group 'emacs-gif-screencast
  :type
  '(repeat string))

(defcustom emacs-gif-screencast-resolution "800x600"
  "Resolution for the output GIF. Format: WIDTHxHEIGHT (e.g. \"800x600\")."
  :group 'emacs-gif-screencast
  :type 'string)

(defvar emacs-gif-screencast-cropping-program "mogrify"
  "A program for cropping the screenshots.
If `emacs-gif-screencast-cropping-program' is not found, cropping will be skipped.")

(defcustom emacs-gif-screencast-cropping-args nil
  "Arguments to `emacs-gif-screencast-cropping-program'.
Don't specify \"-format\" and \"-crop\" since these commands are used
as the default arguments."
  :group 'emacs-gif-screencast
  :type
  '(repeat string))

(defcustom emacs-gif-screencast-want-optimized t
  "If non-nil, run `emacs-gif-screencast-optimize' over the resulting GIF."
  :group 'emacs-gif-screencast
  :type 'boolean)

(defcustom emacs-gif-screencast-optimize-program "gifsicle"
  "A program for optimizing GIF files."
  :group 'emacs-gif-screencast
  :type 'string)

(defcustom emacs-gif-screencast-optimize-args
  '("--batch" "--optimize=3")
  "Arguments to `emacs-gif-screencast-optimize-program'."
  :group 'emacs-gif-screencast
  :type
  '(repeat string))

(defcustom emacs-gif-screencast-countdown 3
  "Countdown before recording.
0 disables countdown."
  :group 'emacs-gif-screencast
  :type 'integer)

(defcustom emacs-gif-screencast-screenshot-directory
  (format "%s/emacs%d"
          (or
           (getenv "TMPDIR")
           "/tmp")
          (user-uid))
  "Output directory for temporary screenshots."
  :group 'emacs-gif-screencast
  :type 'string)

(defcustom emacs-gif-screencast-autoremove-screenshots t
  "If non nil, remove the temporary screenshots after a successful compilation of the GIF."
  :group 'emacs-gif-screencast
  :type 'boolean)

(defcustom emacs-gif-screencast-output-directory nil
  "Output directory for the GIF file.
If nil, use `default-directory' as the output location."
  :group 'emacs-gif-screencast
  :type
  '(choice
    (const :tag "Use default-directory" nil)
    (directory :tag "Specific directory")))

(defcustom emacs-gif-screencast-capture-format
  (if
      (eq system-type 'darwin)
      "ppm" "png")
  "Image format to store the captured images.
If you are a macOS user, \"ppm\" should be specified."
  :group 'emacs-gif-screencast
  :type 'string)

(defcustom emacs-gif-screencast-title-bar-pixel-height
  (cdr
   (alist-get 'title-bar-size
              (frame-geometry)))
  "Height of title bar for cropping screenshots."
  :group 'emacs-gif-screencast
  :type 'integer)

(defcustom emacs-gif-screencast-enable-ffmpeg t
  "Use FFmpeg for GIF generation instead of ImageMagick."
  :group 'emacs-gif-screencast
  :type 'boolean)

(defcustom emacs-gif-screencast-use-native-capture nil
  "Use Emacs' built-in functions for capturing screenshots instead of external programs."
  :group 'emacs-gif-screencast
  :type 'boolean)

;;; Internal Variables

(defvar emacs-gif-screencast--frames nil
  "A frame is a plist in the form '(:time :file :offset).")

(defvar emacs-gif-screencast--offset 0
  "Delay accumulated by all the pauses.")

(defvar emacs-gif-screencast--offset-mark 0
  "Timestamp when user hit pause.")

(defvar emacs-gif-screencast--counter 0
  "Number of running screenshots.")

(defvar emacs-gif-screencast-mode-map
  (let
      ((map
        (make-sparse-keymap)))
    (define-key map
                (kbd "<f9>")
                #'emacs-gif-screencast-stop)
    (define-key map
                (kbd "<f8>")
                #'emacs-gif-screencast-toggle-pause)
    map)
  "Keymap of `emacs-gif-screencast-mode'.")

;;; Environment Detection & Setup

(defun emacs-gif-screencast-detect-wsl
    ()
  "Detect if running in WSL."
  (and
   (eq system-type 'gnu/linux)
   (string-match-p "Microsoft"
                   (shell-command-to-string "uname -r"))))

(defun emacs-gif-screencast-setup-defaults
    ()
  "Setup default tools based on environment."
  (when
      (emacs-gif-screencast-detect-wsl)
    (setq emacs-gif-screencast-use-native-capture t
          emacs-gif-screencast-enable-ffmpeg t)))

;; Call this function when the package loads
(emacs-gif-screencast-setup-defaults)

;;; Core Functions

(define-minor-mode emacs-gif-screencast-mode
  "emacs-gif-screencast bindings"
  :init-value nil
  :global t
  :require 'emacs-gif-screencast
  :keymap emacs-gif-screencast-mode-map)

(defun emacs-gif-screencast-capture-frame
    ()
  "Capture current frame using Emacs' built-in functions."
  (let*
      ((time
        (current-time))
       (file
        (expand-file-name
         (concat
          (format-time-string "screen-%F-%T-%3N" time)
          ".png")
         emacs-gif-screencast-screenshot-directory))
       (frame
        (selected-frame)))

    ;; Ensure we're not showing any message during capture
    (message nil)

    ;; Wait for any screen updates
    (redisplay t)

    ;; Take the screenshot using Emacs' built-in function
    (when
        (fboundp 'x-export-frames)
      (with-temp-file file
        (insert
         (x-export-frames frame 'png))))

    ;; Add the frame to our collection
    (push
     (cons
      (time-subtract time emacs-gif-screencast--offset)
      file)
     emacs-gif-screencast--frames)))

(defun emacs-gif-screencast-capture
    ()
  "Save result of `screencast-program` to `screencast-output-dir`."
  (if emacs-gif-screencast-use-native-capture
      (emacs-gif-screencast-capture-frame)
    (let*
        ((time
          (current-time))
         (file
          (expand-file-name
           (concat
            (format-time-string "screen-%F-%T-%3N" time)
            "." emacs-gif-screencast-capture-format)
           emacs-gif-screencast-screenshot-directory)))
      (setq emacs-gif-screencast--counter
            (+ emacs-gif-screencast--counter 1))
      (let
          ((p
            (apply 'start-process
                   emacs-gif-screencast-program
                   (get-buffer-create emacs-gif-screencast-log)
                   emacs-gif-screencast-program
                   (append emacs-gif-screencast-args
                           (list file)))))
        (set-process-sentinel p 'emacs-gif-screencast-capture-sentinel))
      (push
       (cons
        (time-subtract time emacs-gif-screencast--offset)
        file)
       emacs-gif-screencast--frames))))

(defun emacs-gif-screencast-capture-sentinel
    (_proc _status)
  "Sentinel for screen capturing."
  (setq emacs-gif-screencast--counter
        (- emacs-gif-screencast--counter 1))
  (emacs-gif-screencast--finish))

(defun emacs-gif-screencast--finish
    ()
  "Finish screen capturing."
  (when
      (and
       (not emacs-gif-screencast-mode)
       (= emacs-gif-screencast--counter 0))
    (if emacs-gif-screencast-enable-ffmpeg
        (emacs-gif-screencast-generate-with-ffmpeg)
      (if
          (memq window-system
                '(mac ns))
          (emacs-gif-screencast--crop)
        (emacs-gif-screencast--generate-gif nil nil)))))

(defun emacs-gif-screencast-generate-with-ffmpeg
    ()
  "Generate GIF using ffmpeg directly from the screenshot files."
  (interactive)
  (let
      ((output-file
        (expand-file-name
         (format-time-string "emacs-gif-screenshot-%F-%T.gif"
                             (current-time))
         (or
          (and
           (file-writable-p
            (emacs-gif-screencast-get-output-directory))
           (emacs-gif-screencast-get-output-directory))
          (read-directory-name "Save output to directory: "))))
       (temp-file "/tmp/ffmpeg-file-list.txt")
       (files
        (mapcar 'cdr emacs-gif-screencast--frames)))

    ;; Create file list for ffmpeg
    (with-temp-file temp-file
      (dolist
          (file files)
        (insert
         (format "file '%s'\n" file)
         "duration 0.2\n")))
                                        ; 0.2 seconds between frames
    (message "Generating GIF with ffmpeg...")
    (let
        ((p
          (start-process
           "ffmpeg"
           (get-buffer-create emacs-gif-screencast-log)
           "ffmpeg"
           "-f" "concat" "-safe" "0" "-i" temp-file
           "-vf"
           (format "scale=%s:force_original_aspect_ratio=decrease,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" emacs-gif-screencast-resolution)
           "-y" output-file)))
      (set-process-sentinel
       p
       (lambda
         (process event)
         (emacs-gif-screencast-print-status process event)
         (when
             (and
              (eq
               (process-status process)
               'exit)
              (=
               (process-exit-status process)
               0))
           (message "GIF created successfully: %s" output-file)
           (when emacs-gif-screencast-autoremove-screenshots
             (dolist
                 (f emacs-gif-screencast--frames)
               (delete-file
                (cdr f))))))))))

(defun emacs-gif-screencast--generate-gif
    (process event)
  "Generate GIF file."
  (when process
    (emacs-gif-screencast-print-status process event))
  (let
      (delays
       (index 0)
       (frames emacs-gif-screencast--frames)
       (files
        '()))
    ;; Create delay arguments
    (while
        (cdr frames)
      (push
       (list "(" "-clone"
             (number-to-string index)
             "-set" "delay"
             ;; Converters delays are expressed in centiseconds.
             (format "%d"
                     (* 100
                        (float-time
                         (time-subtract
                          (car
                           (cadr frames))
                          (caar frames)))))
             ")" "-swap"
             (number-to-string index)
             "+delete")
       delays)
      (setq index
            (1+ index)
            frames
            (cdr frames)))
    ;; Collect all image files directly
    (dolist
        (frame emacs-gif-screencast--frames)
      (push
       (cdr frame)
       files))
    (message "Compiling GIF with %s..." emacs-gif-screencast-convert-program)
    (let
        ((output
          (expand-file-name
           (format-time-string "output-%F-%T.gif"
                               (current-time))
           (or
            (and
             (file-writable-p
              (emacs-gif-screencast-get-output-directory))
             (emacs-gif-screencast-get-output-directory))
            (read-directory-name "Save output to directory: "))))
         p)
      ;; Use individual files instead of @ syntax to avoid policy restrictions
      (setq p
            (apply 'start-process
                   emacs-gif-screencast-convert-program
                   (get-buffer-create emacs-gif-screencast-log)
                   emacs-gif-screencast-convert-program
                   (append emacs-gif-screencast-convert-args
                           (reverse files)
                           ;; Add files directly
                           ;; Delays must come after the file arguments.
                           (apply 'nconc delays)
                           (list output))))
      (set-process-sentinel p
                            (lambda
                              (process event)
                              (emacs-gif-screencast-print-status process event)
                              (when
                                  (and emacs-gif-screencast-want-optimized
                                       (eq
                                        (process-status process)
                                        'exit)
                                       (=
                                        (process-exit-status process)
                                        0))
                                (emacs-gif-screencast-optimize output))
                              (when
                                  (and emacs-gif-screencast-autoremove-screenshots
                                       (eq
                                        (process-status process)
                                        'exit)
                                       (=
                                        (process-exit-status process)
                                        0))
                                (dolist
                                    (f emacs-gif-screencast--frames)
                                  (delete-file
                                   (cdr f)))))))))

(defun emacs-gif-screencast--cropping-region
    ()
  "Return the cropping region of the captured image."
  (let
      ((x
        (car
         (frame-position)))
       (y
        (cdr
         (frame-position)))
       (width
        (car
         (alist-get 'outer-size
                    (frame-geometry))))
       (height
        (+
         (frame-pixel-height)
         (or emacs-gif-screencast-title-bar-pixel-height 0)
         (cdr
          (alist-get 'tool-bar-size
                     (frame-geometry))))))
    (format "%dx%d+%d+%d" width height x y)))

(defun emacs-gif-screencast--crop
    ()
  "Crop the captured images to the active region of selected frame."
  (when
      (and
       (not emacs-gif-screencast-mode)
       (= emacs-gif-screencast--counter 0))
    (if
        (executable-find emacs-gif-screencast-cropping-program)
        (progn
          (message "Cropping captured images with %s..."
                   emacs-gif-screencast-cropping-program)
          (let
              ((process-connection-type nil)
               (p
                (apply 'start-process
                       "cropping"
                       (get-buffer-create emacs-gif-screencast-log)
                       emacs-gif-screencast-cropping-program
                       (append
                        '("-format")
                        (list
                         (format "%s" emacs-gif-screencast-capture-format))
                        '("-crop")
                        (list
                         (emacs-gif-screencast--cropping-region))
                        emacs-gif-screencast-cropping-args
                        (mapcar 'cdr emacs-gif-screencast--frames)))))
            (set-process-sentinel p 'emacs-gif-screencast--generate-gif)))
      (message "Cropping program '%s' not found (See `emacs-gif-screencast-cropping-program')" emacs-gif-screencast-cropping-program)
      (emacs-gif-screencast--generate-gif nil nil))))

;;; User Commands
;;;###autoload

(defun emacs-gif-screencast
    ()
  "Start recording the GIF.
A screenshot is taken before every command runs."
  (interactive)
  (if emacs-gif-screencast-mode
      (message "emacs-gif-screencast already running")
    (if
        (and
         (not emacs-gif-screencast-use-native-capture)
         (not
          (executable-find emacs-gif-screencast-program)))
        (message "Screenshot program '%s' not found (See `emacs-gif-screencast-program')" emacs-gif-screencast-program)
      (dolist
          (d
           (list
            (emacs-gif-screencast-get-output-directory)
            emacs-gif-screencast-screenshot-directory))
        (unless
            (file-exists-p d)
          (make-directory d 'parents)))
      (setq emacs-gif-screencast--frames
            '())
      (setq emacs-gif-screencast--counter 0)
      (emacs-gif-screencast-mode 1)
      (dolist
          (i
           (number-sequence emacs-gif-screencast-countdown 1 -1))
        (message "Start recording GIF in %s..." i)
        (sleep-for 0.7))
      (message "Go! (Press %s to stop, %s to pause)"
               (substitute-command-keys "\\[emacs-gif-screencast-stop]")
               (substitute-command-keys "\\[emacs-gif-screencast-toggle-pause]"))
      (add-hook 'pre-command-hook 'emacs-gif-screencast-capture))))

(defun emacs-gif-screencast-toggle-pause
    ()
  "Toggle recording of the GIF."
  (interactive)
  (if
      (memq 'emacs-gif-screencast-capture
            (default-value 'pre-command-hook))
      (progn
        (remove-hook 'pre-command-hook 'emacs-gif-screencast-capture)
        (setq emacs-gif-screencast--offset-mark
              (current-time))
        (message "GIF recording paused. (Press %s to stop, %s to resume)"
                 (substitute-command-keys "\\[emacs-gif-screencast-stop]")
                 (substitute-command-keys "\\[emacs-gif-screencast-toggle-pause]")))
    (setq emacs-gif-screencast--offset
          (time-to-seconds
           (time-subtract
            (current-time)
            emacs-gif-screencast--offset-mark)))
    (add-hook 'pre-command-hook 'emacs-gif-screencast-capture)
    (message "GIF recording resumed. (Press %s to stop, %s to pause)"
             (substitute-command-keys "\\[emacs-gif-screencast-stop]")
             (substitute-command-keys "\\[emacs-gif-screencast-toggle-pause]"))))

(defun emacs-gif-screencast-stop
    ()
  "Stop recording and compile GIF."
  (interactive)
  (remove-hook 'pre-command-hook 'emacs-gif-screencast-capture)
  (emacs-gif-screencast-mode 0)
  (setq emacs-gif-screencast--frames
        (nreverse emacs-gif-screencast--frames))
  (emacs-gif-screencast--finish))

;;; Helper Functions

(defun emacs-gif-screencast-get-output-directory
    ()
  "Get the output directory for GIF files.
If `emacs-gif-screencast-output-directory' is nil, use `default-directory'."
  (if
      (not emacs-gif-screencast-output-directory)
      default-directory
    emacs-gif-screencast-output-directory))

(defun emacs-gif-screencast-print-status
    (process event)
  "Output PROCESS EVENT to minibuffer."
  (princ
   (format "Process '%s' %s"
           process
           (progn
             (while
                 (string-match "\n+\\|\r+" event)
               (setq event
                     (replace-match "" t t event)))
             event))))

(defun emacs-gif-screencast-optimize
    (file)
  "Optimize GIF FILE asynchronously."
  (message "Optimizing with %s..." emacs-gif-screencast-optimize-program)
  (let
      ((p
        (apply 'start-process
               emacs-gif-screencast-optimize-program
               (get-buffer-create emacs-gif-screencast-log)
               emacs-gif-screencast-optimize-program
               (append emacs-gif-screencast-optimize-args
                       (list file)))))
    (set-process-sentinel p 'emacs-gif-screencast-print-status)))

(with-eval-after-load 'emacs-gif-screencast
  (define-key emacs-gif-screencast-mode-map
              (kbd "<f8>")
              'emacs-gif-screencast-toggle-pause)
  (define-key emacs-gif-screencast-mode-map
              (kbd "<f9>")
              'emacs-gif-screencast-stop))

(provide 'emacs-gif-screencast)

(when
    (not load-file-name)
  (message "emacs-gif-screencast.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))