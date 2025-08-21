;; -*- lexical-binding: t; nameless-current-name: "mandelbrot" -*-

(require 'mandelbrot-native)

(defun mandelbrot--generate-output (n output-file)
  (let ((iter-max 50)
        (limit (* 2.0 2.0))
        (bytes-per-row (ceiling (/ (float n) 8.0)))
        (n-as-float (float n)))
    (with-temp-file output-file
      (set-buffer-file-coding-system 'binary)
      (insert (format "P4\n%d %d\n" n n))
      (dotimes (y n)
        (let ((row-data (make-string bytes-per-row ?\0))
              (byte 0)
              (bit-num 0)
              (row-index 0))
          (dotimes (x n)
            (setq byte
                  (logior
                   (ash byte 1)
                   (mandelbrot-test-point-native x y n-as-float iter-max limit)))
            (setq bit-num (1+ bit-num))
            (when (= bit-num 8)
              (aset row-data row-index byte)
              (setq row-index (1+ row-index))
              (setq byte 0)
              (setq bit-num 0)))
          (when (> bit-num 0)
            (let ((padded-byte (ash byte (- 8 bit-num))))
              (aset row-data row-index padded-byte)))
          (insert row-data))))))

(defun mandelbrot-main (n output-file)
  "Plot the Mandelbrot set and write it to a PBM file.
N is the side length of the square bitmap."
  (condition-case err
      (mandelbrot--generate-output n output-file)
    ((file-error)
     (setq backtrace-on-error-noninteractive nil)
     (error "Error: Could not write to file '%s'. Check permissions."
            output-file))
    (t
     (error "Unhandled error: %S" err))))
