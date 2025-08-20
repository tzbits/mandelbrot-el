#!/bin/sh

if [ "$#" -ne 2 ]; then
  echo "Usage: $0 <side_length> <output_file>"
  exit 1
fi

emacs -Q --batch --no-site-file \
  -L "${HOME}/.emacs.d/site-lisp/mandelbrot" \
  --load "mandelbrot.el" \
  --eval "(mandelbrot-main (string-to-number (nth 1 argv)) (nth 2 argv))" \
  -- "$@"
