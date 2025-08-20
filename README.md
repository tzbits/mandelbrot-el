# mandelbrot-el

Output pbm of mandelbrot set

# Install

```
make install
```

This builds the project and copies the lisp code to
`~/.emacs.d/site-lisp/mandelbrot` and the shell script to
`~/.local/bin/mandelbrot`.

Tested on on ubuntu 24.04 LTS.

Dependencies:

```
sudo apt install gcc make emacs emacs-el
```

Inspired by the [benchmarks game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/mandelbrot.html#mandelbrot) and [Down the Mandelbrot Rabbit Hole](https://youtu.be/oJgjU-Ad_Fc).
