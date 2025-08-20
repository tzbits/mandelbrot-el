.POSIX:

PREFIX = ${HOME}/.local
CFLAGS += -fPIC -std=c99 -pedantic -Wall -Wextra -Os $(pkg-config --cflags emacs)
LDFLAGS = -shared
LDLIBS += $(pkg-config --libs emacs)
CC = gcc

all: mandelbrot-native.so mandelbrot.elc

mandelbrot-native.so: mandelbrot-native.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o mandelbrot-native.so \
          mandelbrot-native.c $(LDLIBS)

mandelbrot.elc: mandelbrot.el
	emacs --batch --no-site-file \
	-L . -f batch-byte-compile mandelbrot.el

clean:
	rm -f mandelbrot-native.so mandelbrot.elc
	rm -f $(PREFIX)/bin/mandelbrot

install: mandelbrot-native.so mandelbrot.elc
	mkdir -p $(HOME)/.emacs.d/site-lisp/mandelbrot
	cp mandelbrot.el mandelbrot.elc mandelbrot-native.so $(HOME)/.emacs.d/site-lisp/mandelbrot/.
	cp mandelbrot.sh $(PREFIX)/bin/mandelbrot
