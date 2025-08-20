#include <math.h>
#include <emacs-module.h>

/* The Emacs-specific module API version. */
int plugin_is_GPL_compatible;

/*
  (defun mandelbrot-point (x y n iter-max limit)
    "Calculate the Mandelbrot value for a single pixel (x, y).
  Returns 1 if inside the set, 0 otherwise."
    (let* ((zr 0.0)
           (zi 0.0)
           (tr 0.0)
           (ti 0.0)
           (cr (- (* 2.0 (/ x (float n))) 1.5))
           (ci (- (* 2.0 (/ y (float n))) 1.0))
           (i 0))
      (while (and (< i iter-max) (<= (+ tr ti) limit))
        (setq zi (+ (* 2.0 zr zi) ci))
        (setq zr (+ (- tr ti) cr))
        (setq tr (* zr zr))
        (setq ti (* zi zi))
        (setq i (1+ i)))
      (if (<= (+ tr ti) limit)
          1
        0)))
*/

/* The Mandelbrot point calculation in C. */
static emacs_value
mandelbrot_test_point_c(emacs_env *env,
                         ptrdiff_t nargs __attribute__((unused)),
                         emacs_value *args,
                         void *data __attribute__((unused)))
{
    /* Get the arguments and convert them to C types.
       Argument types are checked in the Lisp wrapper function. */
    ptrdiff_t x = env->extract_integer(env, args[0]);
    ptrdiff_t y = env->extract_integer(env, args[1]);
    double n = env->extract_float(env, args[2]);
    ptrdiff_t iter_max = env->extract_integer(env, args[3]);
    double limit = env->extract_float(env, args[4]);

    double zr = 0.0, zi = 0.0, tr = 0.0, ti = 0.0;

    double cr = (2.0 * x / n) - 1.5;
    double ci = (2.0 * y / n) - 1.0;

    ptrdiff_t i = 0;

    while (i < iter_max && (tr + ti) <= limit) {
        zi = 2.0 * zr * zi + ci;
        zr = tr - ti + cr;
        tr = zr * zr;
        ti = zi * zi;
        i++;
    }

    /* Return 1 if inside the set, 0 otherwise. */
    if ((tr + ti) <= limit) {
        return env->make_integer(env, 1);
    } else {
        return env->make_integer(env, 0);
    }
}

/* The module's entry point. */
int emacs_module_init(struct emacs_runtime *runtime)
{
    emacs_env *env = runtime->get_environment(runtime);

    // Bind the C function to a Lisp name.
    emacs_value mandelbrot_test_point_native = env->make_function(
        env,
        5,
        5,
        mandelbrot_test_point_c,
        "Calculate Mandelbrot point using a C extension.",
        NULL);

    // Set the Lisp function 'mandelbrot-test-point-native'.
    env->funcall(env, env->intern(env, "fset"), 2,
        (emacs_value[]) {
            env->intern(env, "mandelbrot-test-point-native"),
            mandelbrot_test_point_native
        });

    // Provide a new Emacs Lisp symbol 'mandelbrot-native'.
    // Use funcall to call the Lisp function `provide`.
    env->funcall(env, env->intern(env, "provide"), 1,
        (emacs_value[]) {
            env->intern(env, "mandelbrot-native")
        });

    return 0;
}
