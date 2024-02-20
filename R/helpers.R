deriv <- function(f, x = NULL) {
  fx <- parse(text = f)
  dx <- D(fx, "x")
  l <- list("fx" = fx, "dx" = dx)
  if (!is.null(x)) {
    y <- eval(fx, list(x = x))
    m <- eval(dx, list(x = x))
    b <- y - m * x
    l <- append(l, list("y" = y, "m" = m, "x" = x, "b" = b))
  }
  return(l)
}

plot_deriv <- function(deriv, x_range, n = 100, ...) {
  xs <- seq(x_range[1], x_range[2], length.out = n)
  ys <- eval(deriv$fx, list(x = xs))
  plot(xs, ys, ...)
  points(deriv$x, deriv$y, col = "red")
  abline(a = deriv$b, b = deriv$m, col = "red")
}