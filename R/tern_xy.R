#' Convert a ternary triplet to Cartesian coordinates
#'
#' @param a first
#' @param b second
#' @param c third
#'
#' @returns a tibble with x/y parameters
#' @export
tern_xy <- function(a, b, c) {
  tibble(x = 0.5 * (a + 2 * b) / (a + b + c),
         y = 0.5 * sqrt(3) * a / (a + b + c))
}
