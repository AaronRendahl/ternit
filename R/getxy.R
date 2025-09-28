getxy <- function(a, b, c) {
  tibble(x = 0.5 * (a + 2 * b) / (a + b + c),
         y = 0.5 * sqrt(3) * a / (a + b + c))
}
