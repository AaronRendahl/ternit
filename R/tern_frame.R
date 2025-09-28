tern_frame <- function(lims) {
  a1 <- lims[1,1]
  a2 <- lims[1,2]
  b1 <- lims[2,1]
  b2 <- lims[2,2]
  c1 <- lims[3,1]
  c2 <- lims[3,2]
  #b1 a2 c1 b2 a1 c2
  #b1a2 a2c1 c1b2 b2a1 a1c2 c2b1
  foo <- rbind(
    c(a2, b1, 1 - a2 - b1),
    c(a2, 1 - a2 - c1, c1),
    c(1 - b2 - c1, b2, c1),
    c(a1, b2, 1 - a1 - b2),
    c(a1, 1 - a1 - c2, c2),
    c(1 - b1 - c2, b1, c2))
  colnames(foo) <- letters[1:3]
  as_tibble(foo) |>
    mutate(across(everything(), \(x) round(x, 10))) |>
    mutate(tern_xy(a, b, c)) |>
    unique()
}
