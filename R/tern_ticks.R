tern_ticks <- function(grid.spacing, lims, tick.length, tick.label.spacing) {
  alim <- lims[1,]
  blim <- lims[2,]
  clim <- lims[3,]

  tkl <- tick.length
  tkx <- tick.label.spacing

  at <- seq(0, 1, by=grid.spacing)
  at.a <- at[at >= alim[1] - 0.001 & at <= alim[2] + 0.001]
  at.b <- at[at >= blim[1] - 0.001 & at <= blim[2] + 0.001]
  at.c <- at[at >= clim[1] - 0.001 & at <= clim[2] + 0.001]

  # c ticks on a axis
  ax1 <- tibble(c=at.c, b=pmin(blim[2], 1 - at.c - alim[1])) |> mutate(a = 1 - b - c) |>
    mutate(getxy(a, b, c)) |> rename(x1 = x, y1 = y) |>
    mutate(getxy(a - tkl, b + tkl, c)) |> rename(x2 = x, y2 = y) |>
    mutate(getxy(a - tkx, b + tkx, c)) |>
    mutate(txt = c)
  # a ticks on baxis
  ax2 <- tibble(a=at.a, c=pmin(clim[2], 1 - at.a - blim[1])) |> mutate(b = 1 - a - c) |>
    mutate(getxy(a, b, c)) |> rename(x1 = x, y1 = y) |>
    mutate(getxy(a, b - tkl, c + tkl)) |> rename(x2 = x, y2 = y) |>
    mutate(getxy(a, b - tkx, c + tkx)) |>
    mutate(txt = a)
  # b ticks on c axis
  ax3 <- tibble(b=at.b, a=pmin(alim[2], 1 - at.b - clim[1])) |> mutate(c = 1 - b - a) |>
    mutate(getxy(a, b, c)) |> rename(x1 = x, y1 = y) |>
    mutate(getxy(a + tkl, b, c - tkl)) |> rename(x2 = x, y2 = y) |>
    mutate(getxy(a + tkx, b, c - tkx)) |>
    mutate(txt = b)
  bind_rows(ax1, ax2, ax3)
}
