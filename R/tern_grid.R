tern_grid <- function(by, lims) {
  alim <- lims[1,]
  blim <- lims[2,]
  clim <- lims[3,]

  at <- seq(0, 1, by=by)
  at.a <- at[at > alim[1] & at < alim[2]]
  at.b <- at[at > blim[1] & at < blim[2]]
  at.c <- at[at > clim[1] & at < clim[2]]

  bind_rows(
    tibble(a1=at.a, a2=at.a,
           b2=pmin(blim[2], 1-at.a-clim[1]),
           c1=pmin(clim[2], 1-at.a-blim[1])) |>
      mutate(b1=1-a1-c1, c2=1-a2-b2),
    tibble(b1=at.b, b2=at.b,
           c2=pmin(clim[2], 1-at.b-alim[1]),
           a1=pmin(alim[2], 1-at.b-clim[1])) |>
      mutate(c1=1-a1-b1, a2=1-b2-c2),
    tibble(c1=at.c, c2=at.c,
           a2=pmin(alim[2], 1-at.c-blim[1]),
           b1=pmin(blim[2], 1-at.c-alim[1])) |>
      mutate(a1=1-b1-c1, b2=1-a2-c2)) |>
    select(a1, b1, c1, a2, b2, c2) |>
    mutate(getxy(a1, b1, c1)) |> rename(x1=x, y1=y) |>
    mutate(getxy(a2, b2, c2)) |> rename(x2=x, y2=y)
}
