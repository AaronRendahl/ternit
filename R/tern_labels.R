tern_labels <- function(frame, labels, axis.label.spacing, axis.label.location) {
  bind_rows(
    frame |> filter(.data$b==min(.data$b)) |>
      summarize(a=(max(.data$a) - min(.data$a))*axis.label.location + min(.data$a),
                b=min(.data$b)-axis.label.spacing) |> mutate(c=1-.data$b-.data$a),
    frame |> filter(.data$c==min(.data$c)) |>
      summarize(b=(max(.data$b) - min(.data$b))*axis.label.location + min(.data$b),
                c=min(.data$c)-axis.label.spacing) |> mutate(a=1-.data$c-.data$b),
    frame |> filter(.data$a==min(.data$a)) |>
      summarize(c=(max(.data$c) - min(.data$c))*axis.label.location + min(.data$c),
                a=min(.data$a)-axis.label.spacing) |> mutate(b=1-.data$a-.data$c)
  ) |> mutate(tern_xy(.data$a, .data$b, .data$c)) |>
  mutate(txt=labels)
}

tern_arrows <- function(frame, labels, axis.label.spacing, axis.label.location) {
  gap1 <- 0.1
  gap2 <- 0.05
  bind_rows(
    frame |> filter(.data$b==min(.data$b)) |>
      summarize(a1=min(.data$a) + gap1,
                a2=(max(.data$a) - min(.data$a))*axis.label.location + min(.data$a) - gap2,
                b1=min(.data$b)-axis.label.spacing) |>
      mutate(b2=b1, c1=1-.data$b1-.data$a1, c2=1-.data$b2-.data$a2),
    frame |> filter(.data$c==min(.data$c)) |>
      summarize(b2=(max(.data$b) - min(.data$b))*axis.label.location + min(.data$b) - gap2,
                b1=min(.data$b) + gap1,
                c1=min(.data$c)-axis.label.spacing) |>
      mutate(c2=c1, a1=1-.data$c1-.data$b1, a2=1-.data$c2-.data$b2),
    frame |> filter(.data$a==min(.data$a)) |>
      summarize(c2=(max(.data$c) - min(.data$c))*axis.label.location + min(.data$c) - gap2,
                c1=min(.data$c)+gap1,
                a1=min(.data$a)-axis.label.spacing) |>
      mutate(a2=a1, b1=1-.data$a1-.data$c1, b2=1-.data$a2-.data$c2)
  ) |>
    mutate(tern_xy(.data$a1, .data$b1, .data$c1)) |> rename(x1='x', y1='y') |>
    mutate(tern_xy(.data$a2, .data$b2, .data$c2)) |> rename(x2='x', y2='y')
}
