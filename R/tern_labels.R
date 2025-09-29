tern_labels <- function(frame, labels, axis.label.spacing) {
  bind_rows(
    frame |> filter(.data$b==min(.data$b)) |> summarize(a=(max(.data$a) + min(.data$a))/2, b=min(.data$b)-axis.label.spacing) |> mutate(c=1-.data$b-.data$a),
    frame |> filter(.data$c==min(.data$c)) |> summarize(b=(max(.data$b) + min(.data$b))/2, c=min(.data$c)-axis.label.spacing) |> mutate(a=1-.data$c-.data$b),
    frame |> filter(.data$a==min(.data$a)) |> summarize(c=(max(.data$c) + min(.data$c))/2, a=min(.data$a)-axis.label.spacing) |> mutate(b=1-.data$a-.data$c)
  ) |> mutate(tern_xy(.data$a, .data$b, .data$c)) |>
  mutate(txt=labels)
}
