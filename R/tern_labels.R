tern_labels <- function(frame, labels, axis.label.spacing) {
  bind_rows(
    frame |> filter(b==min(b)) |> summarize(a=(max(a) + min(a))/2, b=min(b)-axis.label.spacing) |> mutate(c=1-b-a),
    frame |> filter(c==min(c)) |> summarize(b=(max(b) + min(b))/2, c=min(c)-axis.label.spacing) |> mutate(a=1-c-b),
    frame |> filter(a==min(a)) |> summarize(c=(max(c) + min(c))/2, a=min(a)-axis.label.spacing) |> mutate(b=1-a-c)
  ) |> mutate(tern_xy(a, b, c)) |>
  mutate(txt=labels)
}
