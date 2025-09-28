library(tidyverse)
ternex <- read_csv("data-raw/ternex.csv", show_col_types = FALSE) |>
  mutate(Quad=paste0("Q", Quad),
         across(c(Binary, Trio, Quad), factor),
         N=A+B+C,
         A=A/N, B=B/N, C=C/N) |>
  select(-N) |>
  mutate(ID=1:n(), .before=1) |>
  mutate(Binary=fct_recode(Binary, Yes="Y", No="N"))
usethis::use_data(ternex, overwrite=TRUE)
