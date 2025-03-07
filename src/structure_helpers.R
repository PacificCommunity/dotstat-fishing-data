library(tidyverse)

DF_FISH_CATCH <- read_csv("data/DF_FISH_CATCH.csv")

DF_FISH_CATCH |>
  select(FISHERY) |>
  unique() |>
  filter(FISHERY |> str_starts("2023_YFT")) |>
  mutate(
    name = paste("Fishery", str_extract(FISHERY,"[^_]+$")),
    parent_id = "2023_YFT"
  ) |>
  write_csv("")