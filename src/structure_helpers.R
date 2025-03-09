library(tidyverse)

DF_FISH_CATCH <- read_delim("data/DF_FISH_CATCH_2023_YFT.csv", delim = ";")

DF_FISH_CATCH |>
  select(FISHERY) |>
  unique() |>
  filter(FISHERY |> str_starts("2023_YFT")) |>
  mutate(
    name = paste("Fishery", str_extract(FISHERY,"[^_]+$")),
    parent_id = "2023_YFT"
  ) |>
  write_csv("structs/Fishery_cl.csv", append = TRUE)

DF_FISH_CATCH <- read_delim("data/DF_FISH_CATCH_2023_BET.csv", delim = ";")

DF_FISH_CATCH |>
  select(FISHERY) |>
  unique() |>
  filter(FISHERY |> str_starts("2023_YFT")) |>
  mutate(
    name = paste("Fishery", str_extract(FISHERY,"[^_]+$")),
    parent_id = "2023_YFT"
  ) |>
  write_csv("structs/Fishery_cl.csv", append = TRUE)