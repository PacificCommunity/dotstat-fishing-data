source("src/functions.R")

main_dsd_sources <- c(
  "biomass",
  "catch",
  "cpue",
  "f_aggregate",
  "f_annual",
  "f_season",
  "f_stage",
  "natage",
  "summary"
)

names(main_dsd_sources) <- main_dsd_sources

# indicator variable names as they appear in output data
raw_indicators <- c(
  "ssb",
  "t",
  "obs",
  "pred",
  "f",
  "n",
  "rec",
  "catch",
  "tb",
  "sb",
  "sbf0",
  "dep"
)

all_df <- main_dsd_sources |>
  map_df(
    \(x) x |>
      TAF_obj_url() |>
      read_csv() |>
      indicators_to_long(raw_indicators) |>
      mutate(
        across(
          any_of(c("season", "area", "fishery", "age", "stage")),
          as.character
        )
      ),
    .id = "source"
  )

all_df |>
  mutate(
  season = replace_na(season,"_T"),
  area = replace_na(area,"_T"),
  fishery = replace_na(fishery,"_T"),
  age = replace_na(age,"_T"),
  stage = replace_na(stage,"_T"),
  )
