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

full_df <- all_df |>
  # fill in missing dimension as _T dimensions
  # we don't remove NA from season yet, as we want
  # to distinguish annual and quarterly data
  mutate(
    area = replace_na(area,"_T"),
    fishery = replace_na(fishery,"_T"),
    age = replace_na(age,"_T"),
    stage = replace_na(stage,"_T"),
  ) |>
  # input frequency of observation (annual if not seasonal)
  # and write time period in SDMX style (2024, 2025-1)
  mutate(
    YEAR_MODEL = "2023",
    SPECIES = "YFT",
    FREQ = if_else(is.na(season), "A", "Q"),
    TIME_PERIOD = if_else(
      FREQ == "Q",
      paste(as.character(year),season,sep = "-"),
      as.character(year)
    ),
    # ARE and FISHERY are relative to year and fish species
    # so we write that dependency explicitly
    # unless they are NA or "all", in which case they refer to _T
    AREA = if_else(
        area |> str_ends("[:digit:]"),
      paste0("2023_YFT_", area),
      "_T"
    ),
    FISHERY = if_else(
        fishery |> str_ends("[:digit:]"),
      paste(AREA,fishery,sep = "_"),
      "_T"
    )
  ) |>
  # capitalise names where necessary
  rename(
    AGE := age,
    STAGE := stage,
  ) |>
  # input observation attributes
  mutate(
    UNIT_MEASURE = case_when(
      INDICATOR == "f" ~ "RATIO",
      .default = "",
    ),
    COMMENT = "",
    OBS_STATUS = "",
  )

DF_FISH_CATCH <- full_df |>
  select(
    YEAR_MODEL,
    SPECIES,
    FREQ,
    TIME_PERIOD,
    AREA,
    FISHERY,
    INDICATOR,
    OBS_VALUE,
    UNIT_MEASURE,
    COMMENT,
    OBS_STATUS
  )

write_csv(DF_FISH_CATCH,"data/DF_FISH_CATCH.csv")