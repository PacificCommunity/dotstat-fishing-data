library(testthat)
source("src/functions.R")


year_of_model <- "2023"
species_in_model <- "YFT"

this_repo_name <- paste(
  "ofp-sam",
  str_to_lower(species_in_model),
  year_of_model,
  "diagnostic",
  sep = "-")

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
      TAF_obj_url(project_name = this_repo_name) |>
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
  group_by(year,season,area,INDICATOR) |>
  count()

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
    YEAR_MODEL = year_of_model,
    SPECIES = species_in_model,
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
      paste(year_of_model, species_in_model, area, sep = "-"),
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
      INDICATOR == "f" ~ "I", # instantaneous rate
      INDICATOR %in% c("obs","pred") ~ "Q", # model quantity
      INDICATOR %in% c("sb", "tb", "sbf0", "catch") ~ "T", # tonnes
      INDICATOR == "n" ~ "NI", # number of individuals
      INDICATOR == "rec" ~ "NR", # number of recruits
      INDICATOR == "dep" ~ "R", # ratio
      .default = "",
    ),
    COMMENT = "",
    OBS_STATUS = ""
  )

DF_FISH_CATCH <- full_df |>
  select(
    YEAR_MODEL, # dim
    SPECIES, # dim
    FREQ, # dim common
    TIME_PERIOD, # dim common
    AREA, # dim common
    FISHERY, # dim
    AGE, # dim
    STAGE, # dim
    INDICATOR, # dim common
    OBS_VALUE, # measure
    UNIT_MEASURE, # attr
    COMMENT, # attr
    OBS_STATUS # attr
  )

DF_FISH_CATCH$STAGE |> unique()

# Data output sanity testing:

test_that("We don't create conflicting observation values binding the tables together",
  expect_equal(
    0,
    DF_FISH_CATCH |>
      add_count(TIME_PERIOD,AREA,FISHERY,INDICATOR,AGE,STAGE) |>
      filter(n > 1) |>
      nrow()
  )
)


test_that("No unit of measure is forgotten",
  expect_equal(
    0,
    DF_FISH_CATCH |>
      filter(is_empty(UNIT_MEASURE)) |>
      nrow()
  )
)

write_csv(DF_FISH_CATCH,
  paste0("data/DF_FISH_CATCH_",
  year_of_model, "_",
  species_in_model, "_",
  ".csv"))
