library(testthat)
source("src/functions.R")

year_of_model <- "2024"
species_in_model <- "ALB"

this_repo_name <- get_repo_name(species_in_model, year_of_model)

# Get all relevant TAF objects for this year species

all_df <- main_dsd_sources |>
  map_df(
    \(x) x |> read_csv_taf_poss(this_repo_name),
    .id = "source"
  )

# wrangle to dataflow SDMX compatible

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
    FREQ = if_else("season" %in% names(all_df) & is.na(season), "A", "Q"),
    #DATAFLOW = "SPC:DF_SAM_CATCH(1.0)",
    TIME_PERIOD = if_else(
      FREQ == "Q",
      # quarter time format https://wiki.sdmxcloud.org/SDMX_Time_Formats
      # Q	Quarterly	YYYY-Qn	2010-Q1
      paste(as.character(year),season,sep = "-Q"),
      as.character(year)
    ),
    YEAR_MODEL = year_of_model,
    SPECIES = species_in_model,
    # ARE and FISHERY are relative to year and fish species
    # so we write that dependency explicitly
    # unless they are NA or "all", in which case they refer to _T
    AREA = if_else(
        area |> str_ends("[:digit:]"),
      paste(year_of_model, species_in_model, area, sep = "_"),
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
    #DATAFLOW,
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
  ) |>
  unique()

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
 

# write to file

write_delim(
  DF_FISH_CATCH,
  paste0("data/DF_FISH_CATCH_",
    year_of_model, "_",
    species_in_model,
    ".csv"),
  delim = ";"
)

DF_FISH_CATCH$FISHERY |> unique()
DF_FISH_CATCH$AREA |> unique()
