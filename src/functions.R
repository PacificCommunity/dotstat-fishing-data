library(tidyverse)
library(janitor)

# creates url poiting toward (last version, main branch)
# a specific raw csv file in PacificCommunity github
# doesn't try anything fancy such as authorization
# csv only
TAF_obj_url <- function(object_name,
  project_name = "ofp-sam-yft-2023-diagnostic",
  TAF_phase = "output/",
  base_url = "https://raw.githubusercontent.com/PacificCommunity/") {

  endpoint <- "/refs/heads/main/TAF/"

  url <- paste0(
    base_url, project_name, endpoint, TAF_phase, object_name, ".csv"
  )

  return(url)

}

# pivot indicator variables to long data
# selecting indicator from a provided vector of names
# robust to some indicators _not_ being there
# but all indicators should be included 
indicators_to_long <- function(df, raw_inds) {
df |>
  pivot_longer(
    any_of(raw_inds),
    names_to = "INDICATOR",
    values_to = "OBS_VALUE"
  )
}


read_csv_taf <- function(taf_name, repo_name) {
  taf_name |>
      TAF_obj_url(project_name = this_repo_name) |>
      read_csv() |>
      indicators_to_long(raw_indicators) |>
      mutate(
        across(
          any_of(c("season", "area", "fishery", "age", "stage")),
          as.character
        )
      )
}

read_csv_taf_poss <- possibly(
  read_csv_taf,
  tibble()
)

get_repo_name <- function(species,year) {
  this_repo_name <- paste(
    "ofp-sam",
    str_to_lower(species),
    year,
    "diagnostic",
    sep = "-")
}

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
