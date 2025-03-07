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