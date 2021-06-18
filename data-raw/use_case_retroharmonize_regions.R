devtools::load_all()
here::here()

## Not ready yet for working, a code element got lost. ---------------------------

library(dplyr, quietly = T)
library(tidyr)
library(stringr)
library(lubridate)
source("not_included/daniel_env.R")
eb <- dir ( gesis_dir )
eurobarometer_rounds <- file.path(gesis_dir, eb)

#Not run in the blogpost. In the repo we have a saved version.

dont_run <- function() {
  climate_change_files <- c("ZA5877_v2-0-0.sav", "ZA6595_v3-0-0.sav",  "ZA6861_v1-2-0.sav", 
                            "ZA7488_v1-0-0.sav", "ZA7572_v1-0-0.sav")
  
  eb_waves <- read_surveys(file.path(gesis_dir, climate_change_files), .f='read_spss')
  
  if (dir.exists("data-raw")) {
    save ( eb_waves,  
           file = file.path("data-raw", "eb_climate_change_waves.rda") )
  }
}

if ( file.exists( here::here("data-raw", "eb_climate_change_waves.rda") )) {
  load (here::here( "data-raw", "eb_climate_change_waves.rda" ) )
} 


characters <- geography_chars %>% 
  separate ( data = ., 
             col = .data$rowid,
             into = c("survey_id", "version", "unique_id"), 
             sep = "_") %>%
  select ( -all_of("unique_id")) %>%
  distinct_all() %>%
  pivot_longer( cols = -all_of(c( "survey_id", "version", "isocntry", "region_nuts_codes")), 
                names_to = "variable_name", 
                values_to = "character_value") %>%
  filter ( ! grepl( "^Inap|99", character_value) ) %>%
  filter ( ! is.na(character_value))

labels <- geography_labels %>% 
  separate ( data = ., col = rowid,
             into = c("survey_id", "version", "unique_id"), 
             sep = "_") %>%
  select ( -all_of("unique_id") ) %>%
  distinct_all() %>%
  pivot_longer( cols = -all_of(c( "survey_id", "version", "isocntry", "region_nuts_codes")), 
                names_to = "variable_name", 
                values_to = "value_label") %>%
  filter ( ! is.na(  value_label) ) %>%
  filter ( ! grepl("^Inap", value_label) ) %>%
  rename ( region_nuts_names  = region_nuts_codes )

coding_information <- characters %>%
  full_join ( labels, 
              by = c("survey_id", "version", "isocntry", "variable_name")
              ) %>%
  mutate ( nuts3_code = case_when ( 
    nchar(region_nuts_codes)== 5 ~ region_nuts_codes, 
    isocntry == "LU"             ~ "LU000",
    TRUE ~ NA_character_ )) %>%
  mutate ( nuts2_code = case_when ( 
    nchar(region_nuts_codes)== 4 ~ region_nuts_codes, 
    .data$isocntry == "LU"             ~ "LU00",
    .data$isocntry == "CY"             ~ "CY00", 
    .data$isocntry == "MT"             ~ "MT00",
    TRUE ~ NA_character_ )) %>%
  mutate ( nuts1_code = case_when ( 
    nchar(region_nuts_codes)== 3 ~ region_nuts_codes, 
    isocntry == "LU"             ~ "LU0",
    isocntry == "CY"             ~ "CY0", 
    isocntry == "MT"             ~ "MT0",
    TRUE ~ NA_character_ ))

cy_metadata <- coding_information %>% filter ( isocntry == "CY")

mt_metadata <- coding_information %>% filter ( isocntry == "MT")
