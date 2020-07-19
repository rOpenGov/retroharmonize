library(retroharmonize)
library(dplyr)
source(file.path("not_included", "daniel_env.R"))
testvars <- readRDS(file.path("example", "data-raw", "trust_eb_9waves.rds"))
spss_files <- unique ( testvars$filename)[2:9]

for ( i in seq_along(spss_files)) { 
  
  this_file <- spss_files[i]
  message ( this_file )
  this_survey <- read_spss(
    file =  file.path(gesis_dir, this_file ))
  
  saveRDS( this_survey %>%
    select ( all_of(testvars %>%
                      filter ( filename == this_file ) %>%
                      pull ( var_name_orig ) ) ), 
    file = file.path(
      "example", "data-raw", paste0(substr(attr(this_survey, "id"), 1,6), "-trust.rds")
      ))
  } 

