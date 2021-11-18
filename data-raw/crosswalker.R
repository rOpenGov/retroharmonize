#crosswalk

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
  
  eb_climate_waves <- read_surveys(file.path(gesis_dir, climate_change_files), .f='read_spss')
  
  if (dir.exists("data-raw")) {
    save ( eb_climate_waves,  
           file = file.path("data-raw", "eb_climate_change_waves.rda") )
  }
}

if ( file.exists( here::here("data-raw", "eb_climate_change_waves.rda") )) {
  load (here::here( "data-raw", "eb_climate_change_waves.rda" ) )
} 

eb_climate_metadata <- metadata_surveys_create (survey_list = eb_climate_waves)

most_serious_global_problem <- eb_climate_metadata %>% 
  filter ( grepl("serious_world_problems_first", .data$label_orig) |
             .data$var_name_orig == "rowid")  

global_problem_ctable <- crosswalk_table_create(metadata = most_serious_global_problem) %>%
  mutate ( val_label_target = val_label_normalize(.data$val_label_orig) ) %>%
  mutate ( var_name_target = case_when ( 
    .data$var_name_orig == "rowid"  ~ "rowid", 
    TRUE ~ "global_problem")) %>%
  mutate ( na_label_target = case_when (
    grepl("^DK", .data$val_label_orig)    ~ "declined", 
    grepl("^Refus", .data$val_label_orig) ~ "declined",
    TRUE ~ .data$na_label_target
  )) %>%
  mutate ( val_label_target = case_when (
    .data$na_label_target == "declined" ~ "declined",
    TRUE ~ .data$val_label_target), 
    val_numeric_target = case_when (
      .data$na_label_target == "declined" ~ 99998,
      TRUE ~ .data$val_numeric_target), 
    na_numeric_target = case_when (
      .data$na_label_target == "declined" ~ 99998,
      TRUE ~ .data$na_numeric_target)) %>%
  mutate ( class_target = case_when (
             .data$var_name_target == "rowid" ~ "character",
             TRUE ~ .data$class_target))

global_problem_ctable %>%
  select ( all_of(c("var_name_orig", "class_target", "var_name_target", "id")) ) %>%
  distinct_all() %>%
  pivot_wider ( names_from = "id", values_from  = "var_name_orig")



global_problems <- crosswalk (survey_list = eb_climate_waves, 
                              crosswalk_table = global_problem_ctable %>%
                                filter ( id != "ZA7488_v1-0-0"))

eb_demography_metadata  <- eb_climate_metadata %>%
  filter ( grepl( "rowid|isocntry|^d8$|^d7$|^wex|^w1$|d25|^d15a|^d11$", .data$var_name_orig) ) %>%
  filter ( .data$filename %in% most_serious_global_problem$filename ) 

demography_ctable <- crosswalk_table_create(eb_demography_metadata) %>%
  mutate ( val_label_target = var_label_normalize(.data$val_label_orig) ) %>%
  mutate ( var_name_target = case_when (
    var_name_orig == "d11"      ~ "age", 
    var_name_orig == "d15a"     ~ "occupation", 
    var_name_orig == "d15a_r1"  ~ "occupation_rec_4", 
    var_name_orig == "d15a_r2"  ~ "occupation_rec_8", 
    var_name_orig == "d25"      ~ "type_community", 
    var_name_orig == "d7"       ~ "marital_status", 
    var_name_orig == "d8"       ~ "age_education", 
    var_name_orig == "isocntry" ~ "geo", 
    var_name_orig == "w1"       ~ "w1", 
    var_name_orig %in% c("wex", "wextra") ~ "wex", 
    var_name_orig == "rowid"    ~ "rowid",
    TRUE ~ "<error>"
  )) %>%
  mutate ( na_label_target = case_when (
    grepl("^DK", .data$val_label_orig)    ~ "declined", 
    grepl("^Refus", .data$val_label_orig) ~ "declined",
    TRUE ~ .data$na_label_target
  )) %>%
  mutate ( na_numeric_target = ifelse (!is.na(.data$na_label_target), 
                                       99998, NA_real_)) %>%
  mutate ( val_numeric_target = ifelse (.data$val_label_orig == "No full-time education", 14, .data$val_numeric_target), 
           val_numeric_target = ifelse (.data$val_label_orig == "Still studying", 99996, .data$val_numeric_target), 
           na_numeric_target  = ifelse (.data$val_label_orig == "Still studying", 99996, .data$val_numeric_target), 
           na_label_target    = ifelse (.data$val_label_orig == "Still studying", "still_studying", .data$na_label_target), 
           class_target = case_when (
             var_name_target %in% c("w1", "wex") ~ "numeric", 
             var_name_target == "rowid"          ~ "character",
             var_name_target == "geo"            ~ "factor",
             var_name_target == "age_education"  ~ "factor",
             TRUE ~ .data$class_target
           ))

demography  <- crosswalk (survey_list = eb_climate_waves, 
                              crosswalk_table = demography_ctable %>%
                                filter ( id != "ZA7488_v1-0-0"))

climate_dataset <- left_join ( global_problems, demography, by = c("id", "rowid") )

## Create a crosswalk table

crosswalk_global_problem <- eb_demography_metadata  %>%
  bind_rows (most_serious_global_problem) %>%
  mutate ( class_var = case_when ( 
    var_name_target %in%  c("rowid", "age", "w1", "wex") ~ "numeric", 
    TRUE ~ "factor")) %>%
  select ( all_of(c("filename", "class_var", "var_name_orig", "var_name_target"))) %>%
  pivot_wider ( names_from = "filename", 
                values_from = "var_name_orig") 

numeric_vars <- crosswalk_global_problem$var_name_target[crosswalk_global_problem$class_var == "numeric"]
factor_vars  <- crosswalk_global_problem$var_name_target[crosswalk_global_problem$class_var == "factor"]

crosswalk_table <- crosswalk_global_problem  %>%
  pivot_longer (cols = starts_with("ZA"), 
                names_to = "filename", 
                values_to = "var_name_orig")


crosswalk_surveys <- function(survey_list, crosswalk_table) {
  subset_survey <- function(x) {
    if ( attr(x, "filename") %in% unique(crosswalk_table$filename) ) {
      selection <- crosswalk_table %>% filter ( .data$filename == attr(x, "filename") )
      
      x %>% 
        select ( all_of(selection$var_name_orig) ) %>%
        set_names( selection$var_name_target ) %>%
        mutate ( filename = attr(x, "filename")) %>%
        relocate ( .data$filename, .before = everything())
      
    } else { NULL }
  }
  
  subsetted <- lapply ( eb_climate_waves, function(x) subset_survey(x) )
  
  do.call (rbind, subsetted)
  
}


attributes(eb_climate_waves[[1]])


examples_dir <- system.file("examples", package = "retroharmonize")
survey_list <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
example_surveys <- read_surveys(
   file.path( examples_dir, survey_list), 
   save_to_rds = FALSE)

documented_surveys <- create_surveys_metadata(example_surveys)

crosstable <- documented_surveys %>%
  filter (.data$var_name_orig %in% c("rowid", "w1", "isocntry", "d11") | 
          grepl( "freedom|solidarity", .data$label_orig )) %>%
  select ( all_of(c("filename", "var_name_orig", "label_orig"))) %>%
  mutate ( var_name_target = case_when ( 
    .data$var_name_orig == "isocntry"  ~ "country",
     grepl("solidarity", .data$label_orig) ~  "important_solidarity", 
     grepl("freedom", .data$label_orig) ~  "important_freedom", 
          TRUE ~ .data$var_name_orig
    ))



unique ( crosstable$var_name_orig )

crosstable <- data.frame (
  filename = c(
    c("ZA5913.rds", "ZA5913.rds", "ZA7576.rds", "ZA7576.rds", "ZA6863.rds", "ZA6863.rds"), 
    rep(c("ZA5913.rds", "ZA6863.rds", "ZA7576.rds"), 3 )
    ), 
  var_name_orig = c(
    c( "qd3_4", "qd3_8" , "qd7.4", "qd7.8", "qd6.4", "qd6.8"), 
    rep (c("rowid", "isocntry", "w1" ), 3)
    ),
  var_name_target = c(
    rep(c("important_freedom", "important_solidarity"),3 ),
    rep (c("rowid", "country", "w1" ), 3)
    )
)

crosstable$class_target <- ifelse(crosstable$var_name_orig == "w1", "numeric", "factor")
crosswalk ( example_surveys, crosswalk_table = crosstable)


retroharmonize::as_labelled_spss_survey()