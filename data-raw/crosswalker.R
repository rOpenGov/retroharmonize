#crosswalk

devtools::load_all()
here::here()

## Not ready yet for working, a code element got lost. ---------------------------

library(dplyr, quietly = T)
library(tidyr)
library(stringr)
library(lubridate)
source("not_included/daniel_env.R")
library(dataobservatory)
eb <- dir ( gesis_dir )
eurobarometer_rounds <- file.path(gesis_dir, eb)

#Not run in the blogpost. In the repo we have a saved version.

dont_run <- function() {
  climate_change_files <- c("ZA5877_v2-0-0.sav", "ZA6595_v3-0-0.sav",  "ZA6861_v1-2-0.sav", 
                            "ZA7488_v1-0-0.sav", "ZA7572_v1-0-0.sav", "ZA7781_v1-0-0.sav")
  
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



collection_dates <- tibble (
  id = c("ZA5877_v2-0-0", "ZA6595_v3-0-0", "ZA6861_v1-2-0", "ZA7572_v1-0-0", "ZA7781_v1-0-0"), 
  begin = c(as.Date("2013-11-23"), as.Date("2015-05-30"), as.Date("2017-03-18"), as.Date("2019-04-09"), as.Date("2021-03-15")), 
  end = c(as.Date("2013-12-02"), as.Date("2015-06-08"), as.Date("2017-03-27"), as.Date("2019-04-26"), as.Date("2021-04-14")), 
  notional = c(as.Date("2013-12-01"), as.Date('2015-06-01'), as.Date("2017-04-01"), as.Date("2019-05-01"), as.Date("2021-04-01"))
) %>%
  mutate ( survey_year = as.factor(substr(as.character(.data$notional), 1,4))
  )

global_problem_ctable %>%
  select ( all_of(c("var_name_orig", "class_target", "var_name_target", "id")) ) %>%
  distinct_all() %>%
  pivot_wider ( names_from = "id", values_from  = "var_name_orig")



global_problems <- crosswalk (survey_list = eb_climate_waves, 
                              crosswalk_table = global_problem_ctable %>%
                                filter ( id != "ZA7488_v1-0-0"))



eb_demography_metadata  <- eb_climate_metadata %>%
  filter ( grepl( "rowid|isocntry|^d8$|^d7$|^wex|^w1$|d25|^d11$", .data$var_name_orig) ) %>%
  filter ( .data$filename %in% most_serious_global_problem$filename ) 

demography_ctable <- crosswalk_table_create(eb_demography_metadata) %>%
  mutate ( val_label_target = var_label_normalize(.data$val_label_orig) ) %>%
  mutate ( var_name_target = case_when (
    var_name_orig == "d11"      ~ "age", 
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
    grepl("studying", .data$val_label_orig) ~ "still_studying",
    grepl("not_clearly_documented", .data$val_label_target) ~ "not_clearly_documented",
    TRUE ~ .data$na_label_target
  )) %>%
  mutate ( na_numeric_target = case_when(
    .data$na_label_target == "declined" ~ 99998, 
    .data$na_label_target == "not_clearly_documented" ~ 99991,
    .data$na_label_target == "still_studying" ~ 99992, 
    TRUE ~ NA_real_
  )) %>%
  mutate ( val_numeric_target = case_when (
    .data$na_numeric_target >= 99990 ~ .data$na_numeric_target,
    grepl("No full-time", .data$val_label_orig) ~ 14,
    TRUE ~ .data$val_numeric_target
  )) %>%
  mutate ( val_label_target = case_when (
    grepl("years|older", .data$val_label_orig) & any(.data$var_name_target %in% c("age", "age_education")) ~ as.character(.data$val_numeric_target),
    TRUE ~ .data$val_label_target
  )) %>%
  mutate ( val_label_target = case_when(
    .data$val_label_orig == "No full-time education" ~ "no_full_time_education", 
    .data$val_label_target == 'small_or_middle_sized_town' ~ 'small_middle_town',
    TRUE ~ .data$val_label_target
  )) %>%
  mutate ( val_numeric_target = case_when (
    grepl("time_education", .data$val_label_target) ~ 14,
    TRUE ~ .data$val_numeric_target
  )) %>%
  mutate ( class_target = case_when (
    var_name_target %in% c("w1", "wex") ~ "numeric", 
    var_name_target == "rowid"          ~ "character",
    var_name_target == "geo"            ~ "factor",
    var_name_target %in%  c("age", "age_education")  ~ "numeric",
    TRUE ~ .data$class_target )
    )

demography  <- crosswalk (survey_list = eb_climate_waves, 
                          crosswalk_table = demography_ctable %>%
                            filter ( id != "ZA7488_v1-0-0")
)

climate_dataset <- left_join ( global_problems, demography, by = c("id", "rowid") )

marital_status_vector <- unique (climate_dataset$marital_status)

no_children <- as.character(marital_status_vector[grepl("without_children",marital_status_vector )])
no_child_status <- as.character(marital_status_vector[grepl("^refus|^other", marital_status_vector)])
has_children <- as.character(marital_status_vector[! marital_status_vector %in% c(no_child_status, no_children)])

unique (climate_dataset$global_problem)

climate2 <- climate_dataset %>% 
  mutate ( has_child = case_when(
    .data$marital_status %in% has_children ~ 1, 
    .data$marital_status %in% no_children ~ 0,
    .data$marital_status %in% no_child_status ~ NA_real_,
    TRUE ~ 9999
  )) %>%
  mutate ( age_education = as.character(age_education)) %>%
  mutate ( still_studying = case_when (
    .data$age_education == "still_studying" ~ 1, 
    .data$age_education %in% c("refusal", "declined", "dk", "99996", "not_clearly_documented") ~ NA_real_,
    TRUE ~ 0
  )) %>%
  mutate ( age_education = case_when(
    .data$age_education %in% c("refusal", "declined", "dk", "99996") ~ NA_character_,
    .data$age_education == "still_studying" ~ as.character(.data$age),
    .data$age_education == "no_full_time_education" ~ as.character(.data$age),
    TRUE ~ .data$age_education )) %>%
  mutate (age_education = case_when(
    .data$age_education == "not_clearly_documented" ~ NA_character_,
    TRUE ~ gsub("_years", "", .data$age_education)
  )) %>%
  mutate ( age_education = as.numeric(age_education) ) %>%
  mutate ( infectious_diseases = case_when(
    .data$global_problem == "spread_of_infectious_diseases" ~ 1, 
    .data$global_problem %in% c("declined") ~ NA_real_,
    TRUE ~ 0
  )) %>% 
  mutate ( economy = case_when(
    .data$global_problem == "the_economic_situation" ~ 1, 
    .data$global_problem %in% c("declined") ~ NA_real_,
    TRUE ~ 0
  )) %>%
  mutate ( climate_change = case_when(
    .data$global_problem == "climate_change" ~ 1, 
    .data$global_problem %in% c("declined") ~ NA_real_,
    TRUE ~ 0
  )) %>% mutate ( age = case_when(
    grepl("years|older", .data$age) ~ as.character(gsub('\\D+','', .data$age)), 
    grepl("clearly_documented", .data$age) ~ NA_character_,
    TRUE ~ as.character(.data$age)
  )) %>%
  mutate ( age = as.numeric(.data$age)) %>%
  mutate ( country_group = case_when(
    geo %in% c("HU", "PL", "SK", "CZ") ~ "Visegrad", 
    geo %in% c("BE", "NL", "LU") ~ "Benelux",
    geo %in% c("DE", "DE-W", "DE-E", "CH", "AT", "SI" ) ~ "Central",
    geo %in% c("CY", "GR", "TR", "BG", "XK", "MK", "RO", "RS", "BA", "MO", "RS", "HR") ~ "Southeast",
    geo %in% c("FI", "SE", "NO", "DK", "FI", "IS") ~ "Nordic", 
    geo %in% c("LV", "LT", "EE") ~ "Baltic", 
    geo %in% c("PT", "ES", "MT", "IT") ~ "Southwest", 
    TRUE ~ "West"
  )) %>%
  mutate ( country_code = substr(geo, 1,2))

climate3 <- climate2 %>%
  mutate ( age_education_rec = case_when(
    age_education > 21 ~ "higher", 
    age_education > 19 ~ "tertiary",
    age_education < 18 & age > 21 ~ "no_formal_education", 
    TRUE ~ "secondary"
  )) %>%
  mutate ( age_rec = case_when (
    .data$age < 25 ~ "[15-24]", 
    .data$age < 45 ~ "[25-44]", 
    .data$age < 65 ~ "[45-64]", 
    TRUE ~ "[65-99]"
  ) ) %>%
  mutate( age_education_rec = as.factor(.data$age_education_rec), 
          age_rec = as.factor(.data$age_rec)) %>%
  left_join (collection_dates, by = 'id') %>%
  mutate ( survey_year = as.factor(.data$survey_year))



summary ( test )

model <- glm ( climate_change ~ country_group + age_education_rec + age_rec + type_community + survey_year+ has_child -1, family = binomial(), data = climate3)
summary(model)

modeld <- glm ( infectious_diseases ~ country_group + age_education_rec + age_rec + type_community + survey_year+ has_child -1, family = binomial(), data = climate3)
summary(modeld)
names(climate3)

library("iml")
library("randomForest")
library("tidyverse")

fn_three_values <- function(x) {
  x <- case_when ( x == 1 ~ "mentioned", 
                                           x == 0 ~ "not_mentioned",
                                           TRUE ~ NA_character_ )
  as.factor(x)}

climate_complete <- climate3 %>%
  select (-any_of(c("climate_complete", "begin", "end", "notional")))%>%
  mutate ( across(any_of(c("climate_complete", "has_child", "economy", "infectious_disease", "survey_year")),as.factor)) %>%
  mutate ( across(any_of(c("climate_complete", "has_child", "econoy", "infectious_disease")), fn_three_values)) %>%
  filter ( complete.cases(.) )


names (climate_complete)
X %>% climate_complete 
library(rpart)

model <- rpart(climate_change ~ country_group + age_education_rec + age_rec + type_community + survey_year+ has_child, data = climate_complete, method = "class")
par(xpd = NA) # otherwise on some devices the text is clipped
plot(model)
text(model, digits = 3)
library(caret)
set.seed(123)
model2 <- train(
  climate_change ~ country_group + age_education_rec + age_rec + type_community + survey_year+ has_child, 
  data = climate_complete, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(model2$finalModel)
text(model2$finalModel,  digits = 3)

set.seed(123)
model3 <- train(
  climate_change ~ age_education_rec + age_rec + type_community + survey_year+ has_child, 
  data = climate_complete, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(model3$finalModel)
text(model3$finalModel,  digits = 3)
library(rpart.plot)
rpart.plot(model3$finalModel, type = 4, 
           extra = 100, fallen.leaves = FALSE, yesno = 2, tweak = 1.7, 
           box.palette=c("#007CBB", "#4EC0E4", "#FAE000", "#E88500" ,"#DB001C"  ))

srvisual::sr_palette()
library(tidyverse)

rf <- randomForest(climate_change ~ country_group + age_education_rec + age_rec + type_community + survey_year+ has_child, data = climate_complete, ntree = 50)

X =climate_complete %>%
  dplyr::select ( -any_of(c("w1", "wex", "rowid", "global_problem", "id")) )
predictor <- Predictor$new(rf, data = X, y = climate_complete$climate_change)
imp <- FeatureImp$new(predictor, loss = "mae")
plot(imp)

shapley <- Shapley$new(predictor, x.interest = X[1, ])
shapley$plot()

wex_sum_country <- climate3 %>%
  select ( country_code, w1, notional ) %>% 
  rename ( time = notional ) %>%
  group_by ( country_code, time ) %>% 
  summarise ( wex_sum = sum(w1, na.rm=TRUE)) %>%
  ungroup()

wex_sum_geo <- climate3 %>%
  select ( geo, w1, notional ) %>% 
  rename ( time = notional ) %>%
  group_by ( geo, time ) %>% 
  summarise ( wex_sum_geo = sum(w1, na.rm=TRUE)) %>%
  ungroup()

climate3 %>% filter ( .data$survey_year == '2021')

df = climate3
create_national_indicator <- function (df, var_name = "climate_change", 
                                       dataset_code = NA_character_,
                                       freq = "A2", 
                                       unit = "PC_IND", 
                                       unit_name = "Percent of Individuals") {
  
  wex_sum_geo <- df %>%
    select ( all_of(c("geo", "w1", "notional")) ) %>% 
    rename ( time = notional ) %>%
    group_by ( .data$geo, .data$time ) %>% 
    summarise ( wex_sum_geo = sum(w1, na.rm=TRUE), .groups = 'keep') %>%
    ungroup()
  
  wex_sum_country <- df %>%
    select ( all_of(c("geo", "w1", "notional"))) %>% 
    mutate ( country_code = substr(geo, 1,2)) %>%
    rename ( time = .data$notional ) %>%
    group_by ( .data$country_code, .data$time ) %>% 
    summarise ( wex_sum = sum(w1, na.rm=TRUE), .groups = 'keep' ) %>%
    ungroup()
  
  df_geo <- df %>%
    select ( all_of (c("rowid", "notional", "geo", "w1", var_name))) %>%
    rlang::set_names(c("rowid", "time", "geo", "w1", "var")) %>%
    mutate ( value = .data$var*.data$w1 ) %>%
    group_by ( .data$time, .data$geo ) %>%
    summarize ( value = sum(.data$value, na.rm=TRUE ), .groups = "keep") %>%
    ungroup() %>% 
    left_join ( wex_sum_geo, by = c('geo', 'time') ) %>%
    mutate ( value = .data$value / .data$wex_sum_geo ) %>%
    select (-any_of("wex_sum_geo"))
  
  df_country <- df %>%
    select ( all_of (c("rowid", "notional", "geo", "w1", var_name ))) %>%
    rlang::set_names(c("rowid", "time", "geo", "w1", "var")) %>%
    mutate ( country_code = substr(geo, 1,2)) %>%
    filter (.data$country_code %in% c("DE", "GB")) %>%
    mutate ( value = .data$var*w1 ) %>%
    group_by ( .data$time, .data$country_code ) %>%
    summarize ( value = sum(.data$value, na.rm=TRUE ), .groups = 'keep') %>%
    ungroup() %>% 
    left_join ( wex_sum_country, by = c("time", "country_code")) %>%
    mutate ( value = .data$value / .data$wex_sum ) %>%
    ungroup() %>%
    rename ( geo = .data$country_code ) %>%
    select ( -any_of("wex_sum"))
  
  bind_rows ( df_geo, df_country ) %>%
    arrange (  across(all_of(c("time", "geo")))) %>%
    mutate ( obs_status = case_when ( 
      is.na(.data$value) ~ "O", 
      TRUE ~ "A"), 
      method = .data$obs_status, 
      freq = freq, 
      unit = unit, unit_name = unit_name, 
      dataset_code = dataset_code )
}

names (climate3)

create_national_indicator(climate3, "infectious_diseases", dataset_code = "global_problem_1_infectous_eb")


climate_df <- create_national_indicator(climate3, "climate_change", 
                                        dataset_code = "global_problem_1_climate_change_eb") %>%
  dataset ( climate_df, dataset_code = "global_problem_1_climate_change_eb", 
          freq = "A2", unit = "PCT_IND", unit_name = "Percent of individuals", 
          dataset_title = "Individuals Who Think Climate Change is the Most Serious World Problem, %" )

write.csv(climate_df, file.path("not_included", "global_problem_1_climate_change_eb.csv"), row.names=F)  


economy_df <- create_national_indicator(climate3, "economy",
                                        dataset_code = "global_problem_1_economy_eb") %>%
  dataset ( climate_df, dataset_code = "global_problem_1_economy_eb", 
            freq = "A2", unit = "PCT_IND", unit_name = "Percent of individuals", 
            dataset_title = "Individuals Who Think That the Economic Situation is the Most Serious World Problem, %" )


economy_df <- create_national_indicator(climate3, "economy",
                                        dataset_code = "global_problem_1_economy_eb") %>%
  dataset ( climate_df, dataset_code = "global_problem_1_economy_eb", 
            freq = "A2", unit = "PCT_IND", unit_name = "Percent of individuals", 
            dataset_title = "Individuals Who Think That the Economic Situation is the Most Serious World Problem, %" )


library(dataobservatory)

climate_df <- climate_geo %>% 
  full_join ( climate_country ) %>%
  mutate ( dataset_code = "global_problem_1_climate_change_eb", 
           unit = "PCT_IND", 
           unit_name = "Percent of individuals", 
           obs_code =  "A", 
           method = "A", 
           freq =  "A2") 
dataset ( climate_df, dataset_code = "global_problem_1_climate_change_eb", 
          freq = "A2", unit = "PCT_IND", unit_name = "Percent of individuals", 
          dataset_title = "Individuals Who Think Climate Change is the Most Serious World Problem, %" )


climate_df <- climate_geo %>% 
  full_join ( climate_country ) %>%
  mutate ( dataset_code = "global_problem_1_climate_change_eb", 
           unit = "PCT_IND", 
           unit_name = "Percent of individuals", 
           obs_code =  "A", 
           method = "A", 
           freq =  "A2") 
dataset ( climate_df, dataset_code = "global_problem_1_climate_change_eb", 
          freq = "A2", unit = "PCT_IND", unit_name = "Percent of individuals", 
          dataset_title = "Individuals Who Think Climate Change is the Most Serious World Problem, %" )

write.csv(climate_df, file.path("not_included", "global_problem_1_climate_change_eb.csv"), row.names=F)  

unique ( climate_df$time)
dataobservatory::cl_freq()

test %>% mutate ( country_code = regions::get_country_code(.data$geo))

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


demography_ctable_2 <- eb_climate_metadata %>%
  filter ( grepl( "rowid|isocntry|^d8$|^d7$|^wex|^w1$|d25|^d15a|^d11$", .data$var_name_orig) ) %>%
  filter ( .data$filename %in% most_serious_global_problem$filename ) %>%
  crosswalk_table_create(eb_demography_metadata)  %>%
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
