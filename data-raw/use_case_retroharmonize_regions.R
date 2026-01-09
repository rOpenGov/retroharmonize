devtools::load_all()
here::here()

## Not ready yet for working, a code element got lost. ---------------------------

library(dplyr, quietly = T)
library(tidyr)
library(stringr)
library(lubridate)
source("not_included/daniel_env.R")
eb <- dir(gesis_dir)
eurobarometer_rounds <- file.path(gesis_dir, eb)

# Not run in the blogpost. In the repo we have a saved version.

dont_run <- function() {
  climate_change_files <- c(
    "ZA5877_v2-0-0.sav", "ZA6595_v3-0-0.sav", "ZA6861_v1-2-0.sav",
    "ZA7488_v1-0-0.sav", "ZA7572_v1-0-0.sav"
  )

  eb_climate_waves <- read_surveys(file.path(gesis_dir, climate_change_files), .f = "read_spss")

  if (dir.exists("data-raw")) {
    save(eb_climate_waves,
      file = file.path("data-raw", "eb_climate_change_waves.rda")
    )
  }
}

if (file.exists(here::here("data-raw", "eb_climate_change_waves.rda"))) {
  load(here::here("data-raw", "eb_climate_change_waves.rda"))
}

eb_climate_metadata <- metadata_surveys_create(eb_climate_waves)

eb_climate_metadata

serious_world_problems_first_metadata <- eb_climate_metadata %>%
  mutate(var_name_suggested = var_label_normalize(.data$label_orig)) %>%
  filter(var_name_suggested == "serious_world_problems_first") %>%
  mutate(var_name_suggested = ifelse(.data$var_name_orig %in% c("rowid", "wex", "w1"),
    .data$var_name_orig,
    .data$var_name_suggested
  ))

serious_problems_dataset <- subset_surveys(
  survey_list = eb_climate_waves,
  subset_names = unique(serious_world_problems_first_metadata$var_name_orig)
)

crosswalk_table <- create_crosswalk_table(metadata = serious_world_problems_first_metadata) %>%
  mutate(var_name_target = "most_serious_problem") %>%
  mutate(val_label_target = case_when(
    grepl("^other", tolower(.data$val_label_target)) ~ "other",
    grepl("^none", tolower(.data$val_label_target)) ~ "none",
    grepl("^dk", tolower(.data$val_label_target)) ~ "do_not_know",
    TRUE ~ val_label_normalize(.data$val_label_target)
  )) %>%
  mutate(var_numeric_target = case_when(
    .data$val_label_target == "do_not_know" ~ 99997,
    .data$val_label_target == "inap" ~ 99999
  )) %>%
  mutate(var_class = "factor")

survey_list <- eb_climate_waves

lapply(serious_world_problems_first_metadata$labels, names)
lapply(serious_world_problems_first$filename, cat)

fn_labels <- function(x) {
  val_labels <- names(x$labels)
  label_length <- length(unlist(x$labels))

  tibble(
    id = rep(unique(x$id), label_length),
    filename = rep(unique(x$filename), label_length),
    var_name_orig = rep(x$var_name_orig, label_length),
    var_name_target = rep(x$var_name_orig, label_length),
    val_label_orig = as.character(vapply(x$labels, names, character(label_length))),
    val_label_targett = as.character(vapply(x$labels, names, character(label_length)))
  )
}

seq_len

seq_len(serious_world_problems_first_metadata)
seq_along(serious_world_problems_first_metadata) ==
  x <- serious_world_problems_first_metadata[1, ]

apply(serious_world_problems_first, 1, function(x) fn_labels(serious_world_problems_first))
apply(serious_world_problems_first, 1, function(x) names(x$labels))


crosswalk_table <- eb_climate_metadata %>%
  harmonize_eb_trust() <- function(x) {
  label_list <- list(
    from = c(
      "^tend\\snot", "^cannot", "^tend\\sto", "^can\\srely",
      "^dk", "^inap", "na"
    ),
    to = c(
      "not_trust", "not_trust", "trust", "trust",
      "do_not_know", "inap", "inap"
    ),
    numeric_values = c(0, 0, 1, 1, 99997, 99999, 99999)
  )

  harmonize_values(x,
    harmonize_labels = label_list,
    na_values = c(
      "do_not_know" = 99997,
      "declined" = 99998,
      "inap" = 99999
    )
  )
}


eb_demography_metadata <- eb_climate_metadata %>%
  filter(grepl("rowid|isocntry|^d8$|^d7$|^wex|^w1$|d25|^d15a|^d11$", .data$var_name_orig)) %>%
  mutate(var_name_suggested = var_label_normalize(.data$label_orig)) %>%
  mutate(var_name_suggested = ifelse(.data$var_name_orig %in% c("rowid", "wex", "w1"),
    .data$var_name_orig,
    .data$var_name_suggested
  )) %>%
  filter(id %in% serious_world_problems_first_metadata$id)


eb_regional_metadata <- eb_climate_metadata %>%
  mutate(var_name_suggested = var_label_normalize(.data$label_orig)) %>%
  filter(grepl("rowid|isocntry|p7", .data$var_name_orig)) %>%
  mutate(var_name_suggested = ifelse(.data$var_name_orig %in% c("rowid", "wex", "w1"),
    .data$var_name_orig,
    .data$var_name_suggested
  )) %>%
  filter(id %in% serious_world_problems_first_metadata$id)


characters <- geography_chars %>%
  separate(
    data = .,
    col = .data$rowid,
    into = c("survey_id", "version", "unique_id"),
    sep = "_"
  ) %>%
  select(-all_of("unique_id")) %>%
  distinct_all() %>%
  pivot_longer(
    cols = -all_of(c("survey_id", "version", "isocntry", "region_nuts_codes")),
    names_to = "variable_name",
    values_to = "character_value"
  ) %>%
  filter(!grepl("^Inap|99", character_value)) %>%
  filter(!is.na(character_value))

labels <- geography_labels %>%
  separate(
    data = ., col = rowid,
    into = c("survey_id", "version", "unique_id"),
    sep = "_"
  ) %>%
  select(-all_of("unique_id")) %>%
  distinct_all() %>%
  pivot_longer(
    cols = -all_of(c("survey_id", "version", "isocntry", "region_nuts_codes")),
    names_to = "variable_name",
    values_to = "value_label"
  ) %>%
  filter(!is.na(value_label)) %>%
  filter(!grepl("^Inap", value_label)) %>%
  rename(region_nuts_names = region_nuts_codes)

coding_information <- characters %>%
  full_join(labels,
    by = c("survey_id", "version", "isocntry", "variable_name")
  ) %>%
  mutate(nuts3_code = case_when(
    nchar(region_nuts_codes) == 5 ~ region_nuts_codes,
    isocntry == "LU" ~ "LU000",
    TRUE ~ NA_character_
  )) %>%
  mutate(nuts2_code = case_when(
    nchar(region_nuts_codes) == 4 ~ region_nuts_codes,
    .data$isocntry == "LU" ~ "LU00",
    .data$isocntry == "CY" ~ "CY00",
    .data$isocntry == "MT" ~ "MT00",
    TRUE ~ NA_character_
  )) %>%
  mutate(nuts1_code = case_when(
    nchar(region_nuts_codes) == 3 ~ region_nuts_codes,
    isocntry == "LU" ~ "LU0",
    isocntry == "CY" ~ "CY0",
    isocntry == "MT" ~ "MT0",
    TRUE ~ NA_character_
  ))

cy_metadata <- coding_information %>% filter(isocntry == "CY")

mt_metadata <- coding_information %>% filter(isocntry == "MT")
