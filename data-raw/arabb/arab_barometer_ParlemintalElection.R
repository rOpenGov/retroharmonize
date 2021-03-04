
# 1. the aim of this codes, to test Retroharmonize on other question such as "how would you evaluate the last parliamentary"?.. 
library(retroharmonize)
library(haven)
library(tidyverse) #dplyr, stringr, ggplot2 are part of the tidyverse, no need to load separatelly
library(devtools)

## When working with the development version, not the released, run this
devtools::load_all()

arabbarometer_dir <- file.path("not_included", "arabb", "input")
ab <- dir (arabbarometer_dir, pattern = "sav$")
arabbarometer_rounds <- file.path(arabbarometer_dir, ab)
arab_waves <- read_surveys(arabbarometer_rounds,.f='read_spss')
attr(arab_waves[[1]],"id") <- "Arab_Barometer_1"
attr(arab_waves[[2]],"id") <- "Arab_Barometer_2"
attr(arab_waves[[3]],"id") <- "Arab_Barometer_3"
attr(arab_waves[[4]],"id") <- "Arab_Barometer_4"
attr(arab_waves[[5]],"id") <- "Arab_Barometer_5"

documented_arab_waves <- document_waves(arab_waves)
save(documented_arab_waves, file = file.path("data-raw", "arabb", "Arab.Rda"))

#2 Designing a metadata & merged data set for survey's rounds 
arabb_metadata <- lapply ( X = arab_waves, FUN = metadata_create )
arabb_metadata <- do.call(rbind, arabb_metadata)
#3. picking the question from the loaded survey rounds data
to_harmonize <- arabb_metadata %>%
  filter(var_name_orig %in% c("rowid", "country","date", "wt")|
           grepl("how would you evaluate the last parliamentary", label_orig)) %>%
  mutate(var_label = var_label_normalize(label_orig)) %>%
  mutate(var_label = case_when(
    var_name_orig == "country" ~ "Country",
    var_name_orig == "rowid" ~ "Unique ID AB English", # in pdf Unique ID AB English
    var_name_orig == "date" ~ "Date_of_interview",
    var_name_orig == "wt" ~ "Weight",
    TRUE ~ " Evaluation in the last parliamentary elections")) %>%
  mutate ( var_name = var_label_normalize(var_label))

head(to_harmonize%>% 
       select ( all_of(c("id", "var_name", "var_label"))), 10)
merged_ab <- merge_waves(waves = arab_waves, var_harmonization = to_harmonize)
merged_ab <- lapply (merged_ab, 
                     FUN = function(x) x  %>%
                       mutate( country = as_character(country)))
#4. Both rounds 5 and 1 don't include the variable of our main question of interest, so we omit  the whole round datasets. 

merged_ab[[5]] <- NULL
merged_ab[[1]] <- NULL
documented_merged_ab <- document_waves(merged_ab)

# 5. "date of interview" is missed in both rounds 2 and 4, so we delete them from round 3. we obtain 3 rounds data sets with identical variables(unique id, country, weight, and our question variable)

#merged_ab[[2]][["date_of_interview"]]<-NULL

documented_merged_ab <- document_waves(merged_ab)
merged_arabb <- lapply(merged_ab, function(x){
  if ("date_of_interview" %in% names(x)){
    subset(x, select = -c(date_of_interview))
  } else{
    subset(x)
  }
})
paste(names(merged_arabb[[1]]), collapse = ", ")
R2 <-pull_survey(merged_ab,id = "Arab_Barometer_2")
attributes(R2$`evaluation in the last parliamentary elections`[1:20])
document_survey_item(R2$`evaluation in the last parliamentary elections`)
collect_val_labels(to_harmonize %>% 
                     filter(grepl("evaluation in the last parliamentary elections", var_name)))

#6. Perl is not supported in harmonize_waves function, so our suggestion to adjust to allow for Perl, that will make regex simpler, but her we used regex that exactly matches all our question  answer's labels
#7 we need to handle the NA values before harmonization (this is important ),as in function harmonize_arabb_trust could not distinguish NA if not labeled. so it is better to replace N/A immediately with the value of the "missing/or not applicable" before standardizing response answers's values. we could add label to N/A after adding the values but her not necessary. 
merged_arabb[[3]]$`evaluation in the last parliamentary elections`[is.na(merged_arabb[[3]]$`evaluation in the last parliamentary elections`)==T]<-99999 
merged_arabb[[1]]$`evaluation in the last parliamentary elections`[is.na(merged_arabb[[1]]$`evaluation in the last parliamentary elections`)==T]<-99999 
merged_arabb[[2]]$`evaluation in the last parliamentary elections`[is.na(merged_arabb[[2]]$`evaluation in the last parliamentary elections`)==T]<-99999 



harmonize_arabb_trust <- function(x){
  label_list <- list(
    from = c("(\\d\\.\\s)?(\\w+\\s\\w+\\s)?([c|C]ompletely free and fair)",
             "(.+)(but)?\\s?(with)\\s(some)?\\s{0,}(minor\\s\\w+)",
             "(.+)(but)?\\s?(with)\\s(some)?\\s{0,}(major\\s\\w+)",
             "(.+)?([n|N]ot\\sfree\\s\\w+\\s\\w+)",
             "((\\d.\\s{0,})?\\si\\s)?([d|D]on.t\\sknow)(\\s\\(Do\\snot\\sread\\))?", 
             "[R|r]efuse", 
             "(\\d.\\s)?[d|D]ecline[d]?(\\s\\w+\\s\\w+)(\\s.Do not read.)?",
             "(\\d.\\s)?[m|M]issing"),
    to = c("Free_and_fair", 
           "some_minor_problems",
           "some_major_problems",
           "not_free",
           "do_not_know","declined","declined","missing"),
    numeric_values = c(3,2,1,0,99997,99998,99998,99999))
  harmonize_values(x, harmonize_labels = label_list, 
                   na_values = c("do_not_know"= 99997,
                                 "declined"=99998,
                                 "missing"=99999
                   ))
  
}

set.seed(2020) # I dont'think this is needed unless you want to take a sample from the entire data file.
harmonized_arabb_waves <- harmonize_waves( 
  waves = merged_arabb, 
  .f = harmonize_arabb_trust )

#8 we check transformed variables 
count(harmonized_arabb_waves,`evaluation in the last parliamentary elections`)

#9 we start analyzing the harmonized data
class(harmonized_arabb_waves)
#10 .N/A values are not captured, so as per Daniel pdf's note, we should give attention to them, so we handled them in the next step after label transformation as missing.
attributes(harmonized_arabb_waves$`evaluation in the last parliamentary elections`)

# harmonized_arabb_waves$`evaluation in the last parliamentary elections`[(is.na(harmonized_arabb_waves$`evaluation in the last parliamentary elections`)==TRUE)&(is.character(
#   harmonized_arabb_waves$`evaluation in the last parliamentary elections`==TRUE)
# )] <- 99999
count(harmonized_arabb_waves,`evaluation in the last parliamentary elections`)



h_ab_structure <- attributes(harmonized_arabb_waves)
lapply(h_ab_structure, length)

h_ab_structure$row.names <- NULL 
h_ab_structure
#8 using str_split is not suitable as it extract the splited   ID code from first observation and past it on the the rest of rows, alternatively, we use str_extract that extract the ID code properly. 

harmonized_arabb_waves <- harmonized_arabb_waves %>%
  mutate(id = str_extract(harmonized_arabb_waves$`unique id ab english`, "(\\b[A-Z0-9]+)"))
# clean country names
table(harmonized_arabb_waves$country, useNA = 'always')
harmonized_arabb_waves$country <- gsub('\\d{1,}\\.\\s','',harmonized_arabb_waves$country, fixed = FALSE)
harmonized_arabb_waves$country <- str_squish(harmonized_arabb_waves$country) #remove tailing and leading speaces
table(harmonized_arabb_waves$country, useNA = 'always')

numeric_summary <- as_tibble(harmonized_arabb_waves) %>%
  mutate_at ( vars(starts_with("evaluation")), 
              ~as_numeric(.)*weight) %>%
  select ( -all_of("weight") ) %>%
  group_by ( country, id ) %>%
  summarize_if ( is.numeric, mean, na.rm=TRUE )

# numeric summarize does not make sense due to large na values * weight, better the categorical summary

library(stringr)
harmonized_arabb_waves <- harmonized_arabb_waves %>%
  mutate(id = str_extract(harmonized_arabb_waves$`unique id ab english`, "(\\b[A-Z0-9]+)"))

numeric_summary <- harmonized_arabb_waves %>%
  dplyr::mutate_at(vars(starts_with("evaluation")),
                   ~as_numeric(.)*weight)%>%
  select ( -all_of("weight") )%>%
  group_by ( country, id ) %>%
  summarize_if ( is.numeric, mean, na.rm=TRUE )

numeric_summary

## library(tidyr)  No need to load it, it is part of tidyverse.
categorical_summary <- harmonized_arabb_waves %>%
  select ( -all_of(c("weight", "unique id ab english")) ) %>%
  mutate ( 
    `evaluation in the last parliamentary elections` = 
      as_factor(`evaluation in the last parliamentary elections`)) %>%
  pivot_longer ( starts_with("evaluation"), 
                 names_to  = "indicator", 
                 values_to = "valuation") %>%
  group_by ( country, id, valuation ) %>%
  summarize (n = n()) 

View(categorical_summary)



