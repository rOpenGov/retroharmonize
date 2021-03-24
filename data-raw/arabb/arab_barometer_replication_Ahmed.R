#require("devtools")
#devtools::install_github("antaldaniel/retroharmonize", force = TRUE)
#install.packages('retroharmonize', dependencies = T).
library(retroharmonize)
library(dplyr)
library(tidyr)
library(haven)
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
save(documented_arab_waves, file = file.path("not_included", "arabb", "Arab.Rda"))
# Metadata 
arabb_metadata <- lapply ( X = arab_waves, FUN = metadata_create )
arabb_metadata <- do.call(rbind, arabb_metadata)
# alternative using dplyr bind_rows more efficient
#x <- bind_rows(arabb_metadata)
#**
# 1586 obs compared to 1587 in pdf!! this because the downloaded data has even diff Id.name from pdf"ABV_Release_Data.sav" compared to AB-WaveV-EN.sav of round5
#                                      **
to_harmonize <- arabb_metadata %>%
  filter(var_name_orig %in% c("rowid", "country","date", "wt")|
           grepl("current economic|overall economic", label_orig)) %>%
  mutate(var_label = var_label_normalize(label_orig)) %>%
  mutate(var_label = case_when(
    var_name_orig == "country" ~ "Country",
    var_name_orig == "rowid" ~ "Unique ID AB English", # in pdf Unique ID AB English
    var_name_orig =="date" ~ "Date_of_interview",
    var_name_orig =="wt" ~ "Weight",
    TRUE ~ " Evaluation economic situation")) %>% # in pdf Evaluation economic situation
  mutate ( var_name = var_label_normalize(var_label))

#library(sjlabelled) # to get labels
#get_labels(merged_ab[[5]][["country"]])

head(to_harmonize%>% 
       select ( all_of(c("id", "var_name", "var_label"))), 10)

# Merging 
merged_ab <- merge_waves(waves = arab_waves, var_harmonization = to_harmonize)
merged_ab <- lapply (merged_ab, 
                      FUN = function(x) x  %>%
                        mutate( country = as_character(country)))


documented_merged_ab <- document_waves(merged_ab)

print(documented_merged_ab)
merged_ab[[1]] <- NULL

merged_arabb <- lapply(merged_ab, function(x){
  if ("date_of_interview" %in% names(x)){
    subset(x, select = -c(date_of_interview))
  } else{
    subset(x)
  }
})
documented_merged_arabb <- document_waves(merged_arabb)
paste(names(merged_arabb[[1]]), collapse = ", ")
# now we have similar number of variables for each survey
#paste(names(merged_arabb[[1]]), collapse = ",")
## Harmonization 
# check labels of economic situation ( more complex that afrobarometers)
#collect_val_labels(to_harmonize %>% 
#                     filter(grepl("evaluation economic situation", var_name)))

R3 <-pull_survey(merged_ab,id = "Arab_Barometer_3")
attributes(R3$`evaluation economic situation`[1:20])
document_survey_item(R3$`evaluation economic situation`)
collect_val_labels(to_harmonize %>% 
                     filter(grepl("evaluation economic situation", var_name)))
                     # we try here to write regex experession that match all labess of evaluate_economic_situation but after transforming letters to lowercase.

# regex_table <- list('\\d{1,}.\\s{0,})?very\\s{1,}good','(\\d{1,}\\.\\s{0,}?)?(?<!very\\s)good','([0-9]+(\\.|\\s)\\s{1,})?(?<!very\\s)bad','(\\d{1,}\\.\\s{0,}?)?very\\sbad',"(\\d{1,}\\.\\s{0,}?\\s?)?(i\\s)?([a-zA-z]+.t)\\s{0,}know(\\s)?(.Do not read.)?",'(^([0-9]{1,}\\.)?\\s{0,}?)?(([dD]ecline(d)?))(\\s{0,2}to\\s{0,}answer\\s{0,})?(\\(\\D+)?','not responsible','not\s{1,}clear')

harmonize_arabb_trust <- function(x){
  label_list <- list(
    from = c("(\\d{1,}.\\s{0,})?very\\s{1,}good",
             "^(\\d{0,})?\\W?\\s{0,}good\\b",
             "^(\\d{0,})?\\W?\\s{0,}good\\b",
             "(\\d{1,}.\\s{0,})?very\\s{1,}bad",
             "t\\sknow", 
             "refuse", 
             " decline",
             "missing"),
    to = c("very_good", "good", "bad", "very_bad", "do_not_know","declined","declined","missing"),
    numeric_values = c(3,2,1,0,99997,99998,99998,99999))
  harmonize_values(x, harmonize_labels = label_list, 
                   na_values = c("do_not_know"= 99997,
                                 "declined"=99998,
                                 "missing"=99999))
}


set.seed(2020)
harmonize_arabb_trust(
  sample(R3$`evaluation economic situation`, 12)
)


harmonized_arabb_waves <- harmonize_waves ( 
  waves = merged_arabb, 
  .f = harmonize_arabb_trust )

h_arabb_structure <- attributes(harmonized_arabb_waves)

h_arabb_structure$row.names <- NULL # We have over 70K row names
h_arabb_structure
# we have a problem here, strsplit extract first split _ and copy paste to all cells as ABII while this should be independent based on each Unique ID strings, we need to try regext

#harmonized_arabb_waves <- harmonized_arabb_waves %>%
  
#  mutate(id = strsplit(`unique id ab english`, "[_]")[[1]][2])

#this is one of the alternative
library(stringr)

harmonized_arabb_waves <- harmonized_arabb_waves %>%
  mutate(id = str_extract(harmonized_arabb_waves$`unique id ab english`, "(\\b[A-Z0-9]+)"))


numeric_summary <- harmonized_arabb_waves %>%
  mutate_at(vars(starts_with("evaluation")),
            ~as_numeric(.)*weight)%>%
  select ( -all_of("weight") )%>%
  group_by ( country, id ) %>%
  summarize_if ( is.numeric, mean, na.rm=TRUE )



library(tidyr)  ## tidyr::pivot_longer()
categorical_summary <- harmonized_arabb_waves %>%
  select ( -all_of(c("weight", "unique id ab english")) ) %>%
  mutate ( 
    `evaluation economic situation` = 
      as_factor(`evaluation economic situation`)) %>%
  pivot_longer ( starts_with("evaluation"), 
                 names_to  = "indicator", 
                 values_to = "valuation") %>%
  group_by ( country, id, valuation ) %>%
  summarize ( n = n() ) 
categorical_summary


haven::write_sav(data = harmonized_arabb_waves, "harmonized_arabb_waves.sav")
write.csv(harmonized_arabb_waves, "harmonized_arabb_waves.csv")
write.csv(categorical_summary, "categorical_summary.csv")
write.csv(numeric_summary, "numeric_summary.csv")

# The metadata file contains list objects, which cannot be represented
# in a flat csv file format.
#saveRDS(arabb_metadata, "arabb_metadata.rds")

## The lists of value labels are dropped from the csv output.
#write.csv(arabb_metadata [, -which (sapply ( arabb_metadata, class) == "list")], "arabb_metadata_simplified.csv")
##
# PERL in harmonize_values is not an option, it has to be added to the backend function
                                                                                      ##
# set.seed(2020)
# harmonized_arabb_trust(
#   sample(R3$evaluate_economic_situation,12)
# )
# #with PERL
# z<-collect_val_labels(to_harmonize %>% 
#                         filter(grepl("evaluate_economic_situation", var_name)))
# str_extract_all("very good, 4.very good","(\\d{1,}.\\s{0,})?very\\sgood")
# str_extract_all("good,1.  good,1. good","(\\d{1,}\\.\\s{0,}?)?(?<!very\\s)good")
# str_extract_all("1 bad,1. bad,","([0-9]+(\\.|\\s)\\s{1,})?(?<!very\\s)bad")
# str_extract_all("9.very bad, very bad, very bad","(\\d{1,}\\.\\s{0,}?)?very\\sbad")
# str_extract_all("don't know,Don't know,2.Don't know,Don't know (Do not read)","(\\d{1,}\\.\\s{0,}?\\s?)?(i\\s)?([a-zA-z]+.t)\\s{0,}know(\\s)?(.Do not read.)?")
# str_extract_all("1.declined, declined, Decline to answer (Do not read)
# ,9. declined to answer","(^([0-9]{1,}\\.)?\\s{0,}?)?(([dD]ecline(d)?))(\\s{0,2}to\\s{0,}answer\\s{0,})?(\\(\\D+)?")


#(can't choose.)?(\d{1,}\.\s{0,}?\s?)?(i\s)?([a-zA-z]+.t)\s{0,}know(\s)?(.Do not read.)?


                          