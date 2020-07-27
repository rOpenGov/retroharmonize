save (ab_metadata, to_harmonize, merged_ab, file = 
        file.path("inst", "examples", "afrob_vignette.rda") )

load(system.file( file.path("examples", "afrob_vignette.rda"),
             package = "retroharmonize"))

documented_eb_waves <- document_waves(eb_waves)
test_trust <- pull_survey(eb_waves, filename = "ZA4414_trust.rds")

save ( eb_trust_metadata, test_trust, harmonized_eb_waves, documented_waves, 
       file = file.path("inst", "examples", "euroob_vignette.rda" ))
      
eb_plot <- numeric_harmonization %>%
  tidyr::pivot_longer( cols = contains("trust")) %>%
  filter ( !is.na(value))
names ( eb_plot )

ggplot (data = eb_plot, 
        aes ( 
          x = name,
        y = value , 
        color = name) ) +
  geom_point() +
  facet_wrap ( facets = "country")


names( numeric_harmonization)
library(ggplot2)

ab_plot <- harmonized_ab_waves %>%
  mutate_at ( vars(starts_with("trust")), 
              ~as_numeric(.)*within_country_weighting_factor) %>%
  select ( -all_of("within_country_weighting_factor") ) %>%
  group_by ( country, year ) %>%
  summarize_if ( is.numeric, mean, na.rm=TRUE ) %>%
  select ( all_of ( c("country", "year")), 
           contains("parliament"), 
           contains("president"), 
           contains("religious"),
           contains("traditional")) %>%
  pivot_longer ( cols = contains("trust")) %>%
  filter ( !is.nan(value)) %>%
  mutate ( name = case_when (
    grepl("president", name ) ~ "president", 
    grepl("parliament", name) ~ "parliament",
    grepl("religious", name) ~ "religious leaders",
    grepl("traditional", name ) ~ "traditional leaders", 
    TRUE ~ name 
  )) 

ab_plot_ordering <- ab_plot %>%
  select ( -all_of("name")) %>%
  group_by ( country ) %>%
  summarize ( avg_trust_level = mean(value)) %>%
  ungroup()

ab_plot2<- ab_plot %>%
  left_join ( ab_plot_ordering , by = 'country') %>%
  mutate ( country_code = countrycode::countrycode(
    country, "country.name", "iso2c"))  %>%
  ungroup()
names ( ab_plot2)

ab_plot_ordered  <- ab_plot2 %>%
  mutate ( country_code = forcats::fct_reorder(
    country_code, avg_trust_level), 
    country = forcats::fct_reorder(
      country, avg_trust_level)) %>%
  mutate ( year = as.factor(year))

levels ( ab_plot_ordered$country_code)


ab_plot1 <- ggplot ( data = ab_plot_ordered,
         aes ( x = year, y = value, 
               color = name)) +
  scale_y_continuous( breaks = c(0,1,2,3), 
                      n.breaks = 4,
                      labels = c("not", "little", "somewhat", "a lot")) +
  geom_point () + 
  scale_color_manual( values = c("#BAC615", "#4EC0E4", 
                                 "#E88500" , "#5C2320")) +
  facet_wrap ( facets = "country")+
  theme ( axis.text.x = element_text ( angle = 90), 
          legend.position = "bottom") +
  labs ( y = "trust level", x = "", color = "", 
         title = "Trust In African Leadership 2011-2018", 
         subtitle = "Based on Afrobarometer Rounds 5-7", 
         caption = "http://doi.org/10.5281/zenodo.3937746, R package website: retroharmonize.satellitereport.com \ua9 2020.")


ggsave(filename = file.path ("vignettes", "ab_plot1.png"), 
       plot = ab_plot1,
       units = "cm", width = 18, height = 13, dpi = 350)

here ("vignette")

file.exists(file.path ("vignette", "ab_plot1.png"))
