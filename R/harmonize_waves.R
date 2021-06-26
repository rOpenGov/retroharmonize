#' @title Harmonize waves
#' 
#' @description Harmonize the values of surveys. 
#' 
#' @details The functions binds together variables
#' that are all present in the surveys, and applies a 
#' harmonization function \code{.f} on them. 
#' 
#' @param waves A list of surveys
#' @param .f A function to apply for the harmonization.
#' @param status_message Defaults to \code{FALSE}. If set to \code{TRUE}
#' it shows the id of the survey that is being joined.
#' @return A natural full join of all surveys in a single data frame.
#' @export
#' @importFrom dplyr select bind_cols mutate_all pull
#' @importFrom tidyselect all_of
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' @importFrom haven labelled_spss
#' @family harmonization functions
#' @examples
#' \donttest{
#' examples_dir <- system.file("examples", package = "retroharmonize")
#' survey_list <- dir(examples_dir)[grepl("\\.rds", dir(examples_dir))]
#' 
#' example_surveys <- read_surveys(
#'   file.path( examples_dir, survey_list), 
#'   save_to_rds = FALSE)
#' 
#' metadata <- lapply ( X = example_surveys, FUN = metadata_create )
#' metadata <- do.call(rbind, metadata)
#' 
#' to_harmonize <- metadata %>%
#'   dplyr::filter ( var_name_orig %in% 
#'                   c("rowid", "w1") |
#'                   grepl("trust ", label_orig ) ) %>%
#'   dplyr::mutate ( var_label = var_label_normalize(label_orig)) %>%
#'   dplyr::mutate ( var_name = val_label_normalize(var_label))
#' 
#' harmonize_eb_trust <- function(x) {
#'   label_list <- list(
#'     from = c("^tend\\snot", "^cannot", "^tend\\sto", "^can\\srely",
#'              "^dk", "^inap", "na"), 
#'    to = c("not_trust", "not_trust", "trust", "trust",
#'            "do_not_know", "inap", "inap"), 
#'     numeric_values = c(0,0,1,1, 99997,99999,99999)
#'   )
#'   
#'   harmonize_values(x, 
#'                    harmonize_labels = label_list, 
#'                    na_values = c("do_not_know"=99997,
#'                                  "declined"=99998,
#'                                  "inap"=99999)
#'                    )
#' }
#' 
#' merged_surveys <- merge_waves ( example_surveys, var_harmonization = to_harmonize  )
#' 
#' harmonized <- harmonize_waves(waves = merged_surveys, 
#'                               .f = harmonize_eb_trust,
#'                               status_message = FALSE)
#'                               
#' # For details see Afrobarometer and Eurobarometer Case Study vignettes.
#' }

harmonize_waves <- function(waves, .f, status_message = FALSE) {
  
  validate_survey_list(waves)
  
  all_names <-  unique(unlist(lapply ( waves, names ))) #change from sapply
  
  classes <- unlist(lapply ( waves, function(x) lapply( x, function(y) class(y)[1]) ))
  #classes <- unlist(sapply ( waves, function(x) sapply( x, function(y) class(y)[1]) ))
  
  # The harmonization must take place by variable classes 
  # The retroharmonized, numeric, character, Date types are separately treated ---------------------
  
  retroharmonized <- unique(names(classes[which(classes == "retroharmonize_labelled_spss_survey")]))
  numerics <- unique(names(classes[which(classes %in% c("numeric", "double", "integer"))]))
  characters <- unique(names(classes[which(classes %in% c("character"))]))
  dates <- unique(names(classes[which(classes %in% c("Date"))]))
  other_types <- all_names[which(! all_names %in% c(retroharmonized, numerics, characters, dates))]
  
  assert_that(length(other_types)==0, 
              msg = "Only labelled_spss_survey, numeric, character and Date types are allowed.")
  
  original_attributes <- document_waves(waves)

  extend_survey <- function (dat) {
    
    ## Adds those variables that are missing in the particular wave 
    ## To end up with a single, tidy data.frame, the columns must be the same in all waves.
    ## The columns that are not present must be filled with empty data.
    
    to_add_rh <- retroharmonized[which(!retroharmonized %in% names(dat))]
    to_add_numerics <- numerics[which(!numerics %in% names(dat))]
    to_add_characters <- characters[which(!characters %in% names(dat))]
    to_add_dates <- dates[which(!dates %in% names(dat))]
    
    vars_to_add <- c(to_add_rh, to_add_numerics, 
                     to_add_characters, to_add_dates)
    
    if ( length(vars_to_add) == 0) return (dat)
    assert_that ( all(vars_to_add %in% names(dat))== FALSE)
    
    return_data <- dat
    
    if ( length(to_add_numerics)>0 ) {
      
      ## There are numeric values in other surveys that need to be 
      ## added with NA_real_ values here.
      
      add_numeric_df <- as.data.frame( 
        matrix (rep( NA_real_,
                     length(to_add_numerics)*nrow(dat)), 
                     nrow = nrow(dat), 
                     ncol = length(to_add_numerics)) 
        ) %>%
        stats::setNames(to_add_numerics)
      
      return_data <- bind_cols ( return_data, add_numeric_df)  
    }
    
    if ( length(to_add_characters)>0 ) {
    
      ## Now add the missing character variables   
      
      add_character_df <- as.data.frame(
        matrix ( rep( NA_character_,
                      length(to_add_characters)*nrow(dat)), 
                 nrow = nrow(dat)) 
       ) %>%
        stats::setNames(to_add_characters)
      
      return_data <- bind_cols ( return_data, add_character_df )
    }
    
    if ( length(to_add_dates)>0 ) {
    
      # Now add the missing date variables   
      add_dates_df <- as.data.frame(
        matrix ( rep( as.Date(NA),
                      length(to_add_dates)*nrow(dat)), 
                 nrow = nrow(dat)) 
      ) %>%
        stats::setNames(to_add_dates)
      
      return_data <- bind_cols (return_data, add_dates_df  )
    }
    
    if ( length(to_add_rh)>0 ) {
      
      fn_inap <- function(x) haven::labelled_spss(
        x,
        labels = c(inap = 99999), 
        na_values = c("do_not_know"=99997,
                      "declined"=99998,
                      "inap"=99999))
      
      add_rh_df <- as.data.frame( 
        matrix (rep( 99999,
                     length(to_add_rh)*nrow(dat)), 
                     ncol = length(to_add_rh),
                     nrow = nrow(dat)
                )
        ) %>%
        stats::setNames(to_add_rh) %>%
        mutate_all ( fn_inap )
      
      for ( i in ncol(add_rh_df)) {
        attr( add_rh_df[,i], "label") <- to_add_rh[i]
      }
      
      fn_convert <- function(x) {
        as_labelled_spss_survey(x, attr(dat, "id"))
      }
      
      add_rh_df2 <- as_tibble(lapply ( add_rh_df , fn_convert))

      return_data <- bind_cols ( return_data, add_rh_df2 )
    }
    
  not_added <- vars_to_add [! vars_to_add %in% names ( return_data )]
 
  assert_that( length(not_added ) == 0, 
              msg = paste0( "Could not add ", 
                            paste(not_added, collapse = ",")))
  
  return_data %>%
      select  (all_of(all_names))
    
  }
  
  extended <- lapply ( waves, extend_survey )
  
  #document_waves ( extended )

  full_join_characters <- do.call(
    vctrs::vec_rbind, 
    lapply ( extended, 
             function(x)  x %>% select ( all_of ( characters )) ))
  
  
  full_join_numerics <- do.call(
    vctrs::vec_rbind, 
    lapply ( extended, 
             function(x)  x %>% select ( all_of ( numerics )) ))
  
  full_join_dates <- do.call(
    vctrs::vec_rbind, 
    lapply ( extended, function(x)  x %>%
                                      select (all_of(dates)) %>%
                                      mutate_all (as.Date)))
  
  to_harmonize_labelled <-  lapply ( 
    ## select into a list vars that need to be harmonized by labels
    extended, 
    function(x) x %>% select (all_of (retroharmonized))
    )
  
  fn_harmonize <- function(dat, .f) {
    
    orig_name_order <- names(dat)
    if (status_message) message ( "Harmonize ", attr(dat, "id"))
    harmonized_list <- lapply ( dat[, retroharmonized], FUN = .f )

    retroh <- as_tibble(harmonized_list)
    
    dat %>% select ( -all_of(names(retroh))) %>%
      bind_cols(retroh) %>%
      select (all_of(orig_name_order)) 
  }
  
  dat <- to_harmonize_labelled[[1]]

  rth <- lapply ( to_harmonize_labelled,
                  function(x) fn_harmonize(x, .f) )
  
  #sapply ( rth, function(x) class(x$age))
  j_max <- length(rth)
  
  if (j_max >=2) {
    for ( j in 2:j_max ) {
      ## Validate all possible retroharmonized pairs before merging.
      survey1 <- rth[[j-1]]
      survey2 <- rth[[j]]
      for  ( i in retroharmonized ) {
        x <- survey1 %>% select (all_of(i)) %>% pull()
        y <- survey2 %>% select (all_of(i)) %>% pull()
        
        # remove superflous na_range if there are no values that match them
        x <- remove_na_range(x)
        y <- remove_na_range(y)
        
        vec_ptype2.retroharmonize_labelled_spss_survey.retroharmonize_labelled_spss_survey (
          x,y, 
          orig_names = i
        )
      }
    }
    
  }

  return_value <- rth[[1]]
  

  for (i in 2:j_max) {
   return_value <- vctrs::vec_rbind(return_value, rth[[i]])
  }
  
  if ( ncol(full_join_numerics) > 0 ) {
    return_value <- bind_cols ( full_join_numerics, return_value)
  }
  
  if ( ncol(full_join_characters) > 0 ) {
    return_value <- bind_cols ( full_join_characters, return_value)
  }
  
  if ( ncol(full_join_dates) > 0 ) {
    return_value <- bind_cols ( full_join_dates, return_value)
  }

  attributes (return_value)
  attr(return_value, "id") <- paste0("Waves: ", 
                                     paste ( original_attributes$id, collapse = "; " ))
  
  attr(return_value, "filename") <- paste0("Original files: ", 
                                     paste ( original_attributes$filename, collapse = "; " ))
  attributes (return_value)
  return_value
}
