#' Harmonize waves
#' 
#' Harmonize the values of surveys. It binds together variables
#' that are all present in the surveys, and applies a 
#' harmonization function on them. 
#' 
#' @param waves A list of surveys
#' @param .f A function to apply for the harmonization.
#' @export
#' @importFrom dplyr select bind_cols mutate_all pull
#' @importFrom tidyselect all_of
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' @importFrom haven labelled_spss
#' @family harmonization functions

harmonize_waves <- function(waves, .f) {
  
  all_names <-  unique(unlist(sapply ( waves, names )))
  
  classes <- unlist(sapply ( waves, function(x) sapply( x, function(y) class(y)[1]) ))
  
  retroharmonized <- unique(names(classes[which(classes == "retroharmonize_labelled_spss_survey")]))
  numerics <- unique(names(classes[which(classes %in% c("numeric", "double", "integer"))]))
  characters <- unique(names(classes[which(classes %in% c("character"))]))
  dates <- unique(names(classes[which(classes %in% c("Date"))]))
  
  extend_survey <- function (dat) {
    
    to_add_rh <- retroharmonized[which(!retroharmonized %in% names(dat))]
    to_add_numerics <- numerics[which(!numerics %in% names(dat))]
    to_add_characters <- characters[which(!characters %in% names(dat))]
    to_add_dates <- dates[which(!dates %in% names(dat))]
    
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
      
      dat <- dat %>% 
        bind_cols(add_numeric_df) 
    }
    
    if ( length(to_add_characters)>0 ) {
      
      add_character_df <- as.data.frame(
        matrix ( rep( NA_character_,
                      length(to_add_characters)*nrow(dat)), 
                 nrow = nrow(dat)) %>%
          stats::setNames(to_add_characters)
      )
     
      dat <- dat %>% 
        bind_cols(add_character_df) 
    }
    
    if ( length(to_add_dates)>0 ) {
      
      add_dates_df <- as.data.frame(
        matrix ( rep( as.Date(NA),
                      length(to_add_dates)*nrow(dat)), 
                 nrow = nrow(dat)) %>%
          stats::setNames(to_add_dates)
      )
      
      dat <- dat %>% 
        bind_cols(add_dates_df) 
    }
    
    if ( length(to_add_rh)>0 ) {
      
      fn_inap <- function(x) haven::labelled_spss(
        x,
        labels = c(inap = 999999), 
        na_values = c("do_not_know"=99997,
                      "declined"=99998,
                      "inap"=99999))
      
      add_rh_df <- as.data.frame( 
        matrix (rep( 999999,
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

      dat <- dat %>%
        bind_cols(add_rh_df2)
    }
    
    if ( ! all(sort(names (dat)) == sort(all_names))) {
      stop ( "Extension error in ", attr(dat, "id"))
    }
    
   dat %>%
      select  (all_of(all_names))
    
  }
  
  #test_ext <- extend_survey ( waves[[1]] )
  
  extended <- lapply ( waves, extend_survey )

  full_join_characters <- do.call(
    vctrs::vec_rbind, 
    lapply ( extended, function(x)  x %>% select ( all_of ( characters )) ))
  
  
  full_join_numerics <- do.call(
    vctrs::vec_rbind, 
    lapply ( extended, function(x)  x %>% select ( all_of ( numerics )) ))
  
  full_join_dates <- do.call(
    vctrs::vec_rbind, 
    lapply ( extended, function(x)  x %>% select ( all_of ( dates )) ))
  
  to_harmonize_labelled <-  lapply ( 
    extended, function(x) x %>% select (all_of (retroharmonized )))
  
  fn_harmonize <- function(dat, .f) {
    
    orig_name_order <- names(dat)
    #message ( "Harmonize ", attr(dat, "id"))
    
    retroh <- as_tibble(lapply ( dat[, retroharmonized], FUN = .f ))
    
    #x <-dat$trust_tax_department
  
    dat %>% select ( -all_of(names(retroh))) %>%
      bind_cols(retroh) %>%
      select (all_of(orig_name_order)) 
  }
  
  rth <- lapply ( to_harmonize_labelled,
                  function(x) fn_harmonize(x, .f) )
  
  for ( j in 2:length(rth) ) {
    ## Validate all possible retroharmonized pairs before merging.
    survey1 <- rth[[j-1]]
    survey2 <- rth[[j]]
    for  ( i in retroharmonized ) {
      x = survey1 %>% select (all_of(i)) %>% pull()
      y = survey2 %>% select (all_of(i)) %>% pull()
      
      vec_ptype2.retroharmonize_labelled_spss_survey.retroharmonize_labelled_spss_survey (
        x,y, 
        orig_names = i
      )
    }
  }
  
  return_value <- rth[[1]]
  
  attributes (return_value)
  for (i in 2:length(rth)) {
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
  
  return_value
}

