#' Harmonize waves
#' 
#' Harmonize the values of surveys. It binds together variables
#' that are all present in the surveys, and applies a 
#' harmonization function on them. 
#' 
#' @param waves A list of surveys
#' @param .f A function to apply for the harmonization.
#' @export
#' @importFrom dplyr select bind_cols mutate_all
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

  extend_survey <- function (dat ) {
    
    to_add_rh <- retroharmonized[which(!retroharmonized %in% names(dat))]
    to_add_numerics <- numerics[which(!numerics %in% names(dat))]
    to_add_characters <- characters[which(!characters %in% names(dat))]
    
    add_numeric_df <- as.data.frame( matrix (length(to_add_numerics), nrow(dat))) %>%
      setNames(to_add_numerics)
    
    if ( length(to_add_numerics)>0) {
      dat <- dat %>% bind_cols(as_tibble ( matrix ( rep( NA_real_, 
                                length(to_add_numerics)*nrow(dat)), 
                           nrow = nrow(dat))) %>%
        stats::setNames(nm = to_add_numerics ))
    }
    
    if ( length(to_add_characters)>0) {
      dat <- dat %>% bind_cols(as_tibble (
        matrix ( rep( NA_character_,
                      length(to_add_characters)*nrow(dat)), 
                       nrow = nrow(dat))) %>%
                 stats::setNames(nm = to_add_characters ))
    }
    
    if ( length(to_add_rh)>0 ) {
      
      
      fn_inap <- function(x) haven::labelled_spss(x,
                                             labels = c(inap = 999999), 
                                             na_values = 999999)
        
  
      addition <-  as_tibble ( 
        matrix ( rep( 99999, 
                      length(to_add_rh)*nrow(dat)),
                 nrow = nrow(dat))
      ) %>%
        stats::setNames(nm = to_add_rh ) %>%
        mutate_all ( fn_inap )
      
      for ( i in ncol(addition)) {
        attr( addition[,i], "label") <- to_add_rh[i]
      }
      
      dat <- dat %>% bind_cols(
        as_tibble(
        lapply ( addition, function(x) as_labelled_spss_survey(x, attr(dat, "id")))
        ))
    }
    
    if ( ! all(sort(names ( dat )) == sort(all_names))) {
      stop ( "Extension error in ", attr(dat, "id"))
    }
    
    dat %>%
      select  (all_of(all_names))
    
  }
  
  extended <- lapply ( waves, extend_survey )
  
  fn_harmonize <- function(dat, .f) {
    
    orig_name_order <- names(dat)
    #message ( "Harmonize ", attr(dat, "id"))
    
    retroh <- lapply ( dat[, retroharmonized], FUN = .f )
    dat %>% select ( -all_of(names(retroh))) %>%
      bind_cols(retroh) %>%
      select (all_of(orig_name_order))
    
  }
  fn_harmonize ( dat = extended[[1]], .f = .f)
  
  tmp <- lapply ( extended, function(x) fn_harmonize(x, .f) )
  
  do.call(rbind, tmp)
}
