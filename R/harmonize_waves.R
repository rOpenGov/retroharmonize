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
    
    if ( length(to_add_numerics)>0) {
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
          stats::setNames(to_add_numerics)
      )
     
      dat <- dat %>% 
        bind_cols(add_character_df) 
    }
    
    if ( length(to_add_rh)>0 ) {
      
      fn_inap <- function(x) haven::labelled_spss(x,
                                                  labels = c(inap = 999999), 
                                                  na_values = 999999)
      
      add_rh_df <- as.data.frame( 
        matrix (rep( 999999,
                     length(to_add_rh)*nrow(dat)), 
                     ncol = length(to_add_rh),
                     nrow = nrow(dat)
                )
        ) 
      
      names ( add_rh_df) <- gsub("`", "", to_add_rh)
      
      to_add_rh[1]
      
      add_rh_df$`trust_justice-system`
      
  
      
      %>%
        stats::setNames(to_add_rh) %>%
        mutate_all ( fn_inap )
      
      
     
      for ( i in ncol(add_rh_df)) {
        attr( add_rh_df[,i], "label") <- to_add_rh[i]
      }
      
      dat <- dat %>%
        bind_cols(add_rh_df)
    }
    
    if ( ! all(sort(names ( dat )) == sort(all_names))) {
      stop ( "Extension error in ", attr(dat, "id"))
    }
    
   dat %>%
      select  (all_of(all_names))
    
  }
  
  ext <- extend_survey(dat)
  names ( ext )
  ext$trust_european-parliament
  
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
