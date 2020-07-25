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

  dat <- waves[[1]]
  
  extend_survey <- function (dat) {
    
    to_add_rh <- retroharmonized[which(!retroharmonized %in% names(dat))]
    to_add_numerics <- numerics[which(!numerics %in% names(dat))]
    to_add_characters <- characters[which(!characters %in% names(dat))]
    
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
          stats::setNames(to_add_numerics)
      )
     
      dat <- dat %>% 
        bind_cols(add_character_df) 
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
  
  fn_harmonize <- function(dat, .f) {
    
    orig_name_order <- names(dat)
    #message ( "Harmonize ", attr(dat, "id"))
    
    retroh <- as_tibble(lapply ( dat[, retroharmonized], FUN = .f ))
    
    #x <-dat$trust_tax_department
  
    dat %>% select ( -all_of(names(retroh))) %>%
      bind_cols(retroh) %>%
      select (all_of(orig_name_order)) 
  }
  
  dat <- extended[[1]]
  tmp <- lapply (extended, function(x) fn_harmonize(x, .f))
  
  return_value <- tmp[[1]]
  for (i in 2:length(tmp)) {
    
    match_labels <-sapply ( 1:ncol(return_value), function(x) {
      as.character(labelled::val_labels (return_value)) ==
        as.character(labelled::val_labels (tmp[[i]]))}
      )
    
    not_matching <- match_labels[which(!match_labels)]
    
    if ( length(not_matching)>0 ) {
      warning( "The join ", i, " is not matching in ", not_matching, 
               " in ", attr(tmp[[i]], "id") ,'.' )
    }
    
    message ( "Joining ", i, "/", length(tmp), ": ", attr(tmp[[i]], "id") )
    return_value <- vctrs::vec_rbind(tmp[[1]], tmp[[i]])
  }
  
  return_value
 
}

