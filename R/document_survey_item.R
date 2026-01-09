#' @title Document survey item harmonization
#'
#' @description Document the current and historic coding and labelling of the
#' variable.
#'
#' @param x A labelled_spss_survey vector from a single survey
#' or concatenated from several surveys.
#' @return Returns a list of the current and historic coding, labelling
#' of the valid range and missing values or range, the history of the
#' variable names and the history of the survey IDs.
#' @importFrom rlang set_names
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @family documentation functions
#' @examples
#' var1 <- labelled::labelled_spss(
#'   x = c(1, 0, 1, 1, 0, 8, 9),
#'   labels = c(
#'     "TRUST" = 1,
#'     "NOT TRUST" = 0,
#'     "DON'T KNOW" = 8,
#'     "INAP. HERE" = 9
#'   ),
#'   na_values = c(8, 9)
#' )
#'
#' var2 <- labelled::labelled_spss(
#'   x = c(2, 2, 8, 9, 1, 1),
#'   labels = c(
#'     "Tend to trust" = 1,
#'     "Tend not to trust" = 2,
#'     "DK" = 8,
#'     "Inap" = 9
#'   ),
#'   na_values = c(8, 9)
#' )
#'
#' h1 <- harmonize_values(
#'   x = var1,
#'   harmonize_label = "Do you trust the European Union?",
#'   harmonize_labels = list(
#'     from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk|^don", "^inap"),
#'     to = c("trust", "not_trust", "do_not_know", "inap"),
#'     numeric_values = c(1, 0, 99997, 99999)
#'   ),
#'   na_values = c(
#'     "do_not_know" = 99997,
#'     "inap" = 99999
#'   ),
#'   id = "survey1",
#' )
#'
#' h2 <- harmonize_values(
#'   x = var2,
#'   harmonize_label = "Do you trust the European Union?",
#'   harmonize_labels = list(
#'     from = c("^tend\\sto|^trust", "^tend\\snot|not\\strust", "^dk|^don", "^inap"),
#'     to = c("trust", "not_trust", "do_not_know", "inap"),
#'     numeric_values = c(1, 0, 99997, 99999)
#'   ),
#'   na_values = c(
#'     "do_not_know" = 99997,
#'     "inap" = 99999
#'   ),
#'   id = "survey2"
#' )
#'
#' h3 <- concatenate(h1, h2)
#' document_survey_item(h3)
#' @export

document_survey_item <- function(x) {
  attribute_names <- names(attributes(x))
  original_x_name <- deparse(substitute(x))

  orig_names <- attribute_names[grepl("name$", attribute_names)]
  attr_label <- attribute_names[grepl("label$", attribute_names)]
  labels <- attribute_names[grepl("labels$", attribute_names)]

  attr_na_range <- attribute_names[grepl("na_range$", attribute_names)]
  coding <- as_tibble(sapply(labels, function(l) attr(x, l)))
  coding <- rlang::set_names(coding, gsub("labels", "values", names(coding)))
  labelling <- as_tibble(sapply(labels, function(l) names(attr(x, l))))
  tbl_length <- nrow(coding)

  list(
    code_table = bind_cols(coding, labelling) %>%
      mutate(missing = ifelse(values %in% attr(x, "na_values"),
        TRUE, FALSE
      )),
    history_var_name = c(
      c("name" = original_x_name),
      vapply(orig_names, function(l) attr(x, l), character(1))
    ),
    history_var_label = c(
      vapply(attr_label, function(l) attr(x, l), character(1))
    ),
    history_na_range = c(
      vapply(attr_na_range, function(l) attr(x, l), character(1))
    ),
    history_id = history_survey_id <- attr(x, "id")
  )
}
