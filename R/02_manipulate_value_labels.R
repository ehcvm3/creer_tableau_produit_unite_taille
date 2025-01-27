#' Get value labels for a variable
#'
#' @param qnr_df Questionnaire data frame
#' @param varname Atomic character vector.
#' Variable name as it appears in Designer.
#'
#' @return Named numeric vector. Numbers are answer option codes.
#' Names are labels.
#'
#' @importFrom susometa get_answer_options
#' @importFrom rlang sym
#' @importFrom stringr str_extract
get_value_labels <- function(
  qnr_df,
  varname
) {

  products <- qnr_df |>
    susometa::get_answer_options(varname = !!rlang::sym(varname))

  product_values <- names(products) |>
    stringr::str_extract(pattern = "(?<=__)[0-9]+$") |>
    as.numeric()

  product_lbls <- stats::setNames(
    object = product_values,
    nm = products
  )

  return(product_lbls)

}

#' Construct single set of variable labels from several variables
#'
#' @description Extract variable labels, combine them, and remove duplicates
#'
#' @param qnr_df Questionnaire data frame
#' @param varnames Character vector. Names of target variables. Use
#' names as they appear in Designer
#'
#' @return Named character vector. Values are answer option codes.
#' Names are answer option labels.
#'
#' @importFrom purrr map list_c
construct_values_labels <- function(
  qnr_df,
  varnames
) {

  # collect value labels in a list of vectors
  val_lbl_list <- purrr::map(
    .x = varnames,
    .f = ~ get_value_labels(qnr_df = qnr_df, varname = .x)
  )

  # combine the elements into a single vector
  val_lbls <- purrr::list_c(val_lbl_list)

  # remove duplicates
  dup_lbls <- duplicated(val_lbls)
  val_lbls_unique <- val_lbls[dup_lbls == FALSE]

  # reorder labels in ascending order
  val_lbls_ordered <- val_lbls_unique[order(val_lbls_unique)]

  return(val_lbls_ordered)

}

#' Get product codes for EHCVM3 food group
#'
#' @param product_df Data frame. Data gleaned from EHCVM3 Excel questionnaire.
#' @param food_group Character vector. Food group names in EHCVM languages.
#' For the moment, French and Portuguese.
#'
#' @importFrom dplyr filter pull
get_products_for_food_group <- function(
  product_df,
  food_group
) {

  prods_ehcvm3 <- product_df |>
    dplyr::filter(type_produit %in% food_group) |>
    dplyr::pull(produit_code)

}

#' Create a data fram of units for a food group
#'
#' @param qnr_df Data frame.
#' Contains questionnaire metadata returned by susometa
#' @param varname Character.
#' Name of the unit variable in the target food group roster.
#'
#' @return Data frame. Columns `unite_code` and `unite_texte`.
create_units_tbl <- function(
  qnr_df,
  varname
) {

  units_lbls <- construct_values_labels(
    qnr_df = qnr_df,
    varnames = varname
  )

  units_tbl <- data.frame(
    unite_code = units_lbls,
    unite_texte = names(units_lbls),
    stringsAsFactors = FALSE
  )

  return(units_tbl)

}