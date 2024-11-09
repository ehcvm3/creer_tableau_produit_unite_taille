# ==============================================================================
# installer les packages requis
# ==============================================================================

# ------------------------------------------------------------------------------
# définir un programmes de travail pour l'identification et l'installation de
# programmes absents
# ------------------------------------------------------------------------------

#' Install package if missing on system
#' 
#' @param package Character. Name of package to install.
#'
#' @importFrom stringr str_detect str_locate str_sub
install_if_missing <- function(package) {

  # if package contains a slash for GitHub repo, strip out the package name
  # and install that package
  slash_pattern <- "\\/"
  if (stringr::str_detect(string = package, pattern = slash_pattern)) {

    package_url <- package

    slash_position <- stringr::str_locate(
      string = package,
      pattern = slash_pattern
    )
    package <- stringr::str_sub(
      string = package,
      start = slash_position[[1]] + 1
    )

    if (!require(package, quietly = TRUE, character.only = TRUE)) {
      pak::pak(package_url)
    }

  # otherwises, install the package
  } else {

    if (!require(package, quietly = TRUE, character.only = TRUE)) {
      pak::pak(package)
    }

  }

}

# ------------------------------------------------------------------------------
# installer les programmes absents
# ------------------------------------------------------------------------------

packages_requis <- c(
  "fs", # opérations le système des fichiers
  "readr", # ingérer les fichiers délimités
  "dplyr", # manipulation des données
  "lsms-worldbank/susometa", # extraires des infos de la métadonnée du questionnaire
  "rlang", # programmation
  "readxl", # ingérer les fichiers Excel
  "tidyr", # manipulation de la structure des tableaux
  "labelled", # manipulation des étiquettes de valeur
  "writexl" # écrire un fichier Excel
)

# installer des packages pour faciliter l'installation

# itération et manipulation des listes
if (!base::require("purrr", quietly = TRUE)) {
  install.packages("purrr")
}
# manipulation des données string
if (!base::require("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
# méthode d'installation de packages plus rapide, fiable, et moderne
if (!base::require("pak", quietly = TRUE)) {
  install.packages("pak")
}

purrr::walk(
  .x = packages_requis,
  .f = ~ install_if_missing(.x)
)

# ==============================================================================
# ingérer les tableaux
# ==============================================================================

# faire l'inventaire des tableaux
chemin_tableaux <- fs::dir_ls(
  path = fs::path(here::here(), "data", "in", "ehcvm2"),
  type = "file",
  regexp = "\\.tab"
)

tableaux_liste <- purrr::map(
  .x = chemin_tableaux,
  .f = ~ readr::read_tsv(file = .x)
)

tableaux_df <- dplyr::bind_rows(tableaux_liste) |>
  # renumber rowcode to arbitrary sequential number
  dplyr::mutate(rowcode = dplyr::row_number()) |>
  # rename variables in snake case
  dplyr::select(
    produit_code = produitCode,
    unite_code = uniteCode,
    taille_code = tailleCode
  )

# ==============================================================================
# obtenir les étiquettes de valeur des produits, d'unité, et de taille
# ==============================================================================

# ------------------------------------------------------------------------------
# define functions for extracting values labels
# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------
# ECHVM2
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# ingest questionnaire JSON as a data frame
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

qnr_ehcvm2 <- fs::path(here::here(), "data", "in", "ehcvm2", "document.json") |>
  susometa::parse_questionnaire()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# products
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

product_vars_ehcvm2 <- c(
  "s07Bq02_cereales",
  "s07Bq02_viandes",
  "s07Bq02_poissons",
  "s07Bq02_lait",
  "s07Bq02_huiles",
  "s07Bq02_fruits",
  "s07Bq02_legumes",
  "s07Bq02_legtub",
  "s07Bq02_sucreries",
  "s07Bq02_epices",
  "s07Bq02_boissons"
)

product_val_lbls_ehcvm2 <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = product_vars_ehcvm2
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# units
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

unit_vars <- c(
  "s07Bq03b_cereales",
  "s07Bq03b_viandes",
  "s07Bq03b_poissons",
  "s07Bq03b_huiles",
  "s07Bq03b_laitier",
  "s07Bq03b_fruits",
  "s07Bq03b_legumes",
  "s07Bq03b_legtub",
  "s07Bq03b_sucreries",
  "s07Bq03b_epices",
  "s07Bq03b_boissons"
)

unit_val_lbls <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = unit_vars
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# sizes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

size_vars <- c(
  "s07Bq03c_cereales",
  "s07Bq03c_viandes",
  "s07Bq03c_poissons",
  "s07Bq03c_huiles",
  "s07Bq03c_laitier",
  "s07Bq03c_fruits",
  "s07Bq03c_legumes",
  "s07Bq03c_legtub",
  "s07Bq03c_sucreries",
  "s07Bq03c_epices",
  "s07Bq03c_boissons"
)

size_val_lbls <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = size_vars
)

# ------------------------------------------------------------------------------
# ECHVM3
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# products
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

produits <- fs::path(here::here(), "data", "in", "ehcvm3", "EHCVM3_UEMOA_MEN_04112024.xlsx") |>
  readxl::read_excel(
    sheet = "S7b_Conso_Al",
    skip = 16
  ) |>
  dplyr::rename(
    produit_code = 1,
    produit_nom = 2
  ) |>
  dplyr::select(produit_code, produit_nom) |>
  dplyr::mutate(
    type_produit = dplyr::if_else(
      condition = is.na(produit_code),
      true = produit_nom,
      false = NA_character_
    )
  ) |>
  tidyr::fill(type_produit, .direction = "down") |>
  dplyr::filter(!is.na(produit_code))

produits_lbls_ehcvm3 <- stats::setNames(
  object = produits$produit_code,
  nm = produits$produit_nom
)

cereales_ehcvm3 <- produits |>
  dplyr::filter(type_produit == "CÉRÉALES ET PAINS") |>
  dplyr::pull(produit_code)

viandes_ehcvm3 <- produits |>
  dplyr::filter(type_produit == "VIANDE") |>
  dplyr::pull(produit_code)

poissons_ehcvm3 <- produits |>
  dplyr::filter(type_produit == "POISSON ET FRUITS DE MER") |>
  dplyr::pull(produit_code)

laitier_ehcvm3 <- produits |>
  dplyr::filter(type_produit == "LAIT, FROMAGE ET OEUFS") |>
  dplyr::pull(produit_code)

huiles_ehcvm3 <- produits |>
  dplyr::filter(type_produit == "HUILES ET GRAISSES") |>
  dplyr::pull(produit_code)

fruits_ehcvm3 <- produits |>
  dplyr::filter(type_produit == "FRUITS") |>
  dplyr::pull(produit_code)

legumes_ehcvm3 <- produits |>
  dplyr::filter(type_produit == "LÉGUMES") |>
  dplyr::pull(produit_code)

leg_tub_ehcvm3 <- produits |>
  dplyr::filter(type_produit == "LEGUMINEUSES ET TUBERCULES") |>
  dplyr::pull(produit_code)

sucreries_ehcvm3 <- produits |>
  dplyr::filter(type_produit == "SUCRE, MIEL, CHOCOLAT ET CONFISERIE") |>
  dplyr::pull(produit_code)

epices_ehcvm3 <- produits |>
  dplyr::filter(type_produit == "EPICES, CONDIMENTS ET AUTRES") |>
  dplyr::pull(produit_code)

boissons_ehcvm3 <- produits |>
  dplyr::filter(type_produit == "BOISSONS") |>
  dplyr::pull(produit_code)

# ==============================================================================
# actualiser
# ==============================================================================

# ------------------------------------------------------------------------------
# céréales
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# renuméroter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

cereales_lbls <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = "s07Bq02_cereales"
)

cereales_df <- tableaux_df |>
  dplyr::filter(produit_code %in% cereales_lbls)

cereales_renum_indices <- c(
  1, # Riz local 1
  2, # Riz local 2
  3, # Riz importé 1
  4, # Riz importé 2
  5, # maïs en épi
  6, # maïs en grain
  7, # mil
  8, # sorgho
  9, # blé
  10, # fonio
  11, # autres céréales
  12, # farine de maïs
  13, # semoule de mais
  14, # farine de mil
  15, # semoule de mil
  16, # farine de blé locale ou importée
  17, # semoule de blé
  18, # autres farines de céréales
  19, # autres semoules de céréales
  23, # croissants
  24, # biscuits
  25, # gâteaux
  26 # beignets, galettes
)

cereales_renum_df <- cereales_df |>
  dplyr::filter(produit_code %in% cereales_renum_indices) |>
  dplyr::mutate(
    produit_code = dplyr::case_when(
      produit_code == 1 ~ 1, # Riz local 1
      produit_code == 2 ~ 2, # Riz local 2
      produit_code == 3 ~ 3, # Riz importé 1
      produit_code == 4 ~ 4, # Riz importé 2
      produit_code == 5 ~ 6, # Maïs en épi
      produit_code == 6 ~ 7, # Maïs en grain
      produit_code == 7 ~ 8, # Mil
      produit_code == 8 ~ 9, # Sorgho
      produit_code == 9 ~ 10, # Blé
      produit_code == 10 ~ 11, # Fonio
      produit_code == 11 ~ 12, # Autres céréales
      produit_code == 12 ~ 13, # Farine de maïs
      produit_code == 13 ~ 14, # semoule de mais
      produit_code == 14 ~ 15, # Farine de mil
      produit_code == 15 ~ 16, # Semoule de mil
      produit_code == 16 ~ 17, # Farine de blé locale ou importée
      produit_code == 17 ~ 18, # Semoule de blé
      produit_code == 18 ~ 19, # Autres farines de céréales
      produit_code == 19 ~ 20, # Autres semoules de céréales
      produit_code == 23 ~ 29, # Croissants
      produit_code == 24 ~ 30, # Biscuits
      produit_code == 25 ~ 31, # Gâteaux
      produit_code == 26 ~ 32, # Beignets, galettes
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# pâtes alimentaires -> plusiers pâtes spécifiques
pates_alimentaires <- dplyr::filter(cereales_df, produit_code == 20)
macaroni <- dplyr::mutate(pates_alimentaires, produit_code = 21)
spaghettis <- dplyr::mutate(pates_alimentaires, produit_code = 22)
autres_pates_alimentaires <- dplyr::mutate(pates_alimentaires, produit_code = 23)

# pain moderne -> plusiers pains spécifiques
pain_moderne <- dplyr::filter(cereales_df, produit_code == 21)
pain_moderne_1 <- dplyr::mutate(pain_moderne, produit_code = 24)
pain_moderne_2 <- dplyr::mutate(pain_moderne, produit_code = 25)

# pain traditionnel -> plusiers pains spécifiques
pain_traditionnel <- dplyr::filter(cereales_df, produit_code == 22)
pain_traditionnel_1 <- dplyr::mutate(pain_traditionnel, produit_code = 26)
pain_traditionnel_2 <- dplyr::mutate(pain_traditionnel, produit_code = 27)

# ajouter un 3ième riz importé
riz_importe_3 <- cereales_df |>
  dplyr::filter(produit_code == 3) |>
  dplyr::mutate(produit_code = 5)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

cereales_df_echvm <- cereales_renum_df |>
  dplyr::bind_rows(
    # pâtes
    macaroni, spaghettis, autres_pates_alimentaires,
    # pains
    pain_moderne_1, pain_moderne_2,
    pain_traditionnel_1, pain_traditionnel_2,
    # riz
    riz_importe_3
  )

cereales_in_ehcvm3 <- cereales_df_echvm |>
  dplyr::distinct(produit_code) |>
  dplyr::pull(produit_code)

cereales_not_in_ehcvm3 <- cereales_ehcvm3[!cereales_ehcvm3 %in% cereales_in_ehcvm3]

cereales_missing <- cereales_df_echvm |>
  tidyr::expand(produit_code = cereales_not_in_ehcvm3)

cereales_df_ehcvm3 <- cereales_df_echvm |>
  dplyr::full_join(cereales_missing, by = "produit_code")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# viandes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# renuméroter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

viandes_lbls <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = "s07Bq02_viandes"
)

viandes_df <- tableaux_df |>
  dplyr::filter(produit_code %in% viandes_lbls)

viandes_renum_indices <- c(
  27, # Viande de bœuf
  28, # Viande de chameau
  29, # Viande de mouton
  30, # Viande de chèvre
  32, # Viande de porc
  33, # Poulet sur pied
  171, # Autre volaille sur pied
  34, # Viande de poulet
  35, # Viande d'autres volailles domestiques  
  37, # Viande séchée (boeuf, mouton, chameau) 
  39 # Autres viandes n.d.a.
)

viandes_renum_df <- viandes_df |>
  dplyr::filter(produit_code %in% viandes_renum_indices) |>
  dplyr::mutate(
    produit_code = dplyr::case_when(
      produit_code == 27 ~ 33, # Viande de bœuf
      produit_code == 29 ~ 34, # Viande de mouton
      produit_code == 30 ~ 35, # Viande de chèvre
      produit_code == 28 ~ 36, # Viande de chameau
      produit_code == 32 ~ 37, # Viande de porc
      produit_code == 34 ~ 47, # Viande de poulet
      produit_code == 35 ~ 48, # Viande d'autres volailles domestiques  
      produit_code == 33 ~ 49, # Poulet sur pied
      produit_code == 171 ~ 51, # Autre volaille sur pied
      produit_code == 37 ~ 54, # Viande séchée (boeuf, mouton, chameau) 
      produit_code == 39 ~ 58, # Autres viandes n.d.a.
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# gibier -> plusieurs gibiers spécifiques
gibier <- dplyr::filter(viandes_df, produit_code == 38)
gros_gibier <- dplyr::mutate(gibier, produit_code = 42)
petits_gibier <- dplyr::mutate(gibier, produit_code = 43)
gibier_a_plumes <- dplyr::mutate(gibier, produit_code = 44)
autres_gibier <- dplyr::mutate(gibier, produit_code = 45)

# abats -> plusieurs abats spécifiques
abats <- dplyr::filter(viandes_df, produit_code == 31)
abats_boeuf <- dplyr::mutate(abats, produit_code = 52)
abats_volaille <- dplyr::mutate(abats, produit_code = 53)

# charcuterie -> plusieurs variétés
charcuterie <- dplyr::filter(viandes_df, produit_code == 36)
jambon <- dplyr::mutate(charcuterie, produit_code = 55)
saucisson <- dplyr::mutate(charcuterie, produit_code = 56)
conserves_viande <- dplyr::mutate(charcuterie, produit_code = 57)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

viandes_df_echvm <- viandes_renum_df |>
  dplyr::bind_rows(
    # gibier
    gros_gibier, petits_gibier, gibier_a_plumes, autres_gibier,
    # abats
    abats_boeuf, abats_volaille,
    # charcuterie
    jambon, saucisson, conserves_viande
  )

viandes_in_ehcvm3 <- viandes_df_echvm |>
  dplyr::distinct(produit_code) |>
  dplyr::pull(produit_code)

viandes_not_in_ehcvm3 <- viandes_ehcvm3[!viandes_ehcvm3 %in% viandes_in_ehcvm3]

viandes_missing <- viandes_df_echvm |>
  tidyr::expand(produit_code = viandes_not_in_ehcvm3)

viandes_df_ehcvm3 <- viandes_df_echvm |>
  dplyr::full_join(viandes_missing, by = "produit_code")

# ------------------------------------------------------------------------------
# poissons
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# renuméroter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

poissons_lbls <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = "s07Bq02_poissons"
)

poissons_df <- tableaux_df |>
  dplyr::filter(produit_code %in% poissons_lbls)

poissons_renum_indices <- c(
  40, # Poisson frais type 1
  41, # Poisson frais type 2
  42, # Poisson frais type 3
  43, # Poisson frais type 4
  44, # Poisson fumé type 1
  45, # Poisson fumé type 2
  172, # Poisson fumé type 3
  47, # Crabes
  48, # Crevettes/Gambas fraîches
  49, # Crevettes séchées
  173, # Escargots
  50, # Autres fruits de mer
  51 # Conserves de poisson
)

poissons_renum_df <- poissons_df |>
  dplyr::filter(produit_code %in% poissons_renum_indices) |>
  dplyr::mutate(
    produit_code = dplyr::case_when(
      produit_code == 40 ~ 59, # Poisson frais type 1
      produit_code == 41 ~ 60, # Poisson frais type 2
      produit_code == 42 ~ 61, # Poisson frais type 3
      produit_code == 43 ~ 62, # Poisson frais type 4
      produit_code == 44 ~ 63, # Poisson fumé type 1
      produit_code == 45 ~ 64, # Poisson fumé type 2
      produit_code == 172 ~ 65, # Poisson fumé type 3
      produit_code == 47 ~ 69, # Crabes
      produit_code == 48 ~ 70, # Crevettes/Gambas fraîches
      produit_code == 49 ~ 71, # Crevettes séchées
      produit_code == 50 ~ 72, # Autres fruits de mer
      produit_code == 173 ~ 73, # Escargot
      produit_code == 51 ~ 74, # Conserves de poisson
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# créer 3 postes de poisson séché
poisson_seche <- dplyr::filter(poissons_df, produit_code == 46)
poisson_seche_1 <- dplyr::mutate(poisson_seche, produit_code = 66)
poisson_seche_2 <- dplyr::mutate(poisson_seche, produit_code = 67)
poisson_seche_3 <- dplyr::mutate(poisson_seche, produit_code = 68)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

poissons_df_echvm <- poissons_renum_df |>
  dplyr::bind_rows(
    # poisson séché
    poisson_seche_1, poisson_seche_2, poisson_seche_3
  )

poissons_in_ehcvm3 <- poissons_df_echvm |>
  dplyr::distinct(produit_code) |>
  dplyr::pull(produit_code)

poissons_not_in_ehcvm3 <- poissons_ehcvm3[
  !poissons_ehcvm3 %in% poissons_in_ehcvm3
]

poissons_missing <- poissons_df_echvm |>
  tidyr::expand(produit_code = poissons_not_in_ehcvm3)

poissons_df_ehcvm3 <- poissons_df_echvm |>
  dplyr::full_join(poissons_missing, by = "produit_code")


# ------------------------------------------------------------------------------
# laitier
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# renuméroter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

laitier_lbls <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = "s07Bq02_lait"
)

laitier_df <- tableaux_df |>
  dplyr::filter(produit_code %in% laitier_lbls)

laitier_renum_indices <- c(
  52, # Lait frais
  174, # Lait frais type 2
  54, # Lait concentré sucré
  55, # Lait concentré non-sucré
  56, # Lait en poudre
  57, # Fromage
  59, # Autres produits laitiers
  60 # Œufs frais
)

laitier_renum_df <- laitier_df |>
  dplyr::filter(produit_code %in% laitier_renum_indices) |>
  dplyr::mutate(
    produit_code = dplyr::case_when(
      produit_code == 52 ~ 77, # Lait frais
      produit_code == 174 ~ 78, # Lait frais type 2
      produit_code == 54 ~ 81, # Lait concentré sucré
      produit_code == 55 ~ 82, # Lait concentré non-sucré
      produit_code == 56 ~ 83, # Lait en poudre
      produit_code == 57 ~ 84, # Fromage
      produit_code == 59 ~ 87, # Autres produits laitiers
      produit_code == 60 ~ 88, # Œufs frais
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# lait et farines pour bébé -> lait pour bébé, farines pour bébé
lait_farine <- dplyr::filter(laitier_df, produit_code == 58)
lait_bebe <- dplyr::mutate(lait_farine, produit_code = 85)
farine_bebe <- dplyr::mutate(lait_farine, produit_code = 86)

# lait caillé/yaourt -> lait caillé, yaourt
lait_caille_yaourt <- dplyr::filter(laitier_df, produit_code == 53)
lait_caille <- dplyr::mutate(laitier_df, produit_code = 79)
lait_yaourt <- dplyr::mutate(laitier_df, produit_code = 80)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

laitier_df_echvm <- laitier_renum_df |>
  dplyr::bind_rows(
    # lait et farine pour bébé
    lait_bebe, farine_bebe,
    # lait caillé/yaourt
    lait_caille, lait_yaourt
  )

laitier_in_ehcvm3 <- laitier_df_echvm |>
  dplyr::distinct(produit_code) |>
  dplyr::pull(produit_code)

laitier_not_in_ehcvm3 <- laitier_ehcvm3[
  !laitier_ehcvm3 %in% laitier_in_ehcvm3
]

laitier_missing <- laitier_df_echvm |>
  tidyr::expand(produit_code = laitier_not_in_ehcvm3)

laitier_df_ehcvm3 <- laitier_df_echvm |>
  dplyr::full_join(laitier_missing, by = "produit_code")


# ------------------------------------------------------------------------------
# huiles
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# renuméroter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

huiles_lbls <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = "s07Bq02_huiles"
)

huiles_df <- tableaux_df |>
  dplyr::filter(produit_code %in% huiles_lbls)

huiles_renum_indices <- c(
  61, # "Beurre"
  62, # "Beurre de karité"
  63, # "Huile de palme brute"
  68, # "Huile de palme raffinée"
  175, # Huile de karité
  64, # "Huile d'arachide raffinée"
  65, # "Huile d'arachide 'Segal'"
  66, # "Huile de soja "
  67, # "Huile de coton"
  69, # "Noix de palme"
  70 # "Autres huiles alimentaires n.d.a. (maïs, palmiste, olive, tournesol, coco, lait de vache, etc.)"
)

huiles_renum_df <- huiles_df |>
  dplyr::filter(produit_code %in% huiles_renum_indices) |>
  dplyr::mutate(
    produit_code = dplyr::case_when(
      produit_code == 61 ~ 89, # "Beurre"
      produit_code == 62 ~ 90, # "Beurre de karité"
      produit_code == 63 ~ 91, # "Huile de palme brute"
      produit_code == 68 ~ 92, # "Huile de palme raffinée"
      produit_code == 64 ~ 93, # "Huile d'arachide raffinée"
      produit_code == 65 ~ 94, # "Huile d'arachide 'Segal'"
      produit_code == 66 ~ 95, # "Huile de soja "
      produit_code == 67 ~ 96, # "Huile de coton"
      produit_code == 175 ~ 97, # "Huile de karité"
      produit_code == 69 ~ 98, # "Noix de palme"
      produit_code == 70 ~ 99, # "Autres huiles alimentaires n.d.a. (maïs, palmiste, olive, tournesol, coco, lait de vache, etc.)"
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# N/A

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

huiles_df_echvm <- huiles_renum_df

huiles_in_ehcvm3 <- huiles_df_echvm |>
  dplyr::distinct(produit_code) |>
  dplyr::pull(produit_code)

huiles_not_in_ehcvm3 <- huiles_ehcvm3[!huiles_ehcvm3 %in% huiles_in_ehcvm3]

huiles_missing <- huiles_df_echvm |>
  tidyr::expand(produit_code = huiles_not_in_ehcvm3)

huiles_df_ehcvm3 <- huiles_df_echvm |>
  dplyr::full_join(huiles_missing, by = "produit_code")

# ------------------------------------------------------------------------------
# fruits
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# renuméroter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

fruits_lbls <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = "s07Bq02_fruits"
)

fruits_df <- tableaux_df |>
  dplyr::filter(produit_code %in% fruits_lbls)

fruits_renum_indices <- c(
  71, # "Mangue"
  72, # "Ananas"
  73, # "Orange "
  74, # "Citron"
  76, # "Banane douce"
  77, # "Avocat"
  78, # "Pastèque"
  79, # "Melon"
  80, # "Dattes"
  81, # "Noix de coco"
  82, # "Canne à sucre"
  83, # "Pommes"
  84, # "Papaye"
  176, # Goyave
  85, # "Fruit de baobab"
  86, # "Néré"
  87 # "Autres fruits (raisin, fraise, poire, tamarin noir, liane sauvage, pomme sauvage, etc.)
)

fruits_renum_df <- fruits_df |>
  dplyr::filter(produit_code %in% fruits_renum_indices) |>
  dplyr::mutate(
    produit_code = dplyr::case_when(
      produit_code == 71 ~ 100, # "Mangue"
      produit_code == 72 ~ 101, # "Ananas"
      produit_code == 73 ~ 102, # "Orange "
      produit_code == 74 ~ 103, # "Citron"
      produit_code == 76 ~ 106, # "Banane douce"
      produit_code == 77 ~ 107, # "Avocat"
      produit_code == 78 ~ 108, # "Pastèque"
      produit_code == 79 ~ 109, # "Melon"
      produit_code == 80 ~ 110, # "Dattes"
      produit_code == 81 ~ 111, # "Noix de coco"
      produit_code == 82 ~ 112, # "Canne à sucre"
      produit_code == 83 ~ 113, # "Pommes"
      produit_code == 84 ~ 114, # "Papaye"
      produit_code == 176 ~ 115, # "Goyave"
      produit_code == 85 ~ 116, # "Fruit de baobab"
      produit_code == 86 ~ 117, # "Néré"
      produit_code == 87 ~ 118, # "Autres fruits (raisin, fraise, poire, tamarin noir, liane sauvage, pomme sauvage, etc.)
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

autres_agrumes <- dplyr::filter(fruits_df, produit_code == 75)
madarine <- dplyr::mutate(autres_agrumes, produit_code = 104)
pamplemousse <- dplyr::mutate(autres_agrumes, produit_code = 105)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

fruits_df_ehcvm <- fruits_renum_df |>
  dplyr::bind_rows(
    # autres agrumes
    madarine, pamplemousse
  )

fruits_in_ehcvm3 <- fruits_df_ehcvm |>
  dplyr::distinct(produit_code) |>
  dplyr::pull(produit_code)

fruits_not_in_ehcvm3 <- fruits_ehcvm3[!fruits_ehcvm3 %in% fruits_in_ehcvm3]

fruits_missing <- fruits_df_ehcvm |>
  tidyr::expand(produit_code = fruits_not_in_ehcvm3)

fruits_df_ehcvm3 <- fruits_df_ehcvm |>
  dplyr::full_join(fruits_missing, by = "produit_code")

# ------------------------------------------------------------------------------
# légumes
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# renuméroter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

legumes_lbls <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = "s07Bq02_legumes"
)

legumes_df <- tableaux_df |>
  dplyr::filter(produit_code %in% legumes_lbls)

legumes_renum_indices <- c(
  88, # Salade (laitue)
  89, # Choux
  90, # Carotte
  91, # Haricot vert
  92, # Concombre
  93, # Aubergine
  95, # Poivron frais
  96, # Tomate fraîche
  97, # Tomate séchée
  98, # Gombo frais
  99, # Gombo sec
  100, # Oignon frais
  177, # Champignon frais
  101, # Ail
  102, # Feuilles locales 1
  103, # Feuilles locales 2
  104, # Feuilles locales 3
  105, # Feuilles locales 4
  106, # Autres légumes en feuilles
  107, # Autre légumes frais n.d.a.
  108 # Concentré de tomate
)

legumes_renum_df <- legumes_df |>
  dplyr::filter(produit_code %in% legumes_renum_indices) |>
  dplyr::mutate(
    produit_code = dplyr::case_when(
      produit_code == 88 ~ 119, # Salade (laitue)
      produit_code == 89 ~ 120, # Choux
      produit_code == 90 ~ 121, # Carotte
      produit_code == 91 ~ 122, # Haricot vert
      produit_code == 92 ~ 123, # Concombre
      produit_code == 93 ~ 124, # Aubergine
      produit_code == 95 ~ 127, # Poivron frais
      produit_code == 96 ~ 128, # Tomate fraîche
      produit_code == 97 ~ 129, # Tomate séchée
      produit_code == 98 ~ 130, # Gombo frais
      produit_code == 99 ~ 131, # Gombo sec
      produit_code == 100 ~ 132, # Oignon frais
      produit_code == 177 ~ 133, # Champignon frais
      produit_code == 101 ~ 134, # Ail
      produit_code == 102 ~ 135, # Feuilles locales 1
      produit_code == 103 ~ 136, # Feuilles locales 2
      produit_code == 104 ~ 137, # Feuilles locales 3
      produit_code == 105 ~ 138, # Feuilles locales 4
      produit_code == 106 ~ 139, # Autres légumes en feuilles
      produit_code == 107 ~ 140, # Autre légumes frais n.d.a.
      produit_code == 108 ~ 141, # Concentré de tomate
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# courge/courgette -> courge, courgette
courge_courgette <- dplyr::filter(legumes_df, produit_code == 94)
courge <- dplyr::mutate(courge_courgette, produit_code = 125)
courgette <- dplyr::mutate(courge_courgette, produit_code = 126)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

legumes_df_echvm <- legumes_renum_df |>
  dplyr::bind_rows(
    # courge/courgette
    courge, courgette
  )

legumes_in_ehcvm3 <- legumes_df_echvm |>
  dplyr::distinct(produit_code) |>
  dplyr::pull(produit_code)

legumes_not_in_ehcvm3 <- legumes_ehcvm3[!legumes_ehcvm3 %in% legumes_in_ehcvm3]

legumes_missing <- legumes_df_echvm |>
  tidyr::expand(produit_code = legumes_not_in_ehcvm3)

legumes_df_ehcvm3 <- legumes_df_echvm |>
  dplyr::full_join(legumes_missing, by = "produit_code")


# ------------------------------------------------------------------------------
# légumineuses et tubercules
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# renuméroter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

leg_tub_lbls <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = "s07Bq02_legtub"
)

leg_tub_df <- tableaux_df |>
  dplyr::filter(produit_code %in% leg_tub_lbls)

leg_tub_renum_indices <- c(
  109, # Petit pois frais
  110, # Petit pois secs
  111, # Autres légumes secs n.d.a.
  112, # Niébé/Haricots secs
  113, # Arachides fraîches en coques
  114, # Arachides séchées en coques
  115, # Arachides décortiquées
  116, # Arachides pilées
  117, # Arachide grillée
  118, # Pâte d'arachide
  119, # Fromage à base de soja      
  120, # Sésame
  121, # Noix de cajou
  122, # Noix de karité
  123, # Manioc
  124, # Igname
  125, # Plantain
  126, # Pomme de terre
  128, # Patate douce
  129, # Autres tubercules n.d.a.    
  130, # Farines de manioc
  178, # Pâte de manioc
  131, # Gari, tapioca
  132, # Attiéké
  133 # Fruit de Kapokier
)

leg_tub_renum_df <- leg_tub_df |>
  dplyr::filter(produit_code %in% leg_tub_renum_indices) |>
  dplyr::mutate(
    produit_code = dplyr::case_when(
      produit_code == 109 ~ 142, # Petit pois frais
      produit_code == 110 ~ 143, # Petit pois secs
      produit_code == 111 ~ 144, # Autres légumes secs n.d.a.
      produit_code == 112 ~ 145, # Niébé/Haricots secs
      produit_code == 113 ~ 148, # Arachides fraîches en coques
      produit_code == 114 ~ 149, # Arachides séchées en coques
      produit_code == 115 ~ 150, # Arachides décortiquées
      produit_code == 116 ~ 151, # Arachides pilées
      produit_code == 117 ~ 152, # Arachide grillée
      produit_code == 118 ~ 153, # Pâte d'arachide
      produit_code == 119 ~ 154, # Fromage à base de soja      
      produit_code == 120 ~ 155, # Sésame
      produit_code == 121 ~ 156, # Noix de cajou
      produit_code == 122 ~ 157, # Noix de karité
      produit_code == 123 ~ 158, # Manioc
      produit_code == 124 ~ 159, # Igname
      produit_code == 125 ~ 160, # Plantain
      produit_code == 126 ~ 162, # Pomme de terre
      produit_code == 128 ~ 161, # Patate douce
      produit_code == 129 ~ 165, # Autres tubercules n.d.a.    
      produit_code == 130 ~ 166, # Farines de manioc
      produit_code == 178 ~ 167, # Pâte de manioc
      produit_code == 131 ~ 168, # Gari, tapioca
      produit_code == 132 ~ 169, # Attiéké
      produit_code == 133 ~ 171, # Fruit de Kapokier
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# taro/macabo -> taro, macabo
taro_macabo <- dplyr::filter(leg_tub_df, produit_code == 127)
taro <- dplyr::mutate(taro_macabo, produit_code = 163)
macabo <- dplyr::mutate(taro_macabo, produit_code = 164)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

leg_tub_df_echvm <- leg_tub_renum_df |>
  dplyr::bind_rows(
    # taro/macabo
    taro, macabo
  )

leg_tub_in_ehcvm3 <- leg_tub_df_echvm |>
  dplyr::distinct(produit_code) |>
  dplyr::pull(produit_code)

leg_tub_not_in_ehcvm3 <- leg_tub_ehcvm3[!leg_tub_ehcvm3 %in% leg_tub_in_ehcvm3]

leg_tub_missing <- leg_tub_df_echvm |>
  tidyr::expand(produit_code = leg_tub_not_in_ehcvm3)

leg_tub_df_ehcvm3 <- leg_tub_df_echvm |>
  dplyr::full_join(leg_tub_missing, by = "produit_code")


# ------------------------------------------------------------------------------
# sucreries
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# renuméroter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

sucreries_lbls <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = "s07Bq02_sucreries"
)

sucreries_df <- tableaux_df |>
  dplyr::filter(produit_code %in% sucreries_lbls)

sucreries_renum_indices <- c(
  134, # Sucre en poudre
  135, # Sucre en morceaux
  136, # Miel
  137, # Chocolat à croquer, pâte à tartiner
  138 # Caramel, bonbons, confiseries, etc.
)

sucreries_renum_df <- sucreries_df |>
  dplyr::filter(produit_code %in% sucreries_renum_indices) |>
  dplyr::mutate(
    produit_code = dplyr::case_when(
      produit_code == 134 ~ 172, # Sucre en poudre
      produit_code == 135 ~ 173, # Sucre en morceaux
      produit_code == 136 ~ 174, # Miel
      produit_code == 137 ~ 175, # Chocolat à croquer, pâte à tartiner
      produit_code == 138 ~ 176, # Caramel, bonbons, confiseries, etc.
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

sucreries_df_echvm <- sucreries_renum_df

sucreries_in_ehcvm3 <- sucreries_df_echvm |>
  dplyr::distinct(produit_code) |>
  dplyr::pull(produit_code)

sucreries_not_in_ehcvm3 <- sucreries_ehcvm3[!sucreries_ehcvm3 %in% sucreries_in_ehcvm3]

sucreries_missing <- sucreries_df_echvm |>
  tidyr::expand(produit_code = sucreries_not_in_ehcvm3)

sucreries_df_ehcvm3 <- sucreries_df_echvm |>
  dplyr::full_join(sucreries_missing, by = "produit_code")

# ------------------------------------------------------------------------------
# épices
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# renuméroter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

epices_lbls <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = "s07Bq02_epices"
)

epices_df <- tableaux_df |>
  dplyr::filter(produit_code %in% epices_lbls)

epices_renum_indices <- c(
  139, # Sel
  140, # Piment séché
  141, # Piment frais
  142, # Gingembre frais
  143, # Gingembre moulu
  144, # Cube alimentaire (Maggi, Jumbo, )
  145, # Arôme (Maggi, Jumbo, etc.)       
  146, # Soumbala (moutarde africaine)
  147, # Mayonnaise
  148, # Vinaigre de citron
  149, # Autres vinaigres
  150, # Moutarde
  151, # Poivre
  179, # Poisson séché en condiment
  152, # Autres condiments (poivre etc.)
  153, # Noix de cola
  154 # Autres produits alimentaires (noix de pomme sauvage)
)

epices_renum_df <- epices_df |>
  dplyr::filter(produit_code %in% epices_renum_indices) |>
  dplyr::mutate(
    produit_code = dplyr::case_when(
      produit_code == 139 ~ 177, # Sel
      produit_code == 140 ~ 178, # Piment séché
      produit_code == 141 ~ 179, # Piment frais
      produit_code == 142 ~ 180, # Gingembre frais
      produit_code == 143 ~ 181, # Gingembre moulu
      produit_code == 144 ~ 182, # Cube alimentaire (Maggi, Jumbo, )
      produit_code == 145 ~ 183, # Arôme (Maggi, Jumbo, etc.)       
      produit_code == 146 ~ 184, # Soumbala (moutarde africaine)
      produit_code == 147 ~ 185, # Mayonnaise
      produit_code == 148 ~ 186, # Vinaigre de citron
      produit_code == 149 ~ 187, # Autres vinaigres
      produit_code == 150 ~ 188, # Moutarde
      produit_code == 151 ~ 189, # Poivre
      produit_code == 179 ~ 190, # Poisson séché en condiment
      produit_code == 152 ~ 191, # Autres condiments (poivre etc.)
      produit_code == 153 ~ 192, # Noix de cola
      produit_code == 154 ~ 196, # Autres produits alimentaires (noix de pomme sauvage)
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# N/A

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

epices_df_echvm <- epices_renum_df

epices_in_ehcvm3 <- epices_df_echvm |>
  dplyr::distinct(produit_code) |>
  dplyr::pull(produit_code)

epices_not_in_ehcvm3 <- epices_ehcvm3[!epices_ehcvm3 %in% epices_in_ehcvm3]

epices_missing <- epices_df_echvm |>
  tidyr::expand(produit_code = epices_not_in_ehcvm3)

epices_df_ehcvm3 <- epices_df_echvm |>
  dplyr::full_join(epices_missing, by = "produit_code")


# ------------------------------------------------------------------------------
# boissons
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# renuméroter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

boissons_lbls <- construct_values_labels(
  qnr_df = qnr_ehcvm2,
  varnames = "s07Bq02_boissons"
)

boissons_df <- tableaux_df |>
  dplyr::filter(produit_code %in% boissons_lbls)

boissons_renum_indices <- c(
  158, # Chocolat en poudre
  159, # Autres tisanes et infusions n.d.a. (quinqueliba, citronnelle, etc.)
  161, # Eau minérale/ filtrée
  162, # Boissons gazeuses (Coca, Fanta, Vimto, Sprite, etc.)
  163, # Jus en poudre
  164, # Bières et vins traditionnels (dolo, vin de pa…      
  165 # Bières industrielles
)

boissons_renum_df <- boissons_df |>
  dplyr::filter(produit_code %in% boissons_renum_indices) |>
  dplyr::mutate(
    produit_code = dplyr::case_when(
      produit_code == 158 ~ 201, # Chocolat en poudre
      produit_code == 159 ~ 202, # Autres tisanes et infusions n.d.a. (quinqueliba, citronnelle, etc.)
      produit_code == 161 ~ 203, # Eau minérale/ filtrée
      produit_code == 162 ~ 206, # Boissons gazeuses (Coca, Fanta, Vimto, Sprite, etc.)
      produit_code == 163 ~ 208, # Jus en poudre
      produit_code == 164 ~ 209, # Bières et vins traditionnels (dolo, vin de pa…      
      produit_code == 165 ~ 210, # Bières industrielles
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

cafe <-  boissons_df |>
  dplyr::filter(produit_code %in% c(155, 156)) |>
  dplyr::distinct(produit_code, unite_code, taille_code, .keep_all = TRUE)
cafe_en_poudre_soluble <- dplyr::mutate(cafe, produit_code = 197)
cafe_moulu <- dplyr::mutate(cafe, produit_code = 198)

the <- dplyr::filter(boissons_df, produit_code %in% c(157, 180))
the_en_sachet <- dplyr::mutate(the, produit_code = 199)
the_en_feuille <- dplyr::mutate(the, produit_code = 200)

jus_de_fruits <- dplyr::filter(boissons_df, produit_code == 160)
jus_de_fruits_1 <- dplyr::mutate(jus_de_fruits, produit_code = 204)
jus_de_fruits_2 <- dplyr::mutate(jus_de_fruits, produit_code = 205)

boissons_energisantes <- boissons_df |>
  dplyr::filter(produit_code == 162) |>
  dplyr::mutate(produit_code = 207)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

boissons_df_echvm <- boissons_renum_df |>
  dplyr::bind_rows(
    # cafe
    cafe_en_poudre_soluble, cafe_moulu,
    # the
    the_en_sachet, the_en_feuille,
    # jus de fruit
    jus_de_fruits_1, jus_de_fruits_2,
    # boissons energisantes
    boissons_energisantes
  )

boissons_in_ehcvm3 <- boissons_df_echvm |>
  dplyr::distinct(produit_code) |>
  dplyr::pull(produit_code)

boissons_not_in_ehcvm3 <- boissons_ehcvm3[!boissons_ehcvm3 %in% boissons_in_ehcvm3]

boissons_missing <- boissons_df_echvm |>
  tidyr::expand(produit_code = boissons_not_in_ehcvm3)

boissons_df_ehcvm3 <- boissons_df_echvm |>
  dplyr::full_join(boissons_missing, by = "produit_code")

# ==============================================================================
# créer un fichier Excel
# ==============================================================================

# ------------------------------------------------------------------------------
# rassembler tout
# ------------------------------------------------------------------------------

lookup_df <- dplyr::bind_rows(
  cereales_df_ehcvm3, viandes_df_ehcvm3, poissons_df_ehcvm3, laitier_df_ehcvm3,
  huiles_df_ehcvm3, fruits_df_ehcvm3, legumes_df_ehcvm3, leg_tub_df_ehcvm3,
  sucreries_df_ehcvm3, epices_df_ehcvm3, boissons_df_ehcvm3
)

# ------------------------------------------------------------------------------
# étiquetter
# ------------------------------------------------------------------------------

lookup_df_labelled <- lookup_df |>
  labelled::set_value_labels(
    produit_code = produits_lbls_ehcvm3,
    unite_code = unit_val_lbls,
    taille_code = size_val_lbls
  )

# ------------------------------------------------------------------------------
# créer des colonnes string
# ------------------------------------------------------------------------------

lookup_df_character <- lookup_df_labelled |>
  dplyr::mutate(
    produit_texte = labelled::to_character(
      produit_code,
      levels = "labels",
      nolabel_to_na = TRUE
    ),
    unite_texte = labelled::to_character(
      unite_code,
      levels = "labels",
      nolabel_to_na = TRUE
    ),
    taille_texte = labelled::to_character(
      taille_code,
      levels = "labels",
      nolabel_to_na = TRUE
    )
  ) |>
	dplyr::arrange(produit_code, unite_code, taille_code) |>
	dplyr::select(
    # produit
    produit_code, produit_texte,
    # unité
    unite_code, unite_texte,
    # taille
    taille_code, taille_texte
  )

# ------------------------------------------------------------------------------
# sauvegarder en format Excel
# ------------------------------------------------------------------------------

writexl::write_xlsx(
  x = lookup_df_character,
  path = fs::path(here::here(), "data", "out", "tableau_de_ref_ehcvm3.xlsx"),
  col_names = TRUE
)

# ------------------------------------------------------------------------------
# créer un fichier ressource qui compiles les unités par type de produit
# ------------------------------------------------------------------------------

# construire le chemin du document
chemin_document <- here::here(
  "data", "out",
  "unites_par_groupe_de_produits.html"
)

# purger l'ancien document, s'il existe 
tryCatch(
  error = function(cnd) {
    cat("Le document qui compile les unités n'existe pas encore.")
  },
  fs::file_delete(chemin_document)
)

# créer le document
quarto::quarto_render(
  input = fs::path(here::here(), "inst", "unites_par_groupe_de_produits.qmd")
)

# déplacer le document vers le dossier de sortie
fs::file_move(
  path = fs::path(
    here::here(), "inst",
    "unites_par_groupe_de_produits.html"
  ),
  new_path = chemin_document
)
