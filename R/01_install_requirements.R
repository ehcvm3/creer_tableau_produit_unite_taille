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
  "writexl", # écrire un fichier Excel
  "gt" # créer des tableaux
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

