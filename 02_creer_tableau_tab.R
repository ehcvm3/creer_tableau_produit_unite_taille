# ==============================================================================
# Ingérer les infos du questionnaire Excel
# ==============================================================================

# ------------------------------------------------------------------------------
# confirmer l'existence du questionnaire
# ------------------------------------------------------------------------------

chemin_qnr_excel <- fs::dir_ls(
  path = fs::path(here::here(), "01_entree", "ehcvm3"),
  type = "file",
  regexp = "(\\.xlsx|\\.xls|\\.xlsm)$"
)

if (length(chemin_qnr_excel) == 0) {

  cli::cli_abort(
    message = c(
      "x" = "Aucun questionnaire EHCVM3 retrouvé.",
      "i" = "Le programme attend un questionnaire Excel avec extension `.xlsx` dans le répertoire `01_entree/ehcvm3/`",
      "i" = "Veuillez copier un exemplaire adapté du questionnaire dans ce dossier."
    )
  )

} else if (length(chemin_qnr_excel) > 1) {

  cli::cli_abort(
    message = c(
      "x" = "Plusieurs questionnaires EHCVM3 retrouvés.",
      "i" = "Veuillez supprimer les questionnaires exédentaires dans le répertoire `01_entree/ehcvm3/`",
      chemin_qnr_excel
    )
  )

}

# ------------------------------------------------------------------------------
# ingérer
# ------------------------------------------------------------------------------

produits <- chemin_qnr_excel |>
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

# ------------------------------------------------------------------------------
# extaire des métadonnées utiles pour la validation et la création du tableau
# ------------------------------------------------------------------------------

source(here::here("R", "02_manipulate_value_labels.R"))

cereales_ehcvm3 <- get_products_for_food_group(
  product_df = produits,
  food_group = "CÉRÉALES ET PAINS"
)

viandes_ehcvm3 <- get_products_for_food_group(
  product_df = produits,
  food_group = "VIANDE"
)

poissons_ehcvm3 <- get_products_for_food_group(
  product_df = produits,
  food_group = "POISSON ET FRUITS DE MER"
)

laitier_ehcvm3 <- get_products_for_food_group(
  product_df = produits,
  food_group = "LAIT, FROMAGE ET OEUFS"
)

huiles_ehcvm3 <- get_products_for_food_group(
  product_df = produits,
  food_group = "HUILES ET GRAISSES"
)

fruits_ehcvm3 <- get_products_for_food_group(
  product_df = produits,
  food_group = "FRUITS"
)

legumes_ehcvm3 <- get_products_for_food_group(
  product_df = produits,
  food_group = "LÉGUMES"
)

leg_tub_ehcvm3 <- get_products_for_food_group(
  product_df = produits,
  food_group = "LEGUMINEUSES ET TUBERCULES"
)

sucreries_ehcvm3 <- get_products_for_food_group(
  product_df = produits,
  food_group = "SUCRE, MIEL, CHOCOLAT ET CONFISERIE"
)

epices_ehcvm3 <- get_products_for_food_group(
  product_df = produits,
  food_group = "EPICES, CONDIMENTS ET AUTRES"
)

boissons_ehcvm3 <- get_products_for_food_group(
  product_df = produits,
  food_group = "BOISSONS"
)

# ==============================================================================
# Valilder le contenu du fichier
# ==============================================================================

# ------------------------------------------------------------------------------
# ingérer le tableau
# ------------------------------------------------------------------------------

tableau_df <- readxl::read_excel(
  path = here::here("02_sortie", "tableau_de_ref_ehcvm3.xlsx")
)

# ------------------------------------------------------------------------------
# colonnes devrait être de type numérique
# ------------------------------------------------------------------------------

numeric_cols <- c("produit_code", "unite_code", "taille_code")

col_is_numeric <- function(col) {

  is_numeric <- class(tableau_df[[col]]) == "numeric"

  return(is_numeric)

}

if (
  !col_is_numeric("produit_code") ||
  !col_is_numeric("unite_code") ||
  !col_is_numeric("taille_code")
) {

  cols_non_num <- tableau_df |>
    dplyr::filter(
      dplyr::if_any(
        .cols = c("produit_code", "unite_code", "taille_code"),
        .fns = ~ grepl(
          x = .x,
          pattern = "[A-Za-z]"
        )
      )
    )
  
  print(cols_non_num)

  cli::cli_abort(
    message = c(
      "x" = "Contenu non-numérique retrouvé dans les colonnes de code.",
      "i" = "Voir les exemples dans le tableau ci-haut.",
      "i" = "Les codes doivent avoir un contenu strictement numérique."
    )
  )

}

# ------------------------------------------------------------------------------
# produit_code entre 1 et 227
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# produits absents
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# tous les groupes alimentaire ont au moins un produit
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# aucun contenu vide
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# produit_code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

if (any(is.na(tableau_df$produit_code))) {

  produit_code_vide_obs <- dplyr::filter(tableau_df, is.na(produit_code))

  print(produit_code_vide_obs)

  cli::cli_abort(
    message = c(
      "x" = "Lignes identifiées sans le code de produit.",
      "i" = "Voir les exemples dans le tableau ci-haut.",
      "i" = "Toute ligne du tableau a besoin d'un code produit."
    )
  )
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# unite_code, taille_code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

if (any(is.na(tableau_df$unite_code)) || any(is.na(tableau_df$taille_code))) {

  unite_taille_vide_obs <- tableau_df |>
    dplyr::filter(is.na(unite_code) | is.na(taille_code))

  print(unite_taille_vide_obs)

  cli::cli_abort(
    message = c(
      "x" = "Lignes identifiées sans unité et/ou taille",
      "i" = "Voir les exemples dans le tableau ci-haut.",
      "i" = "Veuillez fournir une unité ou taille pour de telles lignes."
    )
  )

}

# produits exclus (avertissement)

# ------------------------------------------------------------------------------
# produits pas adaptés
# ------------------------------------------------------------------------------

# faire l'inventaire des produits à adapter par les INS
produits_a_adapter <- c(
  "Riz local type [1-2]",
  "Riz importé [1-3]",
  "Pain moderne type [1-2]",
  "Pain traditionnel type [1-2]",
  "Poisson frais type [0-9]{1,2}",
  "Poisson fumé type [1-8]",
  "Poisson séché ou salé type [1-4]",
  "Lait frais type [1-2]",
  "Feuille locale (fraîche ou séchée) [0-9]{1,2}",
  "Jus de fruits type [1-2]"
)

# transformer le vecteur en expression  régulière
produits_a_adapter_pattern <- glue::glue(
  "({glue::glue_collapse(x = produits_a_adapter, sep = '|')})"
)

if (any(grep(x = tableau_df$produit_texte, pattern = produits_a_adapter_pattern))) {

  produits_tjr_a_adapter <- tableau_df |>
    dplyr::filter(
      grepl(
        x = produit_texte,
        pattern = produits_a_adapter_pattern
      )
    ) |>
    dplyr::distinct(produit_texte) |>
    dplyr::pull(produit_texte)

  cli::cli_warn(
    message = c(
      "!" = "Produits non-adaptés au contexte pays identifiés",
      "i" = "Ces textes viennent du questionnaire fourni dans `01_entrees/`",
      produits_tjr_a_adapter
    )
  )

}

# ==============================================================================
# Créer des tableaux de référence délimités par tab
# ==============================================================================

# ------------------------------------------------------------------------------
# transforme le tableau
# ------------------------------------------------------------------------------

tableau_tab_df <- tableau_df |>
  dplyr::mutate(
    # ajouter la colonne rowcode obligatoire
    rowcode = dplyr::row_number(),
    # ajouter une colonne pour indiquer l'appartenance de group de produit
    groupe_code = dplyr::case_when(
      produit_code %in% cereales_ehcvm3 ~ 1,
      produit_code %in% viandes_ehcvm3 ~ 2,
      produit_code %in% poissons_ehcvm3 ~ 3,
      produit_code %in% laitier_ehcvm3 ~ 4,
      produit_code %in% huiles_ehcvm3 ~ 5,
      produit_code %in% fruits_ehcvm3 ~ 6,
      produit_code %in% legumes_ehcvm3 ~ 7,
      produit_code %in% leg_tub_ehcvm3 ~ 8,
      produit_code %in% sucreries_ehcvm3 ~ 9,
      produit_code %in% epices_ehcvm3 ~ 10,
      produit_code %in% boissons_ehcvm3 ~ 11,
      TRUE ~ 0
    )
  ) |>
  dplyr::select(rowcode, produit_code, unite_code, taille_code, groupe_code)

# ------------------------------------------------------------------------------
# sauvegarder un fichier tab
# ------------------------------------------------------------------------------

readr::write_tsv(
  x = tableau_tab_df,
  file = fs::path(here::here(), "02_sortie", "tableau_de_ref_ehcvm3.tab"),
  col_names = TRUE
)
