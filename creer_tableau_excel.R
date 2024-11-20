# ==============================================================================
# installer les packages requis
# ==============================================================================

# installer `{here}` si le package est absent
if (!base::require("here", quietly = TRUE)) {
  install.packages("here")
}
source(here::here("R", "01_install_requirements.R"))

# ==============================================================================
# ingérer les tableaux
# ==============================================================================

# faire l'inventaire des tableaux
chemin_tableaux <- fs::dir_ls(
  path = fs::path(here::here(), "01_entree", "ehcvm2"),
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

source(here::here("R", "02_manipulate_value_labels.R"))

# ------------------------------------------------------------------------------
# ECHVM2
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# ingest questionnaire JSON as a data frame
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

qnr_ehcvm2 <- fs::path(here::here(), "01_entree", "ehcvm2", "document.json") |>
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
# créer une base des produits à partir du questionnaire Excel de l'EHCVM3
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

chemin_qnr_excel <- fs::dir_ls(
  path = fs::path(here::here(), "01_entree", "ehcvm3"),
  type = "file",
  regexp = "(\\.xlsx|\\.xls|\\.xlsm)$"
)

if (length(chemin_qnr_excel) == 0) {
  stop(
    paste(
      "Aucun questionnaire EHCVM3 retrouvé.",
      "Le programme attend un questionnaire Excel avec extension `.xlsx` dans le répertoire `01_entree/ehcvm3/`",
      sep = "\\n"
    )
  )
} else if (length(chemin_qnr_excel) > 1) {
  stop(
    paste(
      "Plusieurs questionnaires EHCVM3 retrouvés.",
      "Veuillez supprimer les questionnaires exédentaires dans le répertoire `01_entree/ehcvm3/`",
      sep = "\\n"
    )

  )
}

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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer des vecteurs des produits, globalement et par groupe de produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

produits_lbls_ehcvm3 <- stats::setNames(
  object = produits$produit_code,
  nm = produits$produit_nom
)

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
# actualiser le tableaux de référence
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
  25 # gâteaux
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
      produit_code == 23 ~ 30, # Croissants -> viennoisseries
      produit_code == 24 ~ 31, # Biscuits
      produit_code == 25 ~ 32, # Gâteaux
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
vermicelles <- dplyr::mutate(pates_alimentaires, produit_code = 23)
autres_pates_alimentaires <- dplyr::mutate(pates_alimentaires, produit_code = 24)

# pain moderne -> plusiers pains spécifiques
pain_moderne <- dplyr::filter(cereales_df, produit_code == 21)
pain_moderne_1 <- dplyr::mutate(pain_moderne, produit_code = 25)
pain_moderne_2 <- dplyr::mutate(pain_moderne, produit_code = 26)

# pain traditionnel -> plusiers pains spécifiques
pain_traditionnel <- dplyr::filter(cereales_df, produit_code == 22)
pain_traditionnel_1 <- dplyr::mutate(pain_traditionnel, produit_code = 27)
pain_traditionnel_2 <- dplyr::mutate(pain_traditionnel, produit_code = 28)

# beignets/galettes -> [beignets, ...] , galettes
beignets_galettes <- dplyr::filter(cereales_df, produit_code == 26)
beignet_boulangerie <- dplyr::mutate(beignets_galettes, produit_code = 33)
beignet_tradit <- dplyr::mutate(beignets_galettes, produit_code = 34)
beignet_mais_mil <- dplyr::mutate(beignets_galettes, produit_code = 35)
galettes <- dplyr::mutate(beignets_galettes, produit_code = 36)

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
    macaroni, spaghettis, vermicelles, autres_pates_alimentaires,
    # pains
    pain_moderne_1, pain_moderne_2,
    pain_traditionnel_1, pain_traditionnel_2,
    # beignets/galettes
    beignet_boulangerie, beignet_tradit, beignet_mais_mil, galettes,
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
  28, # Viande de chameau
  29, # Viande de mouton
  30, # Viande de chèvre
  32, # Viande de porc
  33, # Poulet sur pied
  171, # Autre volaille sur pied
  35, # Viande d'autres volailles domestiques  
  37, # Viande séchée (boeuf, mouton, chameau) 
  39 # Autres viandes n.d.a.
)

viandes_renum_df <- viandes_df |>
  dplyr::filter(produit_code %in% viandes_renum_indices) |>
  dplyr::mutate(
    produit_code = dplyr::case_when(
      produit_code == 28 ~ 44, # Viande de chameau
      produit_code == 29 ~ 42, # Viande de mouton
      produit_code == 30 ~ 43, # Viande de chèvre
      produit_code == 32 ~ 45, # Viande de porc
      produit_code == 33 ~ 60, # Poulet sur pied
      produit_code == 171 ~ 62, # Autre volaille sur pied
      produit_code == 35 ~ 59, # Viande d'autres volailles domestiques
      produit_code == 37 ~ 64, # Viande séchée (boeuf, mouton, chameau)
      produit_code == 39 ~ 68, # Autres viandes n.d.a.
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# viande de boeuf -> viande de boeuf par type de pièce
boeuf <- dplyr::filter(viandes_df, produit_code == 27)
boeuf_filet <- dplyr::mutate(boeuf, produit_code = 37)
boeuf_cote <- dplyr::mutate(boeuf, produit_code = 38)
boeuf_cote <- dplyr::mutate(boeuf, produit_code = 38)
boeuf_sans_os <- dplyr::mutate(boeuf, produit_code = 39)
boeuf_avec_os <- dplyr::mutate(boeuf, produit_code = 40)
boeuf_tripes <- dplyr::mutate(boeuf, produit_code = 41)

# viande de poulet -> viande de poulet par type de pièce
poulet <- dplyr::filter(viandes_df, produit_code == 34)
poulet_entier <- dplyr::mutate(viandes_df, produit_code = 55)
poulet_cuisses <- dplyr::mutate(viandes_df, produit_code = 56)
poulet_ailes <- dplyr::mutate(viandes_df, produit_code = 57)
poulet_autre_mourceaux <- dplyr::mutate(viandes_df, produit_code = 58)

# gibier -> plusieurs gibiers spécifiques
gibier <- dplyr::filter(viandes_df, produit_code == 38)
gros_gibier <- dplyr::mutate(gibier, produit_code = 50)
petits_gibier <- dplyr::mutate(gibier, produit_code = 51)
gibier_a_plumes <- dplyr::mutate(gibier, produit_code = 52)
autres_gibier <- dplyr::mutate(gibier, produit_code = 53)

# abats -> plusieurs abats spécifiques
abats <- dplyr::filter(viandes_df, produit_code == 31)
abats_volaille <- dplyr::mutate(abats, produit_code = 63)

# charcuterie -> plusieurs variétés
charcuterie <- dplyr::filter(viandes_df, produit_code == 36)
jambon <- dplyr::mutate(charcuterie, produit_code = 65)
saucisson <- dplyr::mutate(charcuterie, produit_code = 66)
conserves_viande <- dplyr::mutate(charcuterie, produit_code = 67)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

viandes_df_echvm <- viandes_renum_df |>
  dplyr::bind_rows(
    # viande de boeuf
    boeuf_filet, boeuf_cote, boeuf_cote, boeuf_sans_os, boeuf_avec_os, boeuf_tripes,
    # viande de poulet
    poulet_entier, poulet_cuisses, poulet_ailes, poulet_autre_mourceaux, 
    # gibier
    gros_gibier, petits_gibier, gibier_a_plumes, autres_gibier,
    # abats
    abats_volaille,
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
      produit_code == 40 ~ 69, # Poisson frais type 1
      produit_code == 41 ~ 70, # Poisson frais type 2
      produit_code == 42 ~ 71, # Poisson frais type 3
      produit_code == 43 ~ 72, # Poisson frais type 4
      produit_code == 44 ~ 73, # Poisson fumé type 1
      produit_code == 45 ~ 74, # Poisson fumé type 2
      produit_code == 172 ~ 75, # Poissons fumé type 3
      produit_code == 47 ~ 79, # Crabes
      produit_code == 48 ~ 80, # Crevettes/Gambas fraîches
      produit_code == 49 ~ 81, # Crevettes séchées
      produit_code == 173 ~ 83, # Escargots
      produit_code == 50 ~ 82, # Autres fruits de mer
      produit_code == 51 ~ 84, # Conserves de poisson
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# créer 3 postes de poisson séché
poisson_seche <- dplyr::filter(poissons_df, produit_code == 46)
poisson_seche_1 <- dplyr::mutate(poisson_seche, produit_code = 76)
poisson_seche_2 <- dplyr::mutate(poisson_seche, produit_code = 77)
poisson_seche_3 <- dplyr::mutate(poisson_seche, produit_code = 78)

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
      produit_code == 52 ~ 87, # Lait frais type 1
      produit_code == 174 ~ 88, # Lait frais type 2
      produit_code == 54 ~ 91, # Lait concentré sucré
      produit_code == 55 ~ 92, # Lait concentré non-sucré
      produit_code == 56 ~ 93, # Lait en poudre
      produit_code == 57 ~ 94, # Fromage
      produit_code == 59 ~ 97, # Autres produits laitiers
      produit_code == 60 ~ 98, # Œufs frais
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# lait caillé/yaourt -> lait caillé, yaourt
lait_caille_yaourt <- dplyr::filter(laitier_df, produit_code == 53)
lait_caille <- dplyr::mutate(laitier_df, produit_code = 89)
lait_yaourt <- dplyr::mutate(laitier_df, produit_code = 90)

# lait et farines pour bébé -> lait pour bébé, farines pour bébé
lait_farine <- dplyr::filter(laitier_df, produit_code == 58)
lait_bebe <- dplyr::mutate(lait_farine, produit_code = 95)
farine_bebe <- dplyr::mutate(lait_farine, produit_code = 96)

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
      produit_code == 61 ~ 99, # Beurre
      produit_code == 62 ~ 100, # Beurre de karité
      produit_code == 63 ~ 101, # Huile de palme brute
      produit_code == 64 ~ 103, # Huile d'arachide raffinée
      produit_code == 65 ~ 104, # Huile d'arachide 'Segal'
      produit_code == 66 ~ 105, # Huile de soja
      produit_code == 67 ~ 106, # Huile de coton
      produit_code == 68 ~ 102, # Huile de palme raffinée
      produit_code == 175 ~ 107, # Huile de karité
      produit_code == 69 ~ 108, # Noix de palme
      produit_code == 70 ~ 109, # Autres huiles alimentaires n.d.a. (maïs, pal…   
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
  86 # "Néré"
)

fruits_renum_df <- fruits_df |>
  dplyr::filter(produit_code %in% fruits_renum_indices) |>
  dplyr::mutate(
    produit_code = dplyr::case_when(
      produit_code == 71 ~ 110, # Mangue
      produit_code == 72 ~ 111, # Ananas
      produit_code == 73 ~ 112, # Orange
      produit_code == 74 ~ 113, # Citron
      produit_code == 76 ~ 116, # Banane douce
      produit_code == 77 ~ 117, # Avocat
      produit_code == 78 ~ 118, # Pastèque
      produit_code == 79 ~ 119, # Melon
      produit_code == 80 ~ 120, # Dattes
      produit_code == 81 ~ 121, # Noix de coco
      produit_code == 82 ~ 122, # Canne à sucre
      produit_code == 83 ~ 123, # Pommes
      produit_code == 84 ~ 124, # Papaye
      produit_code == 176 ~ 125, # Goyave
      produit_code == 85 ~ 126, # Fruit de baobab
      produit_code == 86 ~ 127, # Néré
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# autre agrumes -> mandarine, pamplemouse
autres_agrumes <- dplyr::filter(fruits_df, produit_code == 75)
madarine <- dplyr::mutate(autres_agrumes, produit_code = 114)
pamplemousse <- dplyr::mutate(autres_agrumes, produit_code = 115)

# autre fruits -> distinction entre fruits locaux et importés
autres_fruits <- dplyr::filter(fruits_df, produit_code == 87)
autre_fruits_loc <- dplyr::mutate(autres_fruits, produit_code = 129)
autre_fruits_imp <- dplyr::mutate(autres_fruits, produit_code = 130)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

fruits_df_ehcvm <- fruits_renum_df |>
  dplyr::bind_rows(
    # autres agrumes
    madarine, pamplemousse,
    # autre fruits
    autre_fruits_loc, autre_fruits_imp
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
      produit_code == 88 ~ 131, # Salade (laitue)
      produit_code == 89 ~ 132, # Choux
      produit_code == 90 ~ 133, # Carotte
      produit_code == 91 ~ 134, # Haricot vert
      produit_code == 92 ~ 135, # Concombre
      produit_code == 93 ~ 136, # Aubergine
      produit_code == 95 ~ 140, # Poivron frais
      produit_code == 96 ~ 141, # Tomate fraîche
      produit_code == 97 ~ 142, # Tomate séchée
      produit_code == 98 ~ 143, # Gombo frais
      produit_code == 99 ~ 144, # Gombo sec
      produit_code == 100 ~ 145, # Oignon frais
      produit_code == 177 ~ 146, # Champignon frais
      produit_code == 101 ~ 147, # Ail
      produit_code == 102 ~ 148, # Feuilles locales 1
      produit_code == 103 ~ 149, # Feuilles locales 2
      produit_code == 104 ~ 150, # Feuilles locales 3
      produit_code == 105 ~ 151, # Feuilles locales 4
      produit_code == 106 ~ 152, # Autres légumes en feuilles
      produit_code == 107 ~ 153, # Autre légumes frais n.d.a.
      produit_code == 108 ~ 154, # Concentré de tomate
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# courge/courgette -> courge, courgette
courge_courgette <- dplyr::filter(legumes_df, produit_code == 94)
courge <- dplyr::mutate(courge_courgette, produit_code = 138)
courgette <- dplyr::mutate(courge_courgette, produit_code = 139)

# aubergine -> aubergine, aubergine sauvage
aubergine_sauvage <- dplyr::filter(legumes_df, produit_code == 93) |>
	dplyr::mutate(produit_code = 137)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

legumes_df_echvm <- legumes_renum_df |>
  dplyr::bind_rows(
    # courge/courgette
    courge, courgette,
    # aubergine
    aubergine_sauvage
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
      produit_code == 109 ~ 155, # Petits pois frais
      produit_code == 110 ~ 156, # Petit pois secs
      produit_code == 111 ~ 158, # Autres légumes secs n.d.a.
      produit_code == 112 ~ 159, # Niébé/Haricots secs
      produit_code == 113 ~ 162, # Arachides fraîches en coques
      produit_code == 114 ~ 163, # Arachides séchées en coques
      produit_code == 115 ~ 164, # Arachides décortiquées
      produit_code == 116 ~ 165, # Arachides pilées
      produit_code == 117 ~ 166, # Arachide grillée
      produit_code == 118 ~ 167, # Pâte d'arachide
      produit_code == 119 ~ 168, # Fromage à base de soja
      produit_code == 120 ~ 169, # Sésame
      produit_code == 121 ~ 170, # Noix de cajou
      produit_code == 122 ~ 171, # Noix de karité
      produit_code == 123 ~ 172, # Manioc
      produit_code == 124 ~ 173, # Igname
      produit_code == 125 ~ 174, # Plantain
      produit_code == 126 ~ 176, # Pomme de terre
      produit_code == 128 ~ 175, # Patate douce
      produit_code == 129 ~ 179, # Autres tubercules n.d.a.
      produit_code == 130 ~ 180, # Farines de manioc
      produit_code == 178 ~ 181, # Pâte de manioc
      produit_code == 131 ~ 182, # Gari, tapioca
      produit_code == 132 ~ 183, # Attiéké
      produit_code == 133 ~ 185, # Fruit de Kapokier
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# taro/macabo -> taro, macabo
taro_macabo <- dplyr::filter(leg_tub_df, produit_code == 127)
taro <- dplyr::mutate(taro_macabo, produit_code = 177)
macabo <- dplyr::mutate(taro_macabo, produit_code = 178)

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
      produit_code == 134 ~ 186, # Sucre en poudre
      produit_code == 135 ~ 187, # Sucre en morceaux
      produit_code == 136 ~ 188, # Miel
      produit_code == 137 ~ 189, # Chocolat à croquer, pâte à tartiner
      produit_code == 138 ~ 190, # Caramel, bonbons, confiseries, etc.
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
      produit_code == 139 ~ 191, # Sel
      produit_code == 140 ~ 192, # Piment séché
      produit_code == 141 ~ 193, # Piment frais
      produit_code == 142 ~ 194, # Gingembre frais
      produit_code == 143 ~ 195, # Gingembre moulu
      produit_code == 144 ~ 196, # Cube alimentaire (Maggi, Jumbo, )
      produit_code == 145 ~ 197, # Arôme (Maggi, Jumbo, etc.)
      produit_code == 146 ~ 198, # Soumbala (moutarde africaine)
      produit_code == 147 ~ 199, # Mayonnaise
      produit_code == 148 ~ 202, # Vinaigre de citron
      produit_code == 149 ~ 203, # Autres vinaigres
      produit_code == 150 ~ 204, # Moutarde
      produit_code == 151 ~ 205, # Poivre
      produit_code == 179 ~ 206, # Poisson séché en condiment
      produit_code == 152 ~ 207, # Autres condiments (poivre etc.)
      produit_code == 153 ~ 208, # Noix de cola
      produit_code == 154 ~ 213, # Autres produits alimentaires (noix de pomme s…  
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

autre_vinaigres <- dplyr::filter(epices_df, produit_code == 149)
vinaigre_vin <- dplyr::mutate(autre_vinaigres, produit_code = 200)
vinaigre_alcool <- dplyr::mutate(autre_vinaigres, produit_code = 201)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# rassembler le tout
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

epices_df_echvm <- epices_renum_df |>
	dplyr::bind_rows(
    # viagres
    vinaigre_vin, vinaigre_alcool
  )

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
      produit_code == 158 ~ 218, # Chocolat en poudre
      produit_code == 159 ~ 219, # Autres tisanes et infusions n.d.a. (quinquel…
      produit_code == 161 ~ 220, # Eau minérale/ filtrée
      produit_code == 162 ~ 223, # Boissons gazeuses (Coca, Fanta, Vimto, Sprit…     
      produit_code == 163 ~ 225, # Jus en poudre
      produit_code == 164 ~ 226, # Bières et vins traditionnels (dolo, vin de pa…     
      produit_code == 165 ~ 227, # Bières industrielles
      TRUE ~ produit_code
    )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# créer de nouveaux produits
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

cafe <-  boissons_df |>
  dplyr::filter(produit_code %in% c(155, 156)) |>
  dplyr::distinct(produit_code, unite_code, taille_code, .keep_all = TRUE)
cafe_en_poudre_soluble <- dplyr::mutate(cafe, produit_code = 214)
cafe_moulu <- dplyr::mutate(cafe, produit_code = 215)

the <- dplyr::filter(boissons_df, produit_code %in% c(157, 180))
the_en_sachet <- dplyr::mutate(the, produit_code = 216)
the_en_feuille <- dplyr::mutate(the, produit_code = 217)

jus_de_fruits <- dplyr::filter(boissons_df, produit_code == 160)
jus_de_fruits_1 <- dplyr::mutate(jus_de_fruits, produit_code = 221)
jus_de_fruits_2 <- dplyr::mutate(jus_de_fruits, produit_code = 222)

boissons_energisantes <- boissons_df |>
  dplyr::filter(produit_code == 162) |>
  dplyr::mutate(produit_code = 224)

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
# sauvegarder une forme du tableau de référence en format Excel
# ------------------------------------------------------------------------------

writexl::write_xlsx(
  x = lookup_df_character,
  path = fs::path(here::here(), "02_sortie", "tableau_de_ref_ehcvm3.xlsx"),
  col_names = TRUE
)

# ------------------------------------------------------------------------------
# créer un fichier ressource qui compiles les unités par type de produit
# ------------------------------------------------------------------------------

# construire le chemin du document
chemin_document <- here::here("02_sortie", "unites_par_groupe_de_produits.html")

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
