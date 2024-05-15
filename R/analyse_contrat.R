# ---------------- #
# Analyse des contrats
# ---------------- #

library(tidyverse)

source("db.R")

# Import ------------------------------------------------------------------
# table contrat
tbl_contrat <-
  tbl(con, Id(schema = "dadeh", table = "ddadtcontrat"))

# table assure
tbl_assure <- tbl(con, "ddadtassure")

# table employeur assure
tbl_assure_employeur <- tbl(con, "ddadtemployeur_assure")

# arrêt de travail
tbl_arret_travail <- tbl(con, "ddadtarret_trav")

# penibilité
tbl_penibilite <- tbl(con, "ddadtpenibilite")

# Nettoyage ---------------------------------------------------------------
# jointure employeur assure pour récupérer l'identifiant de l'assure
# + variable BOETH par contrat et par assure (sur au moins un contrat)
tbl_contrat_join <- tbl_contrat |>
  left_join(tbl_assure_employeur, by = join_by(id_employeur_assure == id)) |>
  mutate(is_boeth_contrat = if_else(!is.na(statut_boeth), 1, 0)) |>
  group_by(id_assure) |>
  mutate(is_boeth_assure = max(is_boeth_contrat)) |>
  ungroup()

# table avec infos assures
tbl_contrat_join_demo <- tbl_contrat_join |>
  left_join(tbl_assure, by = join_by(id_assure == id)) |>
  # calcul de l'âge
  mutate(age = 2022 - as.numeric(substr(date_naissance, 5, 8)))

# jointure arret travail
tbl_arret_travail_join <- tbl_contrat_join |>
  left_join(tbl_arret_travail, by = join_by(id == id_contrat))

# jointure pénibilité
tbl_penibilite_join <- tbl_contrat_join |>
  left_join(tbl_penibilite,
            by = join_by(id_employeur_assure, num_contrat == numero_contrat))

# Indicateurs -------------------------------------------------------------
# on calcule différents indicateurs sur la qualité de l'emploi

## Démographie ------------------------------------------------------------
# sexe
df_sexe <- tbl_contrat_join_demo |>
  mutate(sexe_recode = substr(num_immatriculation, 1, 1)) |>
  distinct(id_assure, is_boeth_assure, sexe_recode) |>
  group_by(sexe_recode) |>
  summarise(
    n = n(),
    n_boeth_assure = sum(is_boeth_assure),
    .groups = "drop"
  ) |>
  collect()

# âge
df_age <- tbl_contrat_join_demo |>
  left_join(tbl_assure, by = join_by(id_assure == id)) |>
  distinct(id_assure, is_boeth_assure, age) |>
  group_by(age) |>
  summarise(
    n = n(),
    n_boeth_assure = sum(is_boeth_assure),
    .groups = "drop"
  ) |>
  collect()

## Nature du contrat ------------------------------------------------------
df_nature_contrat <- tbl_contrat_join |>
  group_by(nature_contrat) |>
  summarise(
    n_contrat = n(),
    n_contrat_boeth_assure = sum(is_boeth_assure),
    .groups = "drop"
  ) |>
  collect()

## Motif de rupture du contrat --------------------------------------------
# on filtre sur les CDI pour éviter la sur-représentation des fins de CDD et intérim
# et on enlève les contrats non terminés
df_motif_rupture_cdi <- tbl_contrat_join |>
  filter(nature_contrat == "01" & !is.na(motif_rupture)) |>
  group_by(motif_rupture) |>
  summarise(
    n_contrat = n(),
    n_contrat_boeth_assure = sum(is_boeth_assure),
    .groups = "drop"
  ) |>
  collect()

## Quotité de travail -----------------------------------------------------
df_tps_travail <- tbl_contrat_join |>
  group_by(modalite_temps) |>
  summarise(
    n_contrat = n(),
    n_contrat_boeth_assure = sum(is_boeth_assure),
    .groups = "drop"
  ) |>
  collect()

## PCS ---------------------------------------------------------------------
df_pcs <- tbl_contrat_join |>
  mutate(pcs = substr(pcs_ese, 1, 2)) |>
  group_by(pcs) |>
  summarise(
    n_contrat = n(),
    n_contrat_boeth_assure = sum(is_boeth_assure),
    .groups = "drop"
  ) |>
  collect()

## Jours couverts par un contrat ------------------------------------------
# on calcule la durée de chaque contrat
# on somme par individu
# puis on cape à 365
# et on calcul le nombre jours d'inactivité
tbl_contrat_join_2022 <- tbl_contrat_join_demo |>
  filter(year(date_fin_contrat) >= 2022 &
           year(date_debut_contrat) <= 2022) |>
  # sinon date_diff ?
  mutate(
    date_fin_contrat = if_else(
      year(date_fin_contrat) > 2022,
      as.Date("2022-12-31"),
      date_fin_contrat
    ),
    date_debut_contrat = if_else(
      year(date_debut_contrat) < 2022,
      as.Date("2022-01-01"),
      date_debut_contrat
    ),
    duree = difftime(date_debut_contrat, date_fin_contrat, units = "days"),
    duree = as.numeric(duree) + 1
  ) |>
  group_by(id_assure, is_boeth_assure, age) |>
  summarise(n_jours_couverts_contrats = sum(duree),
            .groups = "drop") |>
  mutate(
    n_jours_couverts_contrats = if_else(
      n_jours_couverts_contrats > 365,
      365,
      n_jours_couverts_contrats
    ),
    duree_inactivite = 365 - n_jours_couverts_contrats
  )

# on collect par age
df_age_jours_couverts <- tbl_contrat_join_2022 |>
  count(is_boeth_assure, age, duree_inactivite) |>
  collect() |>
  mutate(
    groupe_age = cut(age, breaks = c(0, 25, 50, 100)),
    duree_inactivite_d = cut(
      duree_inactivite,
      breaks = c(-1, 0, 0.25 * 365, 0.50 * 365, 0.75 * 365, 1000),
      right = TRUE
    )
  ) |>
  filter(!is.na(groupe_age))

## Passage CDD-CDI --------------------------------------------------------
# on regarde si il y a eu des CDD et des CDI pour un même individu
df_contrat_typ_diff_sur_boeth <- tbl_contrat_join |>
  mutate(
    ind_cdi = if_else(nature_contrat %in% c('01', ' 08', '09'), 1, 0),
    ind_cdd = if_else(nature_contrat %in% c('02', '03', '10') , 1, 0)
  ) |>
  group_by(id_assure, is_boeth_assure) |>
  summarise(
    ind_cdi = max(ind_cdi),
    ind_cdd = min(ind_cdd),
    .groups = "drop"
  ) |>
  count(ind_cdi, ind_cdd, is_boeth_assure) |>
  collect()

## Contrats sur BOETH ou non ----------------------------------------------
df_contrat_diff_sur_boeth <- tbl_contrat_join |>
  group_by(id_assure) |>
  summarise(
    is_boeth_max = max(is_boeth_contrat),
    is_boeth_min = min(is_boeth_contrat),
    .groups = "drop"
  ) |>
  count(is_boeth_max, is_boeth_min) |>
  collect()

## Arrêts de travail pour motif accident ----------------------------------
df_arret_travail <- tbl_arret_travail_join |>
  filter(motif_arret %in% c("06")) |>
  distinct(id_assure, is_boeth_assure) |>
  count(is_boeth_assure) |>
  collect()

## Pénibilité -------------------------------------------------------------
df_penibilite <- tbl_penibilite_join |>
  group_by(facteur_exposition) |>
  summarise(
    n_contrat = n(),
    n_contrat_boeth_assure = sum(is_boeth_assure),
    .groups = "drop"
  ) |>
  collect()

# Export ------------------------------------------------------------------
writexl::write_xlsx(
  list(
    "sexe" = df_sexe,
    "age" = df_age,
    "nature_contrat" = df_nature_contrat,
    "motif_rupture" = df_motif_rupture_cdi,
    "quotite" = df_tps_travail,
    "pcs" = df_pcs,
    "jours_travailles" = df_age_jours_couverts,
    "contrat_cdi_cdd" = df_contrat_typ_diff_sur_boeth,
    "chgmt_boeth" = df_contrat_diff_sur_boeth,
    "arret_travail" = df_arret_travail,
    "penibilite" = df_penibilite
  ),
  "data/sorties.xlsx"
)
