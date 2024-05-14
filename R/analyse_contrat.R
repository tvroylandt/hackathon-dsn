# ---------------- #
# Analyse des contrats
# ---------------- #

library(tidyverse)

source("db.R")

# Import ------------------------------------------------------------------
# table contrat
tbl_contrat <- tbl(con, I("dadeh.ddadtcontrat"))

# table employeur assure
tbl_assure_employeur <- tbl(con, "ddadtemployeur_assure")


# Nettoyage ---------------------------------------------------------------
# jointure employeur assure
tbl_join <- tbl_contrat |> 
  left_join(tbl_assure_employeur, by = join_by(id_employeur_assure == id))

# filtre BOETH
tbl_join_boeth <- tbl_join |> 
  filter(!is.na(statut_boeth))

# Calcul ------------------------------------------------------------------

df_boeth <- tbl_join |> 
  filter(!is.na(statut_boeth)) |> 
  count() |> 
  # distinct(id_assure) |> 
  # count(statut_boeth) |> 
  collect()

