# ---------------- #
# Graphs
# ---------------- #

library(tidyverse)
library(gouvdown)

# Import ------------------------------------------------------------------
list_outputs <- map(
  readxl::excel_sheets("data/sorties.xlsx"),
  \(x)readxl::read_xlsx(path = "data/sorties.xlsx", sheet = x)
) |>
  set_names(readxl::excel_sheets("data/sorties.xlsx"))

# Graphs ------------------------------------------------------------------

## Radar ------------------------------------------------------------------

# nature contrat
nature <- list_outputs$nature_contrat |>
  pivot_longer(-nature_contrat) |>
  group_by(name) |>
  mutate(perc_nature = value / sum(value)) |>
  ungroup() |>
  filter(nature_contrat == "01")

# penibilité
penib <- list_outputs$penibilite |>
  group_by(facteur_exposition) |>
  summarise(
    n_contrat = sum(n_contrat),
    n_contrat_boeth_assure = sum(n_contrat_boeth_assure)
  ) |>
  ungroup() |>
  mutate(
    perc = round(n_contrat / sum(n_contrat), 4) * 100,
    perc_boeth = round(n_contrat_boeth_assure / sum(n_contrat_boeth_assure), 4) *
      100
  ) |>
  select(-c(n_contrat, n_contrat_boeth_assure)) |>
  pivot_longer(-facteur_exposition) |>
  filter(!is.na(facteur_exposition)) |>
  group_by(name) |>
  summarise(perc_penib = sum(value) / 100 *10) |>
  ungroup()

# tps partiel
quotite <- list_outputs$quotite |>
  filter(modalite_temps != "99") |>
  group_by(modalite_temps) |>
  summarise(across(c(n_contrat, n_contrat_boeth_assure), sum), .groups = "drop") |>
  pivot_longer(-modalite_temps) |>
  group_by(name) |>
  mutate(perc_quotite = 1 - value / sum(value)) |>
  ungroup() |>
  filter(modalite_temps == "10")

# arret travail
arret <- list_outputs$arret_travail |>
  mutate(is_boeth_assure = fct_recode(
    as.character(is_boeth_assure),
    "n" = "0",
    "n_boeth_assure" = "1"
  )) |>
  left_join(
    list_outputs$sexe |>
      summarise(n = sum(n),
                n_boeth_assure = sum(n_boeth_assure)) |>
      mutate(n = n - n_boeth_assure) |>
      pivot_longer(everything(), names_to = "is_boeth_assure")
  ) |>
  mutate(perc_arret = n / value) |>
  rename(name = is_boeth_assure)

# jours
jours <- list_outputs$jours_travailles |>
  group_by(is_boeth_assure, duree_inactivite) |>
  summarise(n = sum(n), .groups = "drop") |>
  group_by(is_boeth_assure) |>
  summarise(perc_jours = 1 - (weighted.mean(duree_inactivite, n) / 365)) |>
  ungroup()  |>
  mutate(name = fct_recode(
    as.character(is_boeth_assure),
    "n" = "0",
    "n_boeth_assure" = "1"
  ))

# assemblage
df_radar <- bind_rows(nature, arret, quotite, penib, jours) |>
  mutate(boeth = str_detect(name, "boeth")) |>
  select(boeth, starts_with("perc")) |>
  pivot_longer(-boeth) |>
  filter(!is.na(value)) |>
  mutate(value = value) |>
  pivot_wider(names_from = name, values_from = value) |>
  mutate(order = as.numeric(boeth) + 3)

df <- df_radar |>
  add_row(
    order = 2,
    perc_nature = 0,
    perc_arret = 0,
    perc_penib = 0,
    perc_quotite = 0,
    perc_jours = 0
  ) |>
  add_row(
    order = 1,
    perc_nature = 1,
    perc_arret = 1,
    perc_penib = 1,
    perc_quotite = 1,
    perc_jours = 1
  ) |>
  arrange(order) |>
  select(-boeth,-order) |>
  relocate(perc_arret, perc_nature, perc_jours, perc_penib, perc_quotite) |> 
  rename(
    `CDI parmi les contrats` = perc_nature,
    `Temps partiel` = perc_quotite,
    `Arrêt de travail` = perc_arret,
    `Pénibilité` = perc_penib,
    `Jours travaillés` = perc_jours
  ) 

# specif
df_radar |> 
  pivot_longer(-boeth) |> 
  pivot_wider(names_from = boeth, values_from = value) |> 
  mutate(specif = `TRUE`/`FALSE`)

# radar
library(fmsb)

create_beautiful_radarchart <- function(data,
                                        color = "#00AFBB",
                                        vlabels = colnames(data),
                                        vlcex = 0.7,
                                        caxislabels = NULL,
                                        title = NULL,
                                        ...) {
  radarchart(
    data,
    axistype = 1,
    # Personnaliser le polygone
    pcol = color,
    pfcol = scales::alpha(color, 0.2),
    plwd = 2,
    plty = 1,
    # Personnaliser la grille
    cglcol = "grey",
    cglty = 1,
    cglwd = 0.8,
    # Personnaliser l'axe
    axislabcol = "grey",
    # Étiquettes des variables
    vlcex = vlcex,
    vlabels = vlabels,
    caxislabels = caxislabels,
    title = title,
    ...
  )
}

op <- par(mar = c(1, 1, 1, 1))
# Créer les graphiques radar
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 25, 50, 75, 100),
  color = c("#E1000F", "#000091")
)
# Ajouter une légende horizontale
legend(
  x = "bottom", legend = c("Non BOETH", "BOETH"), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#E1000F", "#000091"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)


## Age --------------------------------------------------------------------
list_outputs$age |>
  pivot_longer(-age) |>
  filter(between(age, 18, 65)) |>
  mutate(
    name = fct_recode(name,
                      "Total" = "n",
                      "BOETH" = "n_boeth_assure"),
    name = fct_relevel(name, "BOETH", "Total")
  ) |>
  group_by(name) |>
  mutate(perc = value / sum(value)) |>
  ungroup() |>
  ggplot(aes(x = age, y = perc, color = name)) +
  geom_line(linewidth = 1.5) +
  scale_x_continuous(name = "Âge") +
  scale_y_continuous(name = "",
                     labels = scales::label_percent(accuracy = 1)) +
  scale_color_gouv_discrete() +
  ggtitle(label = "Âge des <span style='color:#000091;'>**BOETH** </span>vs <span style='color:#E1000F;'>total</span>") +
  guides(color = "none") +
  facet_wrap(vars(name)) +
  theme_gouv() +
  theme(title = ggtext::element_markdown(),
        strip.text = element_blank())

## Quotité ----------------------------------------------------------------
list_outputs$quotite |>
  filter(modalite_temps != "99") |>
  group_by(modalite_temps) |>
  summarise(across(c(n_contrat, n_contrat_boeth_assure), sum), .groups = "drop") |>
  pivot_longer(-modalite_temps) |>
  mutate(name = fct_recode(name,
                           "Total" = "n_contrat",
                           "BOETH" = "n_contrat_boeth_assure")) |>
  group_by(name) |>
  mutate(perc = value / sum(value)) |>
  ungroup() |>
  filter(modalite_temps == "10")

## PCS ---------------------------------------------------------------------
list_outputs$pcs |>
  mutate(
    perc = n_contrat / sum(n_contrat),
    perc_boeth = n_contrat_boeth_assure / sum(n_contrat_boeth_assure),
    specif = perc_boeth / perc
  ) |>
  arrange(-specif)

# Jours couverts ----------------------------------------------------------
list_outputs$jours_travailles |>
  group_by(is_boeth_assure, duree_inactivite_d, groupe_age) |>
  summarise(n = sum(n),
            .groups = "drop") |>
  mutate(
    is_boeth_assure = fct_recode(
      as.character(is_boeth_assure),
      "Hors BOETH" = "0",
      "BOETH" = "1"
    ),
    duree_inactivite_d = fct_recode(
      duree_inactivite_d,
      "Pas d'inactivité" = "(-1,0]",
      "1%-25% de jours sans contrat" = "(0,91.2]",
      "25%-50% de jours sans contrat" = "(91.2,182]",
      "50%-75% de jours sans contrat" = "(182,274]",
      "75%-100% des jours sans contrat" = "(274,1e+03]"
    ),
    duree_inactivite_d = fct_rev(
      fct_relevel(
        duree_inactivite_d,
        "Pas d'inactivité",
        "1%-25% de jours sans contrat",
        "25%-50% de jours sans contrat",
        "50%-75% de jours sans contrat",
        "75%-100% des jours sans contrat"
      )
    ),
    groupe_age = fct_recode(
      groupe_age,
      "25 ans ou moins" = "(0,25]",
      "26-50 ans" = "(25,50]",
      "Plus de 50 ans" = "(50,100]"
    ),
    groupe_age = fct_rev(groupe_age)
  ) |>
  group_by(is_boeth_assure, groupe_age) |>
  mutate(perc = n / sum(n)) |>
  ungroup() |>
  ggplot(aes(
    x = duree_inactivite_d,
    y = perc,
    fill = groupe_age,
    group = groupe_age
  )) +
  geom_col(position = position_dodge()) +
  geom_text(
    aes(label = scales::percent(perc, accuracy = 1)),
    position = position_dodge(.8),
    hjust = -.1,
    family = "Marianne",
    fontface = "bold"
  ) +
  scale_x_discrete(name = "") +
  scale_y_continuous(
    name = "",
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  scale_fill_gouv_discrete(name = "", palette = "pal_gouv_qual2") +
  coord_flip() +
  facet_wrap(vars(is_boeth_assure)) +
  ggtitle(label = "Durée d'inactivité en 2022 par groupe d'âge et BOETH") +
  theme_gouv()


## Pénibilité -------------------------------------------------------------
df_penibilite2 <- list_outputs$penibilite |>
  mutate(facteur_exposition = fct_other(facteur_exposition, keep = c("08", "09", "06", "10", "07", NA))) |>
  group_by(facteur_exposition) |>
  summarise(
    n_contrat = sum(n_contrat),
    n_contrat_boeth_assure = sum(n_contrat_boeth_assure)
  ) |>
  ungroup() |>
  mutate(
    perc = round(n_contrat / sum(n_contrat), 4) * 100,
    perc_boeth = round(n_contrat_boeth_assure / sum(n_contrat_boeth_assure), 4) *
      100
  ) |>
  select(-c(n_contrat, n_contrat_boeth_assure)) |>
  pivot_longer(-facteur_exposition)


df_penibilite2 |>
  filter(!is.na(facteur_exposition)) |>
  mutate(
    facteur_exposition = fct_recode(
      facteur_exposition,
      "Températures extrêmes" = "06",
      "Bruit" = "07",
      "Travail de nuit" = "08",
      "Travail en 3x8" = "09",
      "Travail répétitif" = "10",
      "Autres" = "Other"
    )
  ) |>
  pivot_wider(names_from = name, values_from = value) |>
  mutate(specif = perc_boeth / perc)


## Nature ------------------------------------------------------------------
df_nature_contrat2 <- list_outputs$nature_contrat %>%
  mutate(nature_contrat = fct_other(nature_contrat, keep = c("01", "02", "03"))) %>%
  group_by(nature_contrat) %>%
  summarise(
    n_contrat = sum(n_contrat),
    n_contrat_boeth_assure = sum(n_contrat_boeth_assure)
  ) %>%
  ungroup() %>%
  mutate(
    perc = round(n_contrat / sum(n_contrat), 4) * 100,
    perc_boeth = round(n_contrat_boeth_assure / sum(n_contrat_boeth_assure), 4) *
      100
  ) %>%
  select(-c(n_contrat, n_contrat_boeth_assure)) %>%
  pivot_longer(-nature_contrat)


df_nature_contrat2 %>%
  mutate(
    nature_contrat = fct_recode(
      nature_contrat,
      "CDI" = "01",
      "CDD" = "02",
      "Intérim" = "03",
      "Autres" = "Other"
    )
  ) %>%
  mutate(nature_contrat = fct_reorder(nature_contrat, desc(value))) %>%
  ggplot(., aes(x = nature_contrat, y = value)) +
  geom_col() +
  facet_wrap(~ name)
