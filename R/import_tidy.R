# Étude de l'obésité - Importation et remaniement des données
# Auteur : Baseka Manzabe Christelle
# Date : 28/02/2025
###############################################################################

# Packages utiles
SciViews::R(lang = "fr")

# Importation des données brutes

## Création des dossier `data/` et `cache/`
fs::dir_create("data/cache")

## Importation du dictionnaire des données
biometry_metadata <- read$csv(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ14kFDtlqxUqJpfKIcZRHA2i3ZnCwSdT_bqcx7BWp3hk_fqEGk9JmBvRsHvdBZrI3KCACV-LHQ-tAv/pub?gid=0&single=true&output=csv",
  cache_file = "data/cache/biometry_metadata_raw.csv",
  force = FALSE
)

## Importation du tableau de données
biometry <- read$csv(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vTTvgkHmrxPXHJ16cypzK7ooBhcSybscjBMc0obVSt1dvxlNL9rtage91lKD8Jec-3n4eX_6O-VdW7f/pub?gid=0&single=true&output=csv",
  cache_file = "data/cache/biometry_raw.csv",
  force = FALSE
)

# Exploration des données

skimr::skim(biometry)

# Modification des types de variables des données

unique(biometry$genre)
biometry$genre <- factor(biometry$genre, levels = c("H", "F"))

unique(biometry$alimentation)
biometry$alimentation <- factor(biometry$alimentation,
  levels = c("omnivore", "carnivore", "végétarien"))

unique(biometry$intolerance_lactose)
biometry$intolerance_lactose <- factor(biometry$intolerance_lactose,
  levels = c("N", "O"))

unique(biometry$intolerance_gluten)
biometry$intolerance_gluten <- factor(biometry$intolerance_gluten,
  levels = c("N", "O"))

unique(biometry$sucre)
# Correction de quelques niveaux
biometry$sucre[biometry$sucre == "souveny"] <- "souvent"
biometry$sucre[biometry$sucre == "régulierement"] <- "régulièrement"
# Transformation en facteur ordonné
biometry$sucre <- ordered(biometry$sucre,
  levels = c("jamais", "rarement", "régulièrement", "souvent"))

unique(biometry$cortisone)
biometry$cortisone <- factor(biometry$cortisone,
  levels = c("N", "O"))

# Correction, filtre, sélection sur le tableau des données

biometry %>.%
  smutate(., 
    # Calcul de l'age des individu
    age = as.numeric(difftime(date_mesure, date_naissance, units = "days")/365.25), 
    # Calcul de la masse corrigée
    masse_corr = masse * (masse_std_ref/masse_std)) %>.%
    sdrop_na(., masse_corr) -> 
    biometry

# Ajout des labels et des unités
# TODO
biometry <- labelise (biometry, 
  label = list(id = "Identifiant", date_naissance = "Date de naissance", genre = "Genre", masse_std_ref = "Masse de référence de l'expérimentateur", masse_std = "Masse de l'expérimentateur", masse = "Masse", taille = "Taille", tour_poignet = "Tour de poignet", tour_taille ="Tour de taille", tour_hanche = "Tour de hanche", date_mesure = "Date de la mesure", activite_physique = "Activité physique", alimentation = "Régime alimentaire", alcool = "Consommation d'alcool", intolerance_lactose = "Intolerant au lactose", sommeil = "Sommeil", intolerance_gluten = "Intolerant au gluten", fast_food = "Fast-food", sucre = "Consommation de sucre", fumeur = "Fumeur", cortisone = "Cortisone", eau = "Consommation d'eau"), 
units = list(id = "NA", date_naissance = "NA", genre = "NA", masse_std_ref = "kg", masse_std = "kg", masse = "kg", taille = "cm", tour_poignet = "cm", tour_taille ="cm", tour_hanche = "cm", date_mesure = "NA", activite_physique = "min/sem", alimentation = "NA", alcool = "verre/sem", intolerance_lactose = "NA", sommeil = "h/j", intolerance_gluten = "NA", fast_food = "nbr/mois", sucre = "NA", fumeur = "nbr cigarettes/sem", cortisone = "NA", eau = "l/j"))


# Sauvegarde local des données importantes 
write$rds(biometry, "data/biometry.rds", compress = "xz")
write$rds(biometry_metadata, "data/biometry_metadata.rds", compress = "xz")

# Élimination des objets de l'environnement global
rm(biometry_metadata, biometry)

