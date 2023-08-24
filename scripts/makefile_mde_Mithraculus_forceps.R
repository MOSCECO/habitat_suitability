# Makefile

# Modèle de distribution d'espèces

# Sélection de l'espèce ----
# bn <- "Claremontiella nodulosa"
superfm <- "Majoidea"
species <- "Mithraculus_forceps"

# Initialisation ----
source(here::here("scripts", "boot.R"))
# import des occurrences locales et globales
# import des climatologies locales
# copie et import des données environnementales au niveau global (copernicus)
# modification des données environnementales en une matrice
# génération des données environnementales issues des ACP
# séparation dans les climatologies locales des données fixe (habitat) et
# variables
# réduction de la colinéarité par vif (déletion de couches de climatologies)

# Modèle de distribution d'espèce ----
# Première approche (naïve) ----
# Deuxième approche (ACP) ----
# Troisième approche (échelles locale et globale, trois niveaux) ----
# Quatrième approche (hiérarchie des échelles) ----
