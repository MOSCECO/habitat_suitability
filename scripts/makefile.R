# makefile species distribution modelling

source("scripts", "boot.R")

# sauvegarde des données biologiques utilisées par chaque modèle
source("scripts", "bio_datasets.R")

# Modèles pour les données copernicus globales et locales
source("scripts", "SDM01_global_copernicus.R")  # sauvegarde en .shp et .tif
source("scripts", "SDM01_global_copernicus2.R") # sauvegarde en .tif

# Modèles pour les données sextant locales (Guadeloupe et Martinique)
source("scripts", "SDM02_local_sextant.R")     # sauvegarde en .shp et .tif
source("scripts", "SDM02_local_sextant2.R")    # sauvegarde en .tif

# Modèles pour les données gebco locales
source("scripts", "SDM03_local_habitat.R")     # sauvegarde en .shp et .tif
source("scripts", "SDM03_local_habitat2.R")    # sauvegarde en .tif

# Plusieurs problèmes sur le cluster MNHN/meSU :
# codes spécifiques à plusieurs espèces
# (Stramonita rustica global : fait depuis l'ordi)
source("scripts", "SDM02_local_sextant_Cla.nod.R")
source("scripts", "SDM02_local_sextant_Ste.set.R")
source("scripts", "SDM02_local_sextant_Ste.set.R")

source("scripts", "SDM03_local_habitat_Cla.nod.R")
source("scripts", "SDM03_local_habitat_Ste.set.R")
source("scripts", "SDM03_local_habitat_Str.rus.R")

# Études des profondeurs des modèles ----
source("scripts", "violin_profondeurs_validation_modèles.R")

# Compilation des modèles ----
source("scripts", "SDM04_compilation.R")
# importation/visualisation des probabilités d'occurrences / présences-absences
source("scripts", "popa_import.R")
source("scripts", "popa_plot.R")

# Comparaison des valeurs de variable environnementales
# du modèle avec les observations
source("scripts", "violin_profondeurs_validation_modèles.R")
# à modifier pour prendre en compte toutes les cellules du modèle généré en pa,
# pas que celles issues des occurrences
