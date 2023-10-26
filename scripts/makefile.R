# makefile species distribution modelling

source(here::here("scripts", "boot.R"))

# sauvegarde des données biologiques utilisées par chaque modèle
source(here("scripts", "bio_datasets.R"))

# source(here("scripts", "FUN", "sdmOneAlgo"))     # sauvegarde en .shp et .tif
# source(here("scripts", "FUN", "sdmOneAlgo2"))    # sauvegarde en .tif

# Modèles d'adéquation environnementale ----     Pour les données....
source(here("scripts", "SDM01_global_copernicus.R")) # Copernicus locales et globales
source(here("scripts", "SDM02_local_sextant.R"))     # Sextant    locales (GLP et MTQ)
source(here("scripts", "SDM03_local_habitat.R"))     # GEBCO      locales (GLP et MTQ)

# Études des profondeurs des modèles ----
source(here("scripts", "violin_profondeurs_validation_modèles.R"))

# Compilation des modèles ----
source(here("scripts", "SDM04_compilation.R"))
# importation/visualisation des probabilités d'occurrences / présences-absences
source(here("scripts", "popa_import.R"))
source(here("scripts", "popa_plot.R"))

# Comparaison des valeurs de variable environnementales
# du modèle avec les observations
source(here("scripts", "violin_plots_validation_modèles.R"))
# à modifier pour prendre en compte toutes les cellules du modèle généré en pa,
# pas que celles issues des occurrences

# Projection futures : scénarios du GIEC ----
source(here("scripts", "giec_sst_slr.R"))
source(here("scripts", "data_environment_ipcc.R"))
# source(here("scripts", "biomod_model_link.R"))
source(here("scripts", "projection_ipcc.R"))
source(here("scripts", "projection_ssp126.R"))
