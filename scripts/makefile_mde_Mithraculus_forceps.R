# Makefile

# Modèle de distribution d'espèces

# Mithraculus forceps

# Copie des climatologies nécessaires pour le niveau global
cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/data_env_prep/data/tidy/clim_global/ /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/data/raw/clim_global/"
system(cmd)
source(here::here("scripts", "boot.R"))
