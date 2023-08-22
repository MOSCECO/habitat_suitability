# Prise en compte de la profondeur maximale de l'espèce
# à partir des données locales et globales.

# species
binomial_name <- "Claremontiella nodulosa"

# Global
ameri_depth <- read_stars(
  here("data", "raw", "dpth", "gebco_2023_n63.1055_s-60.293_w-143.4375_e-27.4219.tif")
)
ameri_occur <- list.files(
  here("data", "analysis"), pattern = "bio_data_cpc", full.names = T
) %>% readRDS()
ameri_occur <- ameri_occur %>%
  filter(scale == "global" & type == "pr")
res <- st_extract(ameri_depth, ameri_occur, )
names(res) <- c("depth", "geometry")
res$depth <- as.numeric(res$depth)
res$depth[res$depth > 0] <- 0
summary(res$depth)
quantile(res$depth, 0.9)
quantile(res$depth, 0.1)

# Local
local_occur <- list.files(
  here("data", "analysis"), pattern = "bio_data_hab", full.names = T
) %>% readRDS()
summary(local_occur$depth)
quantile(local_occur$depth, 0.9)
quantile(local_occur$depth, 0.1)

# Problème immense avec les profondeurs proposées par les modèles
# on a déjà une preuve que la distribution de la profondeur modélisée est
# différente de celle observée, et avec ces données là on constate que pour
# C. nodulosa, les profondeurs sont bien supérieures à celles évoquées dans
# Garrigues et al. 2014.

# On propose donc d'utiliser les premier et troisième quartile de chaque espèce
# pour circonscrire la distribution modélisée et prévenir les profondeurs
# inexactes.

all_depths <- res$depth %>% c(local_occur$depth)
bnd <- summary(all_depths)[c(2, 5)]

p_depth_boundaries <- here("data", "analysis", "depth_boundaries")
makeMyDir(p_depth_boundaries)
file_name <- paste(gsub(" ", "_", binomial_name), "depth", "boundaries", sep = "_") %>%
  paste0(".rds")
saveRDS(bnd, here(p_depth_boundaries, file_name))
