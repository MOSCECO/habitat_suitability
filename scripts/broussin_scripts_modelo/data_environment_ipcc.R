# Données environnementales projetées ----

# On fait le postulat que la colonne d'eau n'est pas stratifié
# (0-150m, tropiques)

# Scénario optimiste ----
climosaic_ssp126 <- climosaic
climosaic_ssp126$depth <- climosaic_ssp126$depth + slr_minimal_change
climosaic_ssp126$mean.sst <- climosaic_ssp126$mean.sst + sst_minimal_change
climosaic_ssp126$stdv.sst <- climosaic_ssp126$stdv.sst + sst_minimal_change
climosaic_ssp126$mean.sbt <- climosaic_ssp126$mean.sbt + sst_minimal_change
climosaic_ssp126$stdv.sbt <- climosaic_ssp126$stdv.sbt + sst_minimal_change

# Scénario optimiste ----
climosaic_ssp585          <- climosaic
climosaic_ssp585$depth    <- climosaic_ssp585$depth    + slr_maximal_change
climosaic_ssp585$mean.sst <- climosaic_ssp585$mean.sst + sst_maximal_change
climosaic_ssp585$stdv.sst <- climosaic_ssp585$stdv.sst + sst_maximal_change
climosaic_ssp585$mean.sbt <- climosaic_ssp585$mean.sbt + sst_maximal_change
climosaic_ssp585$stdv.sbt <- climosaic_ssp585$stdv.sbt + sst_maximal_change
