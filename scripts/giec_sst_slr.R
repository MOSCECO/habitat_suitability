# CMIP6 - Sea Surface Temperature (SST) Change deg C -
# Medium Term (2041-2060) SSP1-2.6 (rel. to 1995-2014) - Annual (26 models)-CAR
ssp126 <- read.csv(here("data", "raw", "projection", "sst_proj_ssp126.csv"))
# Medium Term (2041-2060) SSP5-8.5 (rel. to 1995-2014) - Annual (27 models)-CAR
ssp585 <- read.csv(here("data", "raw", "projection", "sst_proj_ssp585.csv"))

# CMIP6 - Sea level rise (SLR) Change meters -
# Medium Term (2041-2060) SSP1-2.6 (rel. to 1995-2014) - Annual -CAR
slr126 <- read.csv(here("data", "raw", "projection", "slr_proj_ssp126.csv"))
# Medium Term (2041-2060) SSP5-8.5 (rel. to 1995-2014) - Annual -CAR
slr585 <- read.csv(here("data", "raw", "projection", "slr_proj_ssp585.csv"))

sst_minimal_change <- ssp126$P5[ssp126$Period  == "Medium Term (2041-2060)"]
sst_maximal_change <- ssp585$P95[ssp585$Period == "Medium Term (2041-2060)"]

slr_minimal_change <- slr126$P5[slr126$Period  == "Medium Term (2041-2060)"]
slr_maximal_change <- slr585$P95[slr585$Period == "Medium Term (2041-2060)"]

table1 <- tibble(
  `Scénario` = c("ssp1-2.6", "ssp1-2.6", "ssp5-5.8", "ssp5-5.8"),
  Percentile = c(5, 95, 5, 95),
  `Température de la mer` = c(
    sst_minimal_change,
    ssp126$P95[ssp126$Period  == "Medium Term (2041-2060)"],
    ssp585$P5[ssp585$Period == "Medium Term (2041-2060)"],
    sst_maximal_change
  ),
  `Élévation du niveau de la mer` = c(
    slr_minimal_change,
    slr126$P95[slr126$Period  == "Medium Term (2041-2060)"],
    slr585$P5[slr585$Period == "Medium Term (2041-2060)"],
    slr_maximal_change
  ),
)
write.csv(
  table1,
  here("data", "raw", "projection", "table_livrable.csv"),
  row.names = F
)
