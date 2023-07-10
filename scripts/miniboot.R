# boot data_occ_preparation

# libraries ----
libs_to_call <- list(
  
  "ade4", 
  "adehabitatHS",
  "adehabitatMA",
  "biomod2", 
  "data.table",
  "devtools",
  "doMPI",
  "factoextra",
  "FactoMineR",
  "ggnewscale",
  "ggplot2",
  "ggpubr", 
  "ggthemes",
  "here",
  "missMDA", 
  "ncdf4", 
  "patchwork", 
  "purrr",
  "raster",
  "reshape2",
  "sf", 
  "sp", 
  "stars",
  "stringr",
  "terra",
  "tidync",
  "tidyverse",
  "vegan"
  
)

# library calls
lapply(libs_to_call, function(i) {
  
  bool <- is.element(i, .packages(all.available = TRUE))
  
  if (!bool) {
    install.packages(i, dependencies = T)
  }
  
  library(i, character.only = TRUE)
  
}
)

# remote libraries (github)
Sys.getenv("GITHUB_PAT")
Sys.unsetenv("GITHUB_PAT")
Sys.getenv("GITHUB_PAT")

# remote libraries ----
remote_libs_to_call <- list(
  # "RCMEMS"
  # "ClimateOperators"
)

github_accounts <- list(
  # "markpayneatwork"
)

mapply(
  function(pckg, usr) {
    
    bool <- is.element(pckg, .packages(all.available = TRUE))
    
    if (!bool) {
      path_url <- paste0(usr, "/", pckg)
      print(path_url)
      devtools::install_github(path_url)
    }
    
    library(pckg, character.only = TRUE)
    
  },
  remote_libs_to_call,
  github_accounts,
  SIMPLIFY = FALSE
)

# functions
lapply(
  list.files(
    here("scripts", "FUN"),
    full.names = T
  ),
  source
)