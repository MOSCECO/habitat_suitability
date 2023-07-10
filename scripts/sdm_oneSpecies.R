# HABITAT SUITABILITY MODEL
# One Species
# MOSCECO - Biodiversité benthique de Martinique et de Guadeloupe

# Claremontiella nodulosa

# Données biologiques ----

sp  <- pa$`Claremontiella nodulosa`
sp <- species$muri[species$muri$scientificName == "Claremontiella nodulosa"]
# ajout des missions scientifiques
sp$expedition[is.na(sp$expedition)] <- ifelse(
  grepl("CP|DW", sp$method[is.na(sp$expedition)]),
  "KARUBENTHOS 2",
  NA
)
sp$expedition[is.na(sp$expedition)] <- ifelse(
  grepl("G", sp$method[is.na(sp$expedition)]),
  "KARUBENTHOS 2012",
  "MADIBENTHOS"
)

isl <- unique(sp$ISL)
tax <- unique(sp$TAX)

# Présences
spp <- sp %>% 
  filter(individualCount > 0)
spp_sf <- st_as_sf(
  spp, 
  coords = c("decimalLongitude", "decimalLatitude"),
  remove = F
)
# Absences
spa <- sp %>% 
  filter(individualCount == 0) %>% 
  filter(expedition != "KARUBENTHOS 2")
# On ignore la mission Karubenthos 2 qui présente un biote très différent 
# de celui de la côte... mais que pour les absences ? Sûrement un biais là.

# Données environnementales ----
path_clm <- here("data", "tidy", "climatology") 
mtd <- "mean"

clm <- if (isl == "ANT") {
  out <- lapply(
    list.files(path_clm, full.names = T),
    \(path_isl) {
      out <- lapply(
        list.files(path_isl, full.names = T),
        \(path_vrv) {
          list.files(path_vrv, pattern = mtd, full.names = T) %>% 
            lapply(rast)
        }
      )
      names(out) <- list.files(path_isl)
      return(out)
    }
  )
  names(out) <- list.files(path_clm)
  out
} else {
  out <- lapply(
    list.files(here(path_clm, isl), full.names = T), 
    \(path_vrv) {
      lapply(
        list.files(path_vrv, pattern = mtd, full.names = T),
        rast
      )
    }
  )
  names(out) <- list.files(here(path_clm, isl))
  out <- list(out)
  names(out) <- isl
}

# Matrice environnementale
mat_env <- lapply(
  islands, 
  \(isl) {
    isl2 <- ifelse(isl == "GLP", "GP", "MQ")
    Reduce(
      left_join,
      lapply(
        names(clm[[isl]]),
        \(ev) { # environnemental variable
          r <- clm[[isl]][[ev]][[1]]
          out <- terra::extract(
            r, 
            spp_sf %>% filter(country == isl2)
          )
          names(out)[2] <- ev
          return(out)
        }
      )
    )
  }
)
