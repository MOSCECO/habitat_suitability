# Cartes de distribution

path_figures_carte <- here("figures", "cartes_de_distribution")
makeMyDir(path_figures_carte)

# Occurrences spatialisées de chaque espèce
offsets_title_tax <- list(
  GLP = c(x = - 1.01,  y = - 0.05),
  MTQ = c(x = - 0.47, y = - 0.03)
)
dimensions_ggexport <- list(
  GLP = c(width = 2300, height = 2000, res = 200), 
  MTQ = c(width = 1850, height = 2000, res = 200)
)

figures_cartes_distributions <- mapply(
  function(isl, offs, dims) {
    
    path_figures_carte_isl <- here(path_figures_carte, isl)
    makeMyDir(path_figures_carte_isl)
    
    depths <- bathy[[isl]] 
    depths_bbox <- c(
      xmin = min(depths$x),
      ymin = min(depths$y),
      xmax = max(depths$x),
      ymax = max(depths$y)
    )
    
    p_distribution_taxon <- mapply(
      function(tax, title_tax, col_tax) {
        
        path_figures_carte_isl_tax <- here(path_figures_carte_isl, title_tax)
        makeMyDir(path_figures_carte_isl_tax)
    
        tb <- species[[isl]][[tax]]
        tc <- count(tb, scientificName)
        tc <- tc[order(-tc$n), ]
        tc_threshold <- tc[tc$n >= 10, ]
        tb_threshold <- tb %>% 
          filter(scientificName %in% tc_threshold$scientificName)
        tb_split <- split(tb_threshold, f = tb_threshold$scientificName)
        
        p_distribution_species <- lapply(
          tb_split,
          function(spe) {
            
            # spe <- tb_split$`Coralliophila salebrosa`
            
            bn_spe <- unique(spe$scientificName)
            
            p_out <- ggplot() + 
              geom_tile(
                data = depths,
                aes(x = x, y = y, fill = value)
              ) +
              guides(fill = "none") +
              new_scale("fill") +
              geom_sf(data = maps[[isl]], col = NA, fill = "lightgreen") + 
              geom_point(
                data = spe, 
                aes(x = decimalLongitude, y = decimalLatitude),
                col = "#C01417", 
                shape = "+", 
                size = 8
              ) + 
              theme(
                axis.title = element_blank()
              ) + 
              scale_x_continuous(expand = c(0,0)) +
              scale_y_continuous(expand = c(0,0))
            
            p <- p_out  + 
              annotate(
                "text", 
                label = bn_spe, 
                size = 11,
                col = "white", 
                x = depths_bbox[[3]] + offs[[1]],
                y = depths_bbox[[4]] + offs[[2]]
              )
            
            ggexport(
              p,
              filename = here(
                path_figures_carte_isl_tax,
                paste(
                  "carte",
                  "distribution", 
                  isl %>% tolower(),
                  tax %>% tolower(),
                  gsub(" ", "-", bn_spe),
                  sep = "_"
                ) %>% paste0(".png")
              ),
              width    = dims[[1]],
              height   = dims[[2]],
              res      = dims[[3]]
            )
            
            return(p_out)
          }
        )
        
        return(p_distribution_species)
      },
      taxa,
      Taxa, 
      colors_taxa,
      SIMPLIFY = F, 
      USE.NAMES = T
    )
    return(p_distribution_taxon)
  },
  islands, 
  offsets_title_tax,
  dimensions_ggexport, 
  SIMPLIFY = F, 
  USE.NAMES = T
)

#  faire les versions synthétiques des cartes
#             -----------
#   |    |    -----------
#   | GLP|    | MTQ|
# ----------- |    |
# -----------  
  
ggexport(
  p_distributions,
  filename = here(
    path_figures_histo,
    paste(
      "histogrammes",
      "total",
      sep = "_"
    ) %>% paste0(".png")
  ),
  width    = 3000,
  height   = 2000,
  res      = 200
)
