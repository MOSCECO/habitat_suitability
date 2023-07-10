map <- global_map_sp

## We keep only "wild" indices.
locs <- occ_sp

x11() ; hist(map, type = "l")
## The variable artif is far from symetric

## We perform a square root transformation
## of this variable
## We therefore normalize the variable 'artif'
slot(map, "data")[, "mean.sbt"] <- log2(slot(map, "data")[, "mean.sbt"])
x11() ; hist(map, type = "l")

## We prepare the data for the ENFA
tab <- slot(map, "data")
pr <- slot(count.points(locs, map), "data")[,1]
nona <- which(rowSums(apply(tab, 2, is.na)) == 0)
pr <- pr[nona]
table(pr, useNA = "always")

## We then perform the PCA before the ENFA
pc <- dudi.pca(tab %>% na.omit(), scannf = FALSE)

## The object 'pc' contains the transformed table (i.e.
## centered so that all columns have a mean of 0
## and scaled so that all columns have a variance of 1
## 'pc' also contains the weights of the habitat variables,
## and the weights of the pixels in the analysis

enfa1 <- adehabitatHS::enfa(dudi = pc, pr = pr, scannf = FALSE)
hist(enfa1)
hist(enfa1, scores = FALSE, type = "l")


## scatterplot
ade4::scatter(enfa1)


## randomization test
## Not run: 
(renfa <- randtest(enfa1))
plot(renfa)

require(biomod2)
tb <- map@coords[nona, ] %>% 
  cbind(value = enfa1$li[, 1]) %>% 
  as_tibble()
levelplot(
  x = value ~ x * y,
  data = tb, 
  aspect = "iso",
  main = "Claremontiella nodulosa ENFA global projection", 
  col.regions = colorRampPalette(c("grey90", "yellow4", "green4"))(100)
)
