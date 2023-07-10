# Code marspec Clio

l <- list.files(
  here("data", "raw", "sst02_30s"), 
  pattern = "adf",
  full.names = T
)
l[[1]]
r <- do.call(c, lapply(l, rast))
r <- raster(
  here("data", "raw", "sst02_30s.aux"), band = 1
)
r <- tidync(
  here("data", "raw", "sst02_30s.aux")
)
crs(r) <- crs("EPSG:4326")
plot(r)

r <- rast(l[[1]])
names(r) <- "temp"
values(r) <- values(r)/100
x11(); plot(r)

