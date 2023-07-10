# Here are ways that show how you might do that.
# 
# Example data

library(terra)
x <- rast(xmin=0, xmax=10, ymin=0, ymax=10, res=1, vals=1)
y <- rast(xmin=9, xmax=19, ymin=0, ymax=10, res=0.9, vals=2)
z <- rast(xmin=0, xmax=10, ymin=8.1, ymax=18.1, res=1, vals=3)

# You may want to use one of the input rasters as template. 
# Let's say you like y. In that case:

a <- list(x, z)
names(a) <- c("x", "z")
b <- lapply(a, \(i) {
     x <- extend(rast(y), i)
     resample(i, crop(x, i, "out"))
   })

b <- sprc(c(b, y))
m <- merge(b)
 

# Or create a new template raster. 
# In that case, first find out the combined extent

a <- list(x, y, z)
b <- sapply(a, \(i) ext(i) |> as.vector())
e <- ext(min(b[1,]), max(b[2,]), min(b[3,]), max(b[4,]))

# use the extent to create a raster with the desired spatial resolution 
r <- rast(e, res=1)

And now as above:

gg <- lapply(a, \(i) resample(i, crop(r, i, "out")))
g <- merge(sprc(gg))

Or like this

src <- sprc(a)
ss <- impose(src, r)
s <- max(ss, na.rm=TRUE)

# I suppose some of this could be wrapped into a terra method.

# Also see mosaic as an alternative to merge.

# As for choosing the best resolution etc., 
# there it is up to your needs and what your data might reasonably support. 
# But one important general consideration is that you want to avoid resampling 
# as much as possible --- as it deteriorates the data quality.
