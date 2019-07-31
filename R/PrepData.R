# ----------------
# N. LAMBERT, 2019
# ----------------

setwd("/var/www/html/worldpop/R")

# Packages

library(raster)
library(sf)
library(cartography)
library(SpatialPosition)
library(geojsonio)

# ----------------------
# Data import & handling
# ----------------------


# Variables

fact <- 2
res <- 1
span <- 200000
# Countries

world <- st_read("data/world.shp")
# prj <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# world <- st_transform(world, prj)

# Total population grid

r <- raster("data/gpw_v4_population_count_rev11_2020_1_deg.asc")
dots <- aggregate(r, fact=fact, fun=sum)
dots <- as(dots, 'SpatialPointsDataFrame')
dots <- st_as_sf(dots)
# dots <- st_transform(dots, prj)
colnames(dots) <- c("pop","geometry")

# ------------
# Computations
# ------------

dots$id <- c(1:nrow(dots))
mygrid <- CreateGrid(w = world, resolution = res,returnclass = "sf")
mygrid <- mygrid[mygrid$COORDX>= -180,]
mygrid <- mygrid[mygrid$COORDX<= 180,]
mygrid <- mygrid[mygrid$COORDY>= -90,]
mygrid <- mygrid[mygrid$COORDY<= 90,]

head(mygrid)
plot(st_geometry(mygrid), pch=20, cex = 0.2)
plot(st_geometry(dots), pch=20, col="red", add=T, cex = 0.2)

mymat <- CreateDistMatrix(knownpts = dots, unknownpts = mygrid)

mystewart <- stewart(knownpts = dots, unknownpts = mygrid,
                     matdist = mymat, varname = "pop",
                     typefct = "exponential", span = span,
                     beta = 3, returnclass = "sf", longlat = T)

# ------------
# Carto
# ------------

mystewart$pop <- mystewart$OUTPUT / 1000



summary(mystewart$pop)
bks <- c(-1, 10, 25, 50, 100, 250, 500, 1000, 2000, 3500, 5000, 10000, 20000, 35000, 50000, 75000, 100000, 170000)
contourpoly <- isopoly(x = mystewart, breaks = bks, var = "pop", returnclass = "sf", mask = world)
contourpoly$cols <- carto.pal(pal1 = "blue.pal", n1 = 7, pal2 = "red.pal", n2 = 10)
typoLayer(x = contourpoly, var="center",col = contourpoly$cols, border = "white", lwd = 0.2)

View(contourpoly)

# -----
# Export
# ------
smooth <- st_cast(contourpoly, "POLYGON")
geojson_write(input=smooth, object_name = "smooth", file = "../data/smooth.geojson", overwrite = T)
View(smooth)

legend <- as.data.frame(smooth)
legend <- legend[,c("id","max","cols")]
legend <- unique (legend)
colnames(legend) <- c("id","val","col")
legend$val <- round(legend$val,0)
legend <- legend[order(-legend$val),]

write.csv(x = legend,file = "../data/legend.csv", row.names = F)
