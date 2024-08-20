# install.packages("rgplates")
library(rgplates)

################################################################################
# 0. Last time
# data in the chronosphere
library(chronosphere)

# The PaleoMAP reconstruction model
mod <- fetch("paleomap", "model")

# simple reconstructions are possible
pl60 <- rgplates::reconstruct("static_polygons", age=60, model=mod)
plot(pl60$geometry, col="gray")

########################################----------------------------------------
# 1.B plotting the PBDB data on it
# Prepared phanerozoic data
pbdb <- readRDS("pbdb_processed_2024-07-11.rds")

data(stages)

# collection information
collections <- unique(pbdb[, c("collection_no", "lng", "lat", "stg")])

# filtering of weird points...
collections <- na.omit(collections)

# Using the plates - as earlier:
	# for one particular stage
	nStg <- 79

	# sources of data
	# data -> collections$stg
	# target age -> stages$mid

	# plate reconstruction
	targetAge <- stages$mid[nStg]
	platesStg <- rgplates::reconstruct("static_polygons", age=targetAge, model=mod)

	# the points
	collStg <- collections[which(collections$stg==nStg),]
	coords <- collStg[, c("lng", "lat")]
	coordsStg <- rgplates::reconstruct(coords, age=targetAge, model=mod)

	# plotting
	plot(platesStg$geometry, col="gray", border=NA)
	points(coordsStg, pch=3, col="red")


# Missing points! - validtime
# coordsStg <- rgplates::reconstruct(coords, age=targetAge, model=mod, validtime=FALSE)

################################################################################
# 1. Topography?
# Requires the terra, via and ncdf4 packages
# data objects!
dat <- chronosphere::datasets()

# The PaleoDEMs
dems <- fetch("paleomap", "dem")
dems

# subsetting
# by name
demPresent <- dems["0"]

# element name
demPresent <- dems[["Map01_PALEOMAP_1deg_Holocene_0Ma.nc"]]

# terra::rast(demPresent)
plot(demPresent)

# finer resolution?
# available datasets
dats <- datasets("paleomap")

# download 
dems <- fetch("paleomap", "dem", res="0.1")

# names - age
dem60 <-dems["60"]

# compared to plates?
plot(dem60)
plot(pl60$geometry, add=TRUE, col="#BB003388")

# Differences: slopes, distortion and flooded shelf

# other time slice?
pl140 <- rgplates::reconstruct("static_polygons", age=140, model=mod)
plot(dems["140"])
plot(pl140$geometry, add=TRUE, col="#BB003388")



########################################----------------------------------------
# With the topography
	# for one particular stage
	nStg <- 79
	# sources of data
	# data -> collections$stg
	# target age -> stages$mid + as.numeric(names(dems))

	# which map is closest to the stage 
	# where the difference is smallest
	differences <- abs(as.numeric(names(dems)) - stages$mid[nStg])
	indexClosest <- which.min(differences)
	targetAge <- as.numeric(names(dems))[indexClosest]
	demStg <- dems[as.character(targetAge)]

	# shortcut:
	targetAge <- divDyn::matchtime(as.numeric(names(dems)), stages$mid[nStg])
	demStg <- dems[as.character(targetAge)]

	# the points
	collStg <- collections[which(collections$stg==nStg),]
	coords <- collStg[, c("lng", "lat")]
	coordsStg <- rgplates::reconstruct(coords, age=targetAge, model=mod)

	# plotting
	plot(demStg)
	points(coordsStg, pch=3, col="red")
	

########################################----------------------------------------
# Derived from this: PaleoMAP PaleoAtlas
	pa <- fetch("paleomap", "paleoatlas")
	pa

	# images: 3 channels - R, G, B
	# NO 85!!! - 80 is closest
	pa["80",1] # red channel
	plot(pa["80",1])
	plotRGB(pa["80",]@stack)
	coordsStg <- rgplates::reconstruct(coords, age=80, model=mod, plateperiod=TRUE)
	points(coordsStg, pch=3, col="red")

################################################################################
# 2. Bridge-HADCM3L
# excerpt 85MA
ocean <- terra::rast("86.7_texpr2o.pgclann.nc")

# annual averages
airtemp <- terra::rast("86.7_texpr2a.pcclann.nc", subds="temp_mm_p")
oceantemp <- terra::rast("86.7_texpr2o.pgclann.nc", subds="temp_ym_dpth")

plot(oceantemp[[1]])

oceantemp <- terra::rotate(oceantemp)

# code from earlier
	# for one particular stage
	nStg <- 79 
	# sources of data
	# data -> collections$stg
	# target age -> stages$mid + as.numeric(names(tos))

	# which map is closest to the stage 

	# shortcut:
	targetAge <- divDyn::matchtime(c(80,85, 90), stages$mid[nStg])
	tosStg <-oceantemp[[1]] 

	# the points
	collStg <- collections[which(collections$stg==nStg),]
	coords <- collStg[, c("lng", "lat")]
	coordsStg <- rgplates::reconstruct(coords, age=targetAge, model=mod, plateperiod=TRUE)

	# plotting
	plot(tosStg)
	points(coordsStg, pch=3, col="red")

	# extraction of data!
	colnames(coordsStg) <- c("plng", "plat")
	collStg <- cbind(collStg, coordsStg)
	collStg$tos <- extract(tosStg, coordsStg)

################################################################################
# 3. PALEOMAP PaleoCoastlines coastlines
# Project with C scotese PBDB+Paleomap
pc <- fetch("paleomap", "paleocoastlines")
pc
pc["45", ]

# one time slice
coast45 <- pc["45","coast"]
mar45 <- pc["45", "margin"]

# preferred maps!
x11()
plot(mar45, col="cyan", border=NA)
plot(coast45, col="brown", border=NA, add=TRUE)

# the coordinates - with 85
#points(coordsStg, pch=3, col="red")

# extrapolated tos - Kocsis et al. 2021
tos_extra <- fetch("SOM-kocsis-provinciality", "sstinter")

# again the same bit 
	nStg <- 85 
	# sources of data
	# data -> collections$stg
	# target age -> stages$mid + as.numeric(names(tos))

	# which map is closest to the stage 

	# shortcut:
	targetAge <- divDyn::matchtime(as.numeric(names(tos_extra)), stages$mid[nStg])
	tosStg <- tos_extra[as.character(targetAge)]

	# the points
	collStg <- collections[which(collections$stg==nStg),]
	coords <- collStg[, c("lng", "lat")]
	coordsStg <- rgplates::reconstruct(coords, age=targetAge, model=mod, plateperiod=TRUE)

	# plotting
	plot(tosStg)
	points(coordsStg, pch=3, col="red")

	# extraction of data!
	colnames(coordsStg) <- c("plng", "plat")
	collStg <- cbind(collStg, coordsStg)
	collStg$tos <- extract(tosStg, coordsStg)[,1]
