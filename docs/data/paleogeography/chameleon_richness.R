################################################################################
# Example to calculate a richness map from IUCN chameleon ranges
# Example from Aug. 22, 2025
################################################################################
library(terra)

# set up working dir!
setwd("1_vectors/")

# the chameleon dataset
chameleons <- sf::st_read("data/CHAMELEONS/CHAMELEONS.shx")

# an empty raster
div <- rast(res=0.1)

# aggregator
values(div) <- 0

# layer to be masked
lay <- div
values(lay) <- 1

# omit multiple records of the same species
chams <- chameleons[!duplicated(chameleons$binomial	), ]

# loop through all of them
for(i in 1:nrow(chams)){
	# focus on one species at a time
	thisCham <- chams[i, ]

	# the maskink by one species
	one <- mask(lay,thisCham, updatevalue=0)

	# add these
	div <- div + one

	# loop counter
	cat(i, "\r")
	flush.console()
}

# a heatmap
plot(div)
