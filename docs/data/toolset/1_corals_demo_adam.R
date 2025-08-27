# Script to demonstrate the basic capabilities of R
# Using the divDyn toolchain
# Ádám T. Kocsis, 2023-08-10, Erlangen
# CC BY 4.0

# check whether packages are present, otherwise install them
required <- c("divDyn", "chronosphere", "sf")
installed <- installed.packages()

# a for loop: iterate for all packages in required
for(i in 1:length(required)){
	# conditional statement
	if(!required[i]%in%rownames(installed)){
		install.packages(required[i])
	}
}

# setting a project directory
projDir <- NULL

## # an example working directory is
## projDir <- "/home/adam/Desktop/corals_demo/"
if(!is.null(projDir)) setwd(projDir)

# pair of functions to set/get the working dir
# getwd()
# setwd()

# difference between NA and NULL
# NA: present but undefined
# NULL: not present
length(NA)
length(NULL)

# binary predicate functions checking whether something is something
# is.na()
# is.numeric()
# is.list()

# messages
message("Your current working directory is:")
message(getwd())

# other messaging functions
message("Hello World!")
print("Hello World!")
cat("Hello World!\n")


# using external packages
library(divDyn)
library(chronosphere)
library(sf)

# using built-in data
data(stages) # stage-level timescale
data(corals) # Paleobiology Database download

# executing function calls from packages
dd <- divDyn::divDyn(corals, bin="stg", tax="genus")
stats <- divDyn::binstat(corals, bin="stg", tax="genus")

# calculating simple values and printing them to the console
nCorals <- length(unique(corals$genus))

# concatenating text
statement <-paste(
	"The total number of sampled coral genera is: ",
	nCorals,".",  sep="")

# and printing them to the console
message(statement)

# plotting variables against each other (scatter plots)
plot(
	stats$occs, dd$divSIB,
	xlab="Number of occurrences",
	ylab="Sampled-in-bin diversity")

# the same plot built from scratch (almost)
plot(stats$occs, dd$divSIB,type="n", axes=FALSE, xlab="", ylab="") # empty plot with defined plotting params
box() # bouding box
axis(1) # bottom axis
axis(2) # left axis
points(stats$occs, dd$divSIB) # actual data
mtext(1, text="Number of occurrences", line=3) # x axis label
mtext(2, text="Sampled-in-bin diversity", line=3) # y axis label

# different functions to open new graphics device windows
# windows() # only on Windows!
# quartz() # only on MacOS!
# x11() # general


# calculating simple statistics and printing them out to the console
cor.test(stats$occs, dd$divSIB, method="spearman")
message("It is important to understand warning messages!")

# example warning message
temp <- c("a", "b", 4, 7, 12) # vector promoted to character
as.numeric(temp) # numeric conversion produces a warning

# Note about machine learning in R
# Adam recommends H2O: https://h2o.ai/platform/ai-cloud/make/h2o/

# higher and lower level plotting
divDyn::tsplot(stages, boxes="sys", xlim=52:95, ylim=c(0,300), boxes.col="col")
# low level plotting
lines(stages$mid, dd$divSIB, col="black", lwd=2)
lines(stages$mid, dd$divRT, col="blue", lwd=2)

# legend
legend(
	"topleft",
	legend=c("Sampled-in-bin diversity", "Range-through diversity"),
	col=c("black", "blue"),
	lwd=2)

# saving outputs to a directory
# create new directory from R
dir.create("corals_export")

# list files in the current working directory, or path
list.files()
list.files("corals_export")

# bash-style paths work here!!
# dir.create("./corals_export")
# dir.create("../corals_export")

# subsetting tables (matrices)
saveThis <- dd[52:95,]

# writing (and reading tables)
write.csv( saveThis, file="corals_export/divDyn.csv", row.names=FALSE)

# also the original result
# single objects (.rds)
saveRDS(file="corals_export/divDyn_original.rds", dd)
newDD <- readRDS(file="corals_export/divDyn_original.rds")

# objects with environment information(.RData)
save(dd, file="corals_export/divDyn_original.RData")


# remove the object
rm(dd) # after this, dd is no longer present

#  after running it, the dd object is recreated!
load(file="corals_export/divDyn_original.RData")

# RData for multiple objects
save(list=c("dd", "stages"), file="corals_export/divDyn_multiple.RData")

# downloading and drawing a world map
ne <-fetch("NaturalEarth") # from the chronosphere package
plot(ne$geometry, col="gray", main="Coral occurrences from the world")

# with the coral occurrences
coordinates <- unique(corals[,c("lng","lat")])
# points with RGB colors
points(coordinates, pch=3, col="#AA223355")


#' Function to plot a present-day map of occurrences from a given stage on a given background
#'
#' @param x Data frame of occurrences
#' @param stages Numeric identifier of a stage
#' @param ts Data frame of time scale
#' @param map sf -object, base map
#' @param ...arguments passed to the plot function
#' @return The function has no return value
plot_occs_from_stage <- function(x, stage, ts=stages, map=ne$geometry, ...){
	# occurrences from the stage
	thisStage <- x[which(x$stg==stage),]

	# a plot
	plot(map,
		main=paste("Coral occurrences from the", ts$stage[stage]), ...)

	
	# with the coral occurrences
	coordinates <- unique(thisStage[,c("lng","lat")])
	points(coordinates, pch=3, col=ts$col[stage])
}


# saving plots to the hard drive (raster graphics device), params given in pixels
png("corals_export/one_stage_occs.png", width=2000, height=1000)
	# calling of functions
	plot_occs_from_stage(x=corals, stage=67, col="gray")
dev.off()


# different device (vector graphic), params are given in inches
pdf("corals_export/one_stage_occs.pdf", width=20, height=10)
	# calling of functions
	plot_occs_from_stage(x=corals, stage=67, col="pink", border=NA)
dev.off()
