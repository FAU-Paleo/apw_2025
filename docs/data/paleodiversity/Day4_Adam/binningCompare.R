# Compare two different stage-level binning schemes and write out data.
# Adam Kocsis
# 2024-08-02

library(divDyn)
library(chronosphere)

data(stages)

# Data download with explicit version
# newer versions don't work with stb binning  
dat <- fetch("pbdb", ver="20240711")

# filter records not identified at least to genus
dat <-dat[dat$accepted_rank %in% c("genus", "species"),]

# omit non-informative genus entries
dat <- dat[dat$genus!="", ]

nrow(dat)


# I. Taxonomic filtering
marineNoPlant <- c("",
    "Agmata",
    "Annelida",
    "Bilateralomorpha",
    "Brachiopoda",
    "Bryozoa",
    "Calcispongea",
    "Chaetognatha",
    "Cnidaria",
    "Ctenophora",
    "Echinodermata",
    "Entoprocta",
    "Foraminifera",
    "Hemichordata",
    "Hyolitha",
    "Mollusca",
    "Nematoda",
    "Nematomorpha",
    "Nemertina",
    "Onychophora",
    "Petalonamae",
    "Phoronida",
    "Platyhelminthes",
    "Porifera",
    "Problematica",
    "Rhizopodea",
    "Rotifera",
    "Sarcomastigophora",
    "Sipuncula",
    "Uncertain",
    "Vetulicolia",
    ""
)

# which rows?
bByPhyla <- dat$phylum%in% marineNoPlant

#B. classes
#   levels(factor(noNeed$class))
needClass <- c(
    "Acanthodii",
    "Actinopteri",
    "Actinopterygii",
    "Agnatha",
    "Cephalaspidomorphi",
    "Chondrichthyes",
    "Cladistia",
    "Coelacanthimorpha",
    "Conodonta",
    "Galeaspida",
    "Myxini",
    "Osteichthyes",
    "Petromyzontida",
    "Plagiostomi",
    "Pteraspidomorphi",
    # here come the Arthropods
    "Artiopoda",
    "Branchiopoda",
    "Cephalocarida",
    "Copepoda",
    "Malacostraca",
    "Maxillopoda",
    "Megacheira",
    "Merostomoidea",
    "Ostracoda",
    "Paratrilobita",
    "Pycnogonida",
    "Remipedia",
    "Thylacocephala",
    "Trilobita",
    "Xiphosura"
)

# which rows?
bNeedClass <- dat$class %in% needClass

#C.  mammals
#   mammals <- dat[dat$class=="Mammalia",]
#   levels(factor(mammals$order))
needMammalOrd <- c("Cetacea", "Sirenia")

# which rows?
bMammalOrder <- dat$order %in% needMammalOrd

# the carnivores
#   carnivores <- dat[dat$order=="Carnivora",]
#   levels(factor(carnivores$family))
needFam <- c("Otariidae", "Phocidae", "Desmatophocidae")

# which rows?
bNeedMamFam<- dat$family%in%needFam

# D. Reptiles
#   reptiles <- dat[dat$class=="Reptilia",]
#   levels(factor(reptiles$order))
needReptOrd<-c(
    "Eosauropterygia",
    "Hupehsuchia",
    "Ichthyosauria",
    "Placodontia",
    "Sauropterygia",
    "Thalattosauria"
)

# which rows?
bRept <- dat$order%in%needReptOrd

# E. turtles
#   turtles <- dat[dat$order=="Testudines",]
#   levels(factor(turtles$family))

# Chelonioidea turtles
needTurtleFam <- c(
    "Cheloniidae",
    "Protostegidae",
    "Dermochelyidae",
    "Dermochelyoidae",
    "Toxochelyidae",
    "Pancheloniidae"
)

# which rows?
bTurtle <- dat$family%in%needTurtleFam

# subset the data
dat <- dat[bByPhyla | bNeedClass | bMammalOrder | bNeedMamFam | bRept | bTurtle , ]

# evade homonymy problem
dat$clgen <- paste(dat$class, dat$genus)

# II. Environmental and lithological filtering
omitEnv <- c(
    "\"floodplain\"",
    "alluvial fan",
    "cave",
    "\"channel\"",
    "channel lag" ,
    "coarse channel fill",
    "crater lake",
    "crevasse splay",
    "dry floodplain",
    "delta plain",
    "dune",
    "eolian indet.",
    "fine channel fill",
    "fissure fill",
    "fluvial indet.",
    "fluvial-lacustrine indet.",
    "fluvial-deltaic indet.",
    "glacial",
    "interdune",
    "karst indet.",
    "lacustrine - large",
    "lacustrine - small",
    "lacustrine delta front",
    "lacustrine delta plain",
    "lacustrine deltaic indet.",
    "lacustrine indet.",
    "lacustrine interdistributary bay",
    "lacustrine prodelta",
    "levee",
    "loess",
    "mire/swamp",
    "pond",
    "sinkhole",
    "spring",
    "tar",
    "terrestrial indet.",
    "wet floodplain"
)

# actual omission
dat<-dat[!dat$environment%in%omitEnv, ]

nrow(dat)

dat <- dat[dat$lithification1!="unlithified",]
nrow(dat)


#####################################################
# Stratigraphic binning - 'stg' stages
data(keys)

# B. the stg entries (lookup)
stgMin<-categorize(dat[,"early_interval"],keys$stgInt)
stgMax<-categorize(dat[,"late_interval"],keys$stgInt)

stgMin<-as.numeric(stgMin)
stgMax<-as.numeric(stgMax)

# empty container
dat$stg <- rep(NA, nrow(dat))

# select entries, where
stgCondition <- c(
# the early and late interval fields indicate the same stg
    which(stgMax==stgMin),
# or the late_intervarl field is empty
    which(stgMax==-1))

dat$stg[stgCondition] <- stgMin[stgCondition]

# Cambrian
# load data
load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/cambStrat.RData"))

# correct it with this function
source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2018-08-31/cambProcess.R")

# Ordovician
# load data
load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/ordStrat.RData"))

# correct it with this function
source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2019-05-31/ordProcess.R")



#####################################################
# Stratigraphic binning - 'stb' stages (internal binning of the PBDB)

#' Function to generate stage-lookup table from the Paleobiology Database
#'
#' Processess time-bin data downloaded from the database
#'
#' @param pbdb data.frame from database.
#' @param stagecolumn Charater string, of the time-bin information column
#' @param min_ma Character string, column name of the minimum ages 
#' @param max_ma Character string, column name of the maximum ages 
#' @param bin Character string, this will be the column name of the bin information
#' @return A data.frame with the stages' name bottom, mid and top age and bin number.
StagesFromPBDB<- function(pbdb, stagecolumn="time_contain", min_ma="min_ma", max_ma="max_ma", bin="stb"){
	# get the appropriate colulmns out of the PBDB download
	timeinfo <- unique(pbdb[,c(stagecolumn, max_ma, min_ma)])

	# omit those entires that are not meaningful
	timeinfo <- timeinfo[timeinfo[,stagecolumn]!="-",]
	
	# search for stage minima and maxima
	ma <- tapply(INDEX=timeinfo[,stagecolumn], X=timeinfo[, max_ma], max)
	min <- tapply(INDEX=timeinfo[,stagecolumn], X=timeinfo[, min_ma], min)

	# create stages object
	stagesDF <- data.frame(stage=names(ma), top=ma, mid=(ma+min)/2, bottom=min)
	stagesDF <-stagesDF[order(stagesDF$bottom, decreasing=TRUE),]
	rownames(stagesDF) <- NULL

	# check consistency - Cambrian is not consistent
	# stages$bottom[1:(nrow(stages)-1)]==stages$top[2:nrow(stages)]


	# create integer bin numbers
	stagesDF<- cbind(stagesDF, 1:nrow(stagesDF))
	colnames(stagesDF)[ncol(stagesDF)] <- bin
	return(stagesDF)
}

#' Function to bin PBDB data to stages
#'
#' @param pbdb data.frame from database.
#' @param stages data.frame produced by StagesFromPBDB.
#' @param bin Character string, this will be the column name of the bin information
#' @param stagecolumn Charater string, of the time-bin information column in the PBDB
#' @return A vector with integer bin numbers
#' @examples
#' pbdb <- chronosphere::fetch("pbdb")
#' # new stages object
#' stages <- StagesFromPBDB(pbdb)
#' # bins from the pbdb output
#' pbdb$stb <- BinPBDB(pbdb, stages)
BinPBDB <- function(pbdb,stages,bin="stb",stagecolumn="time_contain"){
	# new vector
	y <- rep(NA, nrow(pbdb))

	# simple for loop to execute the binning
	for(i in 1:nrow(stages)){
		y[which(pbdb[, stagecolumn]==stages$stage[i])] <-stages[i,bin]
	}

	return(y)
}


# Execute
# new stages object
stagebins <- StagesFromPBDB(dat)

# bins from the pbdb output
dat$stb <- BinPBDB(dat, stagebins)

################################################################################
# Merge data together for CMR
stgFields <- stages[,c("stg", "stage","top", "bottom")]
colnames(stgFields)[2:4] <- paste0("stg_", colnames(stgFields))[2:4]
datMerged <- merge(dat,stgFields, by="stg")

stbFields <- stagebins
colnames(stbFields)[1:4] <- paste0("stb_",colnames(stbFields)[1:4])
datMerged <- merge(datMerged,stbFields, by="stb")

# save file
saveRDS(datMerged, file="pbdb_processed_2024-07-11.rds")

################################################################################
# Comparison of the two solutions (number of records)
sampStg<- binstat(dat, bin="stg", tax="clgen", coll="collection_no",
        duplicates=FALSE)
sampStb<- binstat(dat, bin="stb", tax="clgen", coll="collection_no",
        duplicates=FALSE)

# plot binning comparison
samplingStg <- merge(stages, sampStg, by="stg")
samplingStb <- merge(stagebins, sampStb, by="stb")

tsplot(stages, boxes="sys", shading="sys", xlim=4:95, ylim=c(0,30000),
    ylab="Number of Records")

lines(samplingStg$mid, samplingStg$occs, lwd=2)
lines(samplingStb$mid, samplingStb$occs, lwd=2, col="red")


legend("top", legend=c("'stg' (divDyn)", "'stb' (PBDB)"), col=c("black", "red"), lwd=1, bg="white", inset=c(0, 0.02))

################################################################################
# Comparison of the two solutions (rt-diversity)
ddStg<- divDyn(dat, bin="stg", tax="clgen")
ddStb<- divDyn(dat, bin="stb", tax="clgen")

# plot binning comparison
divStg <- merge(stages, ddStg, by="stg")
divStb <- merge(stagebins, ddStb, by="stb")

tsplot(stages, boxes="sys", shading="sys", xlim=4:95, ylim=c(0,4000),
    ylab="Range-through diversity")

lines(divStg$mid, divStg$divRT, lwd=2)
lines(divStb$mid, divStb$divRT, lwd=2, col="red")

legend("top", legend=c("'stg' (divDyn)", "'stb' (PBDB)"), col=c("black", "red"), lwd=1, bg="white", inset=c(0, 0.02))
