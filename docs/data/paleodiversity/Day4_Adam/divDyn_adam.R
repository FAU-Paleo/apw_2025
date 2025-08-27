# This is the code that Adam wrote during the lecture on Aug 21, 2025

# load packages
library(divDyn)
library(chronosphere)

# attach the timescales
data(tens, package="divDyn")
str(tens)
data(stages, package="divDyn")
str(stages)


# download pbdb from Zenodo
dat <- chronosphere::fetch(src="pbdb", ver="20250819")

################################################################################
# Chronosphere interlude
# overview (source-series combinations)
dats <- chronosphere::datasets()

# example downloads
dsmsni <- fetch(src="DSMS-NI") # a single data frame
myers <- fetch(src="myers-hotspots", ser="outer") # sf objects

# Different versions in the same source (src)
pbdbAvailable <- datasets(src="pbdb")

# accessing a different version from the chronosphere
datOld <- chronosphere::fetch(src="pbdb", ver="20220711")

# in case you have an issue downloading (e.g from figshare?)
# try again after running this
chronosphere::configure(curl=TRUE)

################################################################################
# 1. Taxonomy filtering
################################################################################

# taxonomy filetering
dat <-dat[dat$accepted_rank %in% c("genus", "species", "subgenus", "subspecies"),]

marineNoPlant <- c(
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

# the homonymy problem
dat$clgen <- paste(dat$class, dat$genus)

################################################################################
# 2. filtering by environment
################################################################################

# ENVIRONMENT
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
dat<-dat[!dat$environment%in%omitEnv, ]

# remaining
nrow(dat)

################################################################################
# Stratigraphic binning
################################################################################

# data are accessible from here
data(keys, package="divDyn")

# the stg entries (lookup)
stgMin<-categorize(dat[,"early_interval"],keys$stgInt)
stgMax<-categorize(dat[,"late_interval"],keys$stgInt)

# make them numeric
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

# copy over the coded early_intervals
dat$stg[stgCondition] <- stgMin[stgCondition]

# Additional stratigraphy of the Cambrian-Ordovician
# load data
load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/cambStrat.RData"))

# correct it with this function
source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2018-08-31/cambProcess.R")

# load data
load(url("https://github.com/divDyn/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/ordStrat.RData"))

# correct it with this function
source("https://github.com/divDyn/ddPhanero/raw/master/scripts/strat/2019-05-31/ordProcess.R")


# binning stats
sampStg<- binstat(dat, bin="stg", tax="clgen", coll="collection_no", duplicates=FALSE)

# binnning stats with references
sampStgRef<- binstat(dat, bin="stg", tax="clgen", coll="collection_no", ref="reference_no", duplicates=FALSE)


# merging with timescale is recommended for easier plotting
sampling <- merge(stages, sampStg, by="stg")
str(sampling)

# simples plot, kind of meh
plot(sampling$mid,sampling$colls, xlab="Age (Ma)" , ylab="Number of collections")

# an example geological time scale plot
tsplot(stages, boxes="sys", shading="sys", xlim=4:95, ylim=c(0,20000),
    ylab="Number of Records")

# The plot above is customizable to your exact needs
stages2 <- stages
# replace the Cretaecaous symbol with 'Cr', for example
stages2[stages2$sys=="K", "sys"] <- "Cr"

# plotting stages2 will show this
tsplot(stages2, boxes="sys", shading="sys", xlim=4:95, ylim=c(0,20000),
    ylab="Number of Records")

# occurrences
tsplot(stages, boxes="sys", shading="sys", xlim=4:95, ylim=c(0,20000),
    ylab="Number of Records")
lines(sampling$mid, sampling$occs, lwd=2)
# collections
lines(sampling$mid, sampling$colls, lwd=2, col="blue")

legend("topleft", bg="white", legend=c("occurrences", "collections"),
    col=c("black", "blue"), lwd=2, inset=c(0.05,0.01), cex=1.3)


# very strong correlation between occurrences and collections!
cor.test(sampling$occs, sampling$colls, method="spearman")

# actual diversity dynamics
dd <- divDyn(dat, bin="stg", tax="clgen")
str(dd)

# look here for the meaning of variables in the data.frame
?divDyn

# same treatment as with binstat - merging with the timesscale is recommended
patterns <- merge(stages, dd, by="stg")
str(patterns)


# plot richenss
tsplot(stages, boxes="sys", shading="sys", xlim=4:95, ylim=c(0,4000),
    ylab="Richness")
lines(patterns$mid, patterns$divRT)
lines(patterns$mid, patterns$divBC, col="blue")
legend("topleft", bg="white", legend=c("divRT", "divBC"),
    col=c("black", "blue"), lwd=2, inset=c(0.05,0.01), cex=1.3)

# turnover rates
tsplot(stages, boxes="sys", shading="sys", xlim=4:95, ylim=c(0,1.5),
    ylab="Taxonomic rates")
lines(patterns$mid, patterns$oriPC, col="green", lwd=2)
lines(patterns$mid, patterns$extPC, col="red", lwd=2)
legend("topleft", bg="white", legend=c("origination", "extinction"),
    col=c("green", "red"), lwd=2, inset=c(0.05,0.01), cex=1.3)


# richness with SIB and cSIB
tsplot(stages, boxes="sys", shading="sys", xlim=4:95, ylim=c(0,4000),
    ylab="Richness")
lines(patterns$mid, patterns$divRT)
lines(patterns$mid, patterns$divBC, col="blue")
lines(patterns$mid, patterns$divSIB, col="orange")
lines(patterns$mid, patterns$divCSIB, col="purple")
legend("topleft", bg="white", legend=c("divRT", "divBC", "divSIB", "divCSIB"),
    col=c("black", "blue", "orange", "purple"), lwd=2, inset=c(0.05,0.01), cex=1.3)


# correlation between occurrence counts and collections
cor.test(sampling$occs, patterns$divCSIB, method="spearman")
cor.test(sampling$colls, patterns$divCSIB, method="spearman")

# the subsample function - remember to increaese the iteration to at least 100! (preferable more)
crStg<-subsample(dat, bin="stg", tax="clgen", coll="collection_no", q=1000,
    iter=25, duplicates=FALSE, na.rm=TRUE)

# this structure is based on that returned by divDy!
cr <- merge(stages, crStg, by="stg")
