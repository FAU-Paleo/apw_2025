# Basics written up here:
# https://evolv-ed.net//articles/2023-03-31_subsample-1.html

# Required pacages
library(iNEXT) # rarefaction (analytical solution) - only Hill numbers!
library(divDyn) # subsampling (algorithmic solution)

# Some made up example data
# counts of specimens
counts <- c(35,19,13,9,6,4,2,2,2,2,2,2,2,1,1,1,1,1,1,1)

# visualization
barplot(counts, xlab="Rank of species", ylab="count of specimens")


########################################----------------------------------------
# 1A. Rarefaction in iNEXT
########################################----------------------------------------

# the high-level function showing
overview <- iNEXT::iNEXT(counts) # multiple target sample sizes
plot(overview)

# more detailed
iNEXT::estimateD(counts) # extrapolation to complete sampling
multipleEstimates <- iNEXT::estimateD(counts, level=seq(10, 250, 10)) # multiple target sample sizes

########################################----------------------------------------
# 1B. Classical Rarefaction (subsampling manually implemented)
########################################----------------------------------------
# (loose code)
# for easier treatment of the data!
# This is also more common data format
# one letter that represents each species
species <- letters[1:length(counts)]

# extended format of the sample
specimens <- rep(species, counts)
table(specimens)

# parameters

# container to the subsampling trial results

# iteration for every trial
	# create a trial subset (sample)

	# calculate something from trial subset (number of species)

	# loop counter

# calculate the expectation


########################################----------------------------------------
# 1C. Classical Rarefaction (subsampling manually implemented, with applied function)
########################################----------------------------------------

# The applied function
#' @param x A vector of entities coded as characters
richness <- function(x) length(unique(x))
richness(specimens)

# use this function to calculate richness!

########################################----------------------------------------
# 1D. Classical rarefaction as a function
########################################----------------------------------------

# Let's make a function out of what we have!

# parameters
#' @param x A vector of entities
#' @param q Subsampling quota
#' @param iter Number of iterations
#' @param FUN A function returning a single number based on a vector.
#' @return The expectation for the function.


########################################----------------------------------------
# 1E. Classical rarefaction - calculated for multiple qs
########################################----------------------------------------

# Exercise 1: Use a for loop to iterate the function for every q from q=1 to q=length(specimens)! (5 min)

# Exercise 2:  Try to calculate the subsampled Shannon's entropy! This is implemented in vegan::diversity!
# NOTE: The input to this function needs to be a vector of counts, so you have to wrap
# vegan::diversity in a function that translates the subsampling trial data/subsample to counts!
# (hint: the function to do this was used in this script!)

########################################----------------------------------------
# 2A. Classical rarefaction in divDyn
########################################----------------------------------------

# make it a data.frame to conform with divdyn input expectation
samp <- data.frame(specimens, stringsAsFactors=FALSE)

# Richness subsampled to 10 individuals, number of iterations is 1000
# #tax is the parameter that sets the column name in which the taxon names are!
divDyn::subsample(samp, tax="specimens", q=10, FUN=richness, iter=1000)

# Exercise 3: replace the function in the iteration (1E) with divDyn::subsample!

########################################----------------------------------------
# 2B. SQS in divDyn and Coverage-based rarefaction in iNEXT
########################################----------------------------------------

# using iNEXT
iNEXT::estimateD(counts, base="coverage", level=0.7)

# includes the frequency correction (singletons="occ")
divDyn::subsample(samp, tax="specimens", q=0.7, FUN=richness, iter=1000, type="sqs")

# Exercise 4: compare with Steve Holland's sqs script!

