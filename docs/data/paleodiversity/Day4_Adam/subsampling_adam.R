# Basics written up here:
# https://evolv-ed.net//articles/2023-03-31_subsample-1.html
# This is the script written by Adam during the lecture (Aug 21)

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
multipleEstimates

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
q <- 10 # quota
iter <- 1000

# container to the subsampling trial results
result <- rep(NA, iter) # better to allocate some space beforehand!
# result <- NULL
# result <- numeric()

# iteration for every trial
for(i in 1:iter){
	# create a trial subset (sample)
	trialSet <- sample(specimens, q, replace=FALSE)

	# calculate something from trial subset (number of species)
	result[i] <- length(unique(trialSet))

	# loop counter
	cat(i, "\r")
	flush.console()
}
# calculate the expectation
#hist(result)
mean(result)

########################################----------------------------------------
# 1C. Classical Rarefaction (subsampling manually implemented, with applied function)
########################################----------------------------------------

# The applied function
#' @param x A vector of entities coded as characters
#' @return A single numeric value.
richness <- function(x) length(unique(x))

# when applied to the original data
richness(specimens)

# use this function to calculate richness!
# parameters
q <- 10 # quota
iter <- 1000

# container to the subsampling trial results
result <- rep(NA, iter)
# result <- NULL
# result <- numeric()

# iteration for every trial
for(i in 1:iter){
	# create a trial subset (sample)
	trialSet <- sample(specimens, q, replace=FALSE)

	# calculate something from trial subset (number of species)
	result[i] <- richness(trialSet)

	# loop counter
	cat(i, "\r")
	flush.console()
}
# calculate the expectation
#hist(result)
mean(result)

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
CR <- function(x,q,iter, FUN, counter=TRUE){
	# container
	result <- rep(NA, iter)
	# result <- NULL
	# result <- numeric()

	# iteration for every trial
	for(i in 1:iter){
		# create a trial subset (sample)
		trialSet <- sample(x, q, replace=FALSE)

		# calculate something from trial subset (number of species)
		result[i] <- FUN(trialSet)

		# loop counter
		if(counter){
			cat(i, "\r")
			flush.console()
		}
	}
	# calculate the expectation
	#hist(result)
	return(mean(result))
}

# call to the function
CR(x=specimens, q=10, iter=100000, FUN=richness)

########################################----------------------------------------
# 1E. Classical rarefaction (subsampling) - calculated for multiple qs
########################################----------------------------------------

# Exercise 1: Use a for loop to iterate the function for every q from q=1 to q=length(specimens)! (5 min)

# the target quotas
target <- 1:length(specimens)
# target <- c(10, 40, 55, 80)

# define a container to store the results
expectations <- rep(NA, length(target))

#iterate for values between 1 - length of specimens
for(i in 1:length(target)){
	# call to CR, and store
	expectations[i] <- CR(x=specimens, q=target[i], iter=1000, FUN=richness, counter=FALSE)
	#
	cat(i, "\r")
	flush.console()
}

# plot the result
plot(x=target, y=expectations, xlab="Sample size", ylab="Number of species")


# Exercise 2:  Try to calculate the subsampled Shannon's entropy! This is implemented in vegan::diversity!
# NOTE: The input to this function needs to be a vector of counts, so you have to wrap
# vegan::diversity in a function that translates the subsampling trial data/subsample to counts!
# (hint: the function to do this was used in this script!)
library(vegan)
vegan::diversity(counts)

# NOTE: iNEXT gives you Hill numbers, i.e. the effecive number of species based on this.
# the observed sample (with 107 specimens), we wa
estimateD(counts, level=sum(counts))

# the first order Hill number is the same as
exp(vegan::diversity(counts))

# Getting the function to work!
# we want to use it in this, but...
CR(x=specimens, q=50, iter=10000, FUN=richness, counter=FALSE)

#it gives us an error ,
# CR(x=specimens, q=50, iter=10000, FUN=vegan::diversity, counter=FALSE)

# trialSet is a character vector - we used this when we developed the function
# an example shoudl still lurk around in the workspace
# # needs to be table-d before entering vegan::diversity
diversity(table(trialSet))

# new wrapper function is defined with this
DiversityFromSpecimens <- function(x) vegan::diversity(table(x))
DiversityFromSpecimens(specimens)

# this should be the same as the iNEXT output
diversity50 <- CR(x=specimens, q=50, iter=1000, FUN=DiversityFromSpecimens, counter=FALSE)
exp(diversity50)

# close enough!
iNEXT::estimateD(counts, level=50)[2, "qD"]

########################################----------------------------------------
# 2A. Classical rarefaction in divDyn
########################################----------------------------------------

# very similar to the example function above
# make it a data.frame to conform with divdyn input expectation
samp <- data.frame(specimens, stringsAsFactors=FALSE)

# Richness subsampled to 10 individuals, number of iterations is 1000
# #tax is the parameter that sets the column name in which the taxon names are!
divDyn::subsample(samp, tax="specimens", q=10, FUN=richness, iter=1000)

# Exercise 3: replace the function in the iteration (1E) with divDyn:
target <- 2:length(specimens)
# target <- c(10, 40, 55, 80)

# define a container to store the results
expectations <- rep(NA, length(target))

#iterate for values between 1 - length of specimens
for(i in 1:length(target)){
	# call to CR, and store
	expectations[i] <- divDyn::subsample(x=samp, tax="specimens", q=target[i], iter=10000, FUN=richness, counter=FALSE)
	#
	cat(i, "\r")
	flush.console()
}
# plot the result
plot(x=target, y=expectations, xlab="Sample size", ylab="Number of species")


########################################----------------------------------------
# 2B. SQS in divDyn and Coverage-based rarefaction in iNEXT
########################################----------------------------------------

# using iNEXT
iNEXT::estimateD(counts, base="coverage", level=0.7)

# includes the frequency correction (singletons="occ")
divDyn::subsample(samp, tax="specimens", q=0.7, FUN=richness, iter=1000, type="sqs")

# Exercise 4: compare with Steve Holland's sqs script! - give it a try
