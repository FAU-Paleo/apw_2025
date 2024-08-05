# Beta Diversity
# Script for course

library(vegan)

# download from PBDB Middle Triassic community data from Stiller (2001) in South China
# Bangtoupo, Qingyan, China: Pelsonian - Illyrian, China

url <- "https://paleobiodb.org/data1.2/occs/list.csv?coll_id=31610,31611,31612,31613,31614,31615,31616,31617,31618,31619,31620,31621,31622&show=class,abund,ecospace"
temp <- read.csv(file = url)

dat <- subset(temp, accepted_rank=="genus" | accepted_rank=="species")

table(dat$collection_no)

# Create a species-by-site matrix
occ.tab <- table(dat$collection_no, dat$accepted_name)

# Use the dist function first (binary)
dis <- dist(occ.tab) # Euclidean distance
dis <- dist(occ.tab, method="binary") # This is Jaccard dissimilarity (check) 


# A simple cluster analysis
clus <- hclust(dis) # "complete" is the default
plot(clus)

X11()
clus <- hclust(dis, "ward.D") # recommended method
plot(clus, hang=-1) 


# Get abundance values in for further analyses ...
