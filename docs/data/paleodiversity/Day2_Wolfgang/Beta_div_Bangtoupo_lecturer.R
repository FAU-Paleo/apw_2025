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

# This has both genus and species names, which is ok
# Identification done at species level
#dat <- subset(dat, identified_rank=="species")

# use accepted_name as default
 dat$species <- dat$accepted_name

# replace with identified name if species authority is not present
 dat$species[dat$accepted_rank=="genus"] <- dat$identified_name[dat$accepted_rank=="genus"]

# check for mistakes/typos
sort(unique(dat$species))

# name doesn't matter, but they need to be the same
dat$species[dat$species=="Triassocirrus moellendorfii"] <- "Triassocirrus moellendorfi"
dat$species[dat$species=="Pseudomyoconcha maximilianileuchtenbergensis"] <- "Pseudomyoconcha maximilianleuchtenberensis"
dat$species[dat$species=="Pseudomyoconcha maximilianleuchtenbergensis"] <- "Pseudomyoconcha maximilianleuchtenberensis"
dat$species[dat$species=="Delphinulopsis binodusa"] <- "Delphinulopsis binodosa"
dat$species[dat$species=="Cassianella subcislonensis"] <- "Cassianella subcicloensis"
dat$species[dat$species=="Hologyra guizhui"] <- "Hologyra guizhoui"
dat$species[dat$species=="Hartmanina ? bangtoupoensis"] <- "Hartmanina bangtoupoensis ?"

# every species/collection combination should be there only once!
tapply(INDEX=dat$collection_no, X=dat$species, FUN=function(x) all(table(x)==1))
# issues with 31620 - ideally these would need to be cleaned

# make occurrence table
occ.tab <- table(dat$collection_no, dat$species)

# easier to understand
class(occ.tab) <- "matrix"

# Use the dist function first (binary)
dis <- dist(occ.tab) # Euclidean distance
dis <- dist(occ.tab, method="binary") # This is Jaccard dissimilarity (check) 


# A simple cluster analysis
clus <- hclust(dis) # "complete" is the default
plot(clus)

X11()
clus <- hclust(dis, "ward.D2") # recommended method
plot(clus, hang=-1) 


# Get abundance values in for further analyses ...
abun.tab <- occ.tab

# loop through all occurrences and rewrite occurrence record counts with abundances
for(i in 1:nrow(dat)){
    abun.tab[as.character(dat$collection_no[i]),dat$species[i]] <- dat$abund_value[i]
}



########################
  ##### alpha diversity #####
H <- diversity(abun.tab)
S <- specnumber(abun.tab)
N <- rowSums(abun.tab)
test <- rarefy(abun.tab, seq(5,30,by=5)) # rarefaction

# ditch low diversity sample 
 rar.tab <- abun.tab[-1,]
 test <- rarefy(rar.tab, seq(5,100,by=5)) # rarefaction

 x <- seq(5,100,by=5)
 plot(x, test[1,], type="l", col=colors()[547], xlab="Quota", ylab="S",
      main="Rarefaction of Bangtoupo samples")
 text(101, max(test[1,]), labels=gr[1])
  for(i in 2:12) {
    lines(x, test[i,], col=colors()[547+i])
    text(101, max(test[i,]), labels=gr[i])
  }
    
p.abun <- prop.table(abun.tab, 1)
# rowSums(p.abun)

# Test predictive value of Berger Parker
berger <- apply(p.abun, 1, max)
  plot(berger, H, pch=19)
   abline(lm(H ~ berger))

   st <- cor.test(H, berger)
    R <- round(st$estimate, 2)
    p <- round(st$p.value, 4)
    text(0.5, 3.8, labels=paste("R = ", R, "; p =", p), cex=1.4)

    
    ##### Beta diversity ####
    Beta <- ncol(abun.tab)/mean(S)


X11()
dis <- vegdist(p.abun)
clus <- hclust(dis, "ward.D2") # recommended method
plot(clus, hang=-1) 

r <- vegdist(t(p.abun))



mds <- metaMDS(dis, k=2)
plot(mds)
stressplot(mds)

# R mode and Q mode combined
library(paleotree)
twoWayEcologyCluster(r, dis, propAbund=t(p.abun), clustMethod="ward.D2")

# assign group numbers to samples following Q-mode cluster
gr <- rep(1, nrow(p.abun))

# High samples
 sel <- seq(31618,31622)
gr[row.names(p.abun) %in% sel] <- 2

 # mid samples
 sel <- c(31610, 31611, 31616, 31617)
 gr[row.names(p.abun) %in% sel] <- 3

 ## alternative grouping - this is significant
 # assign group numbers to samples following Q-mode cluster
 gr <- rep(1, nrow(p.abun))
 
 # High samples
 sel <- seq(31618,31622)
 gr[row.names(p.abun) %in% sel] <- 2
 
 
 boxplot(H ~ gr)
  wilcox.test(H[gr==1], H[gr==2])
 
 
  
  
  
 plot(mds)
 ordihull(mds, gr, display="sites", draw="polygon", 
          col=c("red", "blue", "green")) 
  adonis2(dis ~ gr)
  groups <-  anosim(p.abun, gr)
  summary(groups)
  plot(groups)
  
  
  #######
  # Do specific 
  