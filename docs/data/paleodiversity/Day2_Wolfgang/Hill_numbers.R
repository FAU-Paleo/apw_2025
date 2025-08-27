# Hill numbers
# Species abundances in two communities
comm <- data.frame(
  species = paste0("Sp", 1:5),
  A = c(10, 20, 30, 25, 15),
  B = c(1, 1, 1, 1, 96)
)
comm


hill_number <- function(p, q) {
  p <- p[p > 0] / sum(p)
  if (q == 1) {
    return(exp(-sum(p * log(p))))
  } else {
    return((sum(p^q))^(1 / (1 - q)))
  }
}
abund_A <- comm$A
hill_number(abund_A, 0) # richness
hill_number(abund_A, 1) # e^Shannon
hill_number(abund_A, 2) # Reciprocal Simpson

abund_B <- comm$B
hill_number(abund_B, 0) # richness
hill_number(abund_B, 1) # Shannon
hill_number(abund_B, 2) # Simpson


# Test
p.comm <- proportions(as.matrix(comm[,2:3]), margin=2)

term <- log(p.comm)*p.comm
H <- -colSums(term)
exp(H)

term2 <- p.comm^2
D <- colSums(term2)
1/D


# Diversity profiles

qs <- seq(0, 4, by = 0.1)
profile <- data.frame(
  q = qs,
  A = sapply(qs, function(q) hill_number(comm$A, q)),
  B = sapply(qs, function(q) hill_number(comm$B, q))
)

library(ggplot2)
ggplot(profile, aes(x = q)) +
  geom_line(aes(y = A, color = "Community A")) +
  geom_line(aes(y = B, color = "Community B")) +
  labs(y = "Effective number of species", color = "Community") +
  theme_minimal()



# Crossing diversity profiles
# Community A: high richness, very uneven (one dominant)
A <- c(60, 5, 5, 5, 5, 5, 5, 5, 5, 5)     # S = 10

# Community B: lower richness, very even
B <- c(17, 17, 17, 16, 15, 12)            # S = 6

# Community C: intermediate richness and evenness
C <- c(30, 25, 15, 10, 8, 6, 4, 2)        # S = 8


hill_number <- function(x, q){
  p <- x[x>0] / sum(x)
  if (q == 1) exp(-sum(p * log(p))) else (sum(p^q))^(1/(1 - q))
}



