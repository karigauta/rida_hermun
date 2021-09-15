# Baetur vegna bustofns i ridu

# Fastar
frjosemi <- c(0.7, 1.65,1.65,1.65,1.65, 1.65,1.65,1.65 )
ending   <- c(0.90,0.84,0.83,0.76,0.72,0.51,0.27,0
)
hjord.staerd <- 100
hjord.aldurdreif <-  c( 0.153364632,0.148669797,0.165884194,0.148669797,0.187793427,0.13458529,0.054773083,0.006259781)

hjord <- round(hjord.staerd*hjord.aldurdreif)
hjord.aldur <- matrix(0, ncol = lengd, nrow = 8)
framlegd <- 6913 # tekjur - breytilegur
lengd <- 8
afurdir <- matrix(0, ncol = lengd, nrow = 8 )
afvoxtun <- 0.025

# Simulation
RiduHermir <- function (lengd){
for (i in 1:lengd) {
  hjord.aldur[,i] <- hjord
    afurdir[,i] <- hjord*frjosemi*framlegd

  # lata gripi eldast um eitt timaskref
  for (j in 1:length(hjord)) {
    hjord[length(hjord)+1-j] <- ifelse(length(hjord)-j > 0,ceiling(hjord[length(hjord)-j]*ending[length(hjord)-j]), 0)
    # hjord[i] <- 0
  }

}
  npv <- rep(0, times = ncol(afurdir))
  for (n in 1:ncol(afurdir)) {
    npv[n] <- colSums(afurdir)[n]/(1+afvoxtun)^n
    }
return ( sum(npv)/hjord.staerd)
  }

