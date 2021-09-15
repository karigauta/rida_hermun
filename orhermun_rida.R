# Baetur vegna bustofns i ridu


# Simulation
RiduHermir <- function (hjord.staerd){
  frjosemi <- c(0.7, 1.65, 1.65, 1.65, 1.65, 1.65, 1.65, 1.65) # Lomb til nytja/grip
  ending   <-
    c(0.90, 0.84, 0.83, 0.76, 0.72, 0.51, 0.27, 0)   # Likur a ad gripur se ekki logad (heimild: tafla 1, https://skemman.is/bitstream/1946/22073/1/BS%20Linda%20Sif%20N%C3%ADelsd%C3%B3ttir.pdf)
  hjord.staerd <- 100                                # skiptir ekki mali, bara til einfoldunar aflestrar
  hjord.aldurdreif <-                                # upphaflega aldursdreifing (tekid ur ritgerd lindu sifar her ad ofan)
    c(
      0.153364632,
      0.148669797,
      0.165884194,
      0.148669797,
      0.187793427,
      0.13458529,
      0.054773083,
      0.006259781
    )
  framlegd <- 6913                                    # tekjur - breytilegur. Forsendur ANR i utreikningi bota v. lamba í fyrra
  lengd <- 8                                          # hamarksending a gripi, ekki gott ad breyta tharsem ending og allt hitt m.v. 8
  afvoxtun <- 0.025                                   # afvoxtunarstudull, notast vid verdbolgumarkmid her
  hjord <- round(hjord.staerd*hjord.aldurdreif)       # vigur sem heldur utan um aldursdreifingu innan ars
  hjord.aldur <- matrix(0, ncol = lengd, nrow = lengd)    # vigur sem heldur utan um aldursdreifingu oll arin
  afurdir <- matrix(0, ncol = lengd, nrow = lengd )       # vigur sem heldur utan um framlegd allra ara


for (i in 1:lengd) {
  hjord.aldur[,i] <- hjord
    afurdir[,i] <- hjord*frjosemi*framlegd

  # lata gripi eldast um eitt timaskref
  for (j in 1:length(hjord)) {
    hjord[length(hjord) + 1 - j] <-
      ifelse(length(hjord) - j > 0, ceiling(hjord[length(hjord) - j] * ending[length(hjord) -
                                                                                j]), 0)
  }

}
  npv <- rep(0, times = ncol(afurdir))                # vigur til ad halda utan um afvoxtud gildi
  for (n in 1:ncol(afurdir)) {
    npv[n] <- colSums(afurdir)[n]/(1+afvoxtun)^n      # standard net present value utreikningur
  }
return ( sum(npv)/hjord.staerd)                       # skila medal rekstrartapi a grip
  }

RiduHermir(100) #
