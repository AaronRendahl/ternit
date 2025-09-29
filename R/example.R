###
if(FALSE) {
alim <- c(0, 0.5) #range(datxy[["h"]])
blim <- c(0, 0.6) #range(datxy[["0"]])
clim <- c(0.3, 1) #range(datxy[["s"]])
library(conflicted)
library(tidyverse)
conflicts_prefer(dplyr::filter, dplyr::lag, dplyr::summarize)



datxy <- ternex |> mutate(tern_xy(C, B, A))
ternit(datxy,
       labs = c("cc", "bb", "aa"),
       lims = rbind(c(0,0.5),c(0,0.6),c(0.3,1)),
       grid.spacing=0.1, tick.length=0.02) +
  aes(fill=Quad) +
  geom_point(pch=21) +
  #scale_fill_discrete(name=NULL) +
  scale_fill_brewer(palette="RdYlBu", name=NULL)


facets <- distinct(ternex, Trio, Quad)

#labs <- letters[1:3]

clim <- c(0, 0.5) #range(datxy[["C"]])
blim <- c(0, 0.6) #range(datxy[["B"]])
alim <- c(0.3, 1) #range(datxy[["A"]])
lims <- rbind(clim, blim, alim)

ternit(datxy, labs, lims=lims, grid.spacing=0.1, tick.length=0.02) +
  aes(fill=Quad) +
  geom_point(pch=21) +
  #scale_fill_discrete(name=NULL) +
  scale_fill_brewer(palette="RdYlBu", name=NULL)

}
