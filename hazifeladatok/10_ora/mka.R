library(mclust)

mka <- Mclust(wreath)
mka
plot(mka, "BIC", legendArgs = list(x = "topleft"))

mka <- Mclust(wreath, G = 10:20)
mka
plot(mka, "BIC", legendArgs = list(x = "bottomright"))

mka <- Mclust(wreath, G = 10:16, c("VVV", "VEE", "VVE", "EEV"))
plot(mka, "BIC", legendArgs = list(x = "bottomright"))


# ICL integrated complete data likelihood
ICLsima <- mclustICL(wreath, G = 10:20)
plot(ICLsima, "ICL", legendArgs = list(x = "bottomright"))
write.table(ICLsima, file = "ICLsima.txt", dec = ",", sep = "\t")
# NA ~ MKA nem konvergalt, vagz a folyamat beleszaladt egy ervenytelen modellbe
# ezt orvosolja a priorControl parameter

ICLprior <- mclustICL(wreath, G = 10:20, prior = priorControl())
plot(ICLprior, "ICL", legendArgs = list(x = "bottomright"))
?priorControl


# Baudry-fele elemzes
mcl <- Mclust(wreath, G = 10:20)
bmcl <- clustCombi(mcl)
bmcl
entPlot(bmcl$MclustOutput$z, bmcl$combiM, abc = "standard") # hol ugrik meg hirtelen az entropia
write.table(bmcl$classification[14], file = "Baudry.txt", col.names = F)
