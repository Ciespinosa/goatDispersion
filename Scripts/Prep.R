##Cargamos los datos
library(readxl)

dta_par <- read_excel("Data/B.datos.xls", sheet = "parcelas")
dta_cor <- read_excel("Data/B.datos.xls", sheet = "corrales")
dta_sem <- read_excel("Data/B.datos.xls", sheet = "SemillasA ")

dta_arb <- read_excel("Data/Arboles Zapotillo.xlsx", sheet = "arboles")

#Eliminamos a Tabebuia y Ceiba de los corrales

dta_cor <- subset(dta_cor, dta_cor$Cientifico!= "Tabebuia billbergii")
dta_cor <- subset(dta_cor, dta_cor$Cientifico!= "Ceiba trichistandra")

## Incluimos una variable para unir dta_parcela y dta_arb

dta_arb$Sitio <- dta_arb$cluster
dta_arb$Sitio[dta_arb$Sitio=="C3"] <- "La Manga"
dta_arb$Sitio[dta_arb$Sitio=="C11"] <- "Balsa Real"
dta_arb$Sitio[dta_arb$Sitio=="C15"] <- "Sendero"

##Preparamos datos para el análisis
library(reshape2)
library(vegan)

parT <- dcast(dta_par, format(`Fecha de recolección`, "%Y/%m")~`Nombre Científico`, 
              value.var = "Número", fun.aggregate = sum)
parT <- parT[, -which(colnames(parT)=="Sin datos")]

corT <- dcast(dta_cor, format(`Fecha_recolección`, "%Y/%m")~ Cientifico)

##Descriptivo de que especies y en que abundancias
parSp <- data.frame(spp=colnames(parT)[-1], 
                    abunPar= colSums(parT[,-1]))
corSp <- data.frame(spp=colnames(corT)[-1], 
                    abunCor= colSums(corT[,-1]))

espDis <- merge(parSp, corSp, by="spp", all = T)

##Graficamos

barplot(as.matrix(t(espDis[,-1])), horiz = TRUE, log = "x")

tiff("Output/Dispersion.tiff", width = 20, height = 8,
     units = "cm", pointsize = 12, res = 300)
par(mfcol=c(1,2), mar=c(3,1,1,0.5), mgp=c(1.8,0.5,0), tck=-0.04)
layout(matrix(c(1,2), nrow = 1, ncol=2), widths = c(1,1.6))
barplot(espDis$abunPar/100, horiz = TRUE, xlim=c(100,0), 
        col="black", xlab="N. semillas/100", cex.lab=0.8, cex.axis=0.8)
mtext("Bosque", side = 3, at=20)
barplot(espDis$abunCor/100, horiz = TRUE, xlim=c(-60,100),
          col="darkgrey", border = "darkgrey", axes = F, 
        xlab="", cex.lab=0.8)
mtext("N. semillas/100", side = 1, at = 50, line=1.8, cex=0.8)
axis(1, at= 0:5*20, labels = 0:5*20, cex.axis=0.8)
text(x = rep(-35, 10), y = 0.6:10*1.2, 
     labels = espDis$spp, cex=0.8, font=3)
mtext("Corrales", side = 3, at=20)
dev.off()

##Temporal (revisar acacia)
corT2 <- dcast(dta_cor, SITIO+format(`Fecha_recolección`, "%Y/%m")~ Cientifico, length)

corT2$riq <- specnumber(corT2[,-(1:2)]) 
corT2$Abu <- rowSums(corT2[,-c(1,2,11)])
colnames(corT2)[2] <- "Fecha"
meanT <- data.frame(aggregate(corT2$riq, by=list(Fecha=corT2$Fecha), mean),
                    aggregate(corT2[,c(3,11)], by=list(Fecha=corT2$Fecha), mean)[,-1])
colnames(meanT) <- c("Fecha", "Riq", "A.macr", "Abun")

meanT$AbAc <- meanT$Abun-meanT$A.macr
  
sdT <- data.frame(aggregate(corT2$riq, by=list(Fecha=corT2$Fecha), sd),
                    aggregate(corT2[,c(3,11)], by=list(Fecha=corT2$Fecha), sd)[,-1])
colnames(sdT) <- c("Fecha", "Riq", "A.macr", "Abun")

sdT <- sdT[,-1]/sqrt(3)

tiff("Output/Temporal.tiff", width = 15, height = 13,
     units = "cm", pointsize = 12, res = 300)
par(mfcol=c(2,1), mar=c(1,3,1,1), oma = c(2,1,.5,0),
    mgp = c(2,0.5, 0), tck=-0.04)
barplot(as.matrix(t(meanT[,c(5,3)]/100)), cex.names=0.7, 
        col=c("black", "grey"), border=FALSE, las=1, 
        cex.axis = 0.9, ylim = c(0,18), ylab = "N. semillas/100",
        cex.lab=0.8)
arrows(0.6:6.6*1.2, meanT$Abun/100+sdT$Abun/100, 0.6:6.6*1.2, 
       meanT$Abun/100-sdT$Abun/100, 
       length = 0.07, code = 3, angle = 90)
mtext("A)", adj=0, line=0.3, font=2)

barplot(meanT$Riq, names.arg = meanT$Fecha, cex.names=0.7,
        col="grey60", las=1, cex.axis = 0.9, ylab = "N. especies", 
        cex.lab=0.8, border=FALSE, ylim=c(0,8))
arrows(0.6:6.6*1.2, meanT$Riq+sdT$Riq, 0.6:6.6*1.2, meanT$Riq-sdT$Riq, 
       length = 0.07, code = 3, angle = 90)

mtext("B)", adj=0, line=0.3, font=2)

dev.off()

##Comparar lo establecido vs lo dispersado
## las especies que disperan las cabras en el bosque corresponden
## a la disponibilidad o existe cierta preferencia

##todas las cacas en cualquier sitio tiene la misma composicion


##Efecto del paso por el tracto digestivo




