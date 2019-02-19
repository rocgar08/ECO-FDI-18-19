datosTOP <- read.csv("topprueba.csv", stringsAsFactors = FALSE, dec = ",")
View(datosTOP)
str(datosTOP)
datosTOP<-datosTOP[-(1:2),]
datosTOP<-datosTOP[,-1]
plot(datosTOP$VIRT)
plot(datosTOP$X.CPU)
plot(as.factor(datosTOP$USUARIO),datosTOP$VIRT)
title("Uso de la memoria virtual")
X <- datosTOP$X.CPU
Y <- datosTOP$VIRT/(1024)
plot(X,Y,type = "l",xlab="CPU",ylab="Memoria Virtual",col = "red")
title("Relación entre memoria virtual y % de CPU")

