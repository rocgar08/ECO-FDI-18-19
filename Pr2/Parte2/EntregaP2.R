#1. PREPARAR LOS DATOS LEYENDO EL ARCHIVO sarprueba.csv Y VISUALIZAR SU ESTRUCTURA

datosSAR<-read.csv("sarprueba.csv", stringsAsFactors=FALSE, dec=",")
print(datosSAR)

# PREGUNTA 1: Â¿CuÃ¡l es la frecuencia del monitor? 
#  Â¿CuÃ¡ntos registros y cuÃ¡ntas variables hay? 

#Observamos que la frecuencia de refresco es de dos segundos, donde hay 30 registros
#siendo el 31 el calculo de la media, y 8 variables.
# PREGUNTA 2: CuÃ¡l es el tiempo de monitorizaciÃ³n en total?

#Se monitoriza durante un minuto, aunque la primera medida es a las 11:58:23 y la 
#ultima 11:59:21, podriamos decir que realmente se monitorizan 58 segundos 

#2. INSTALAR LA LIBRERÃ�A DPLYR DE TIDYVERSE DESDE PACKAGES
# UTLIZAR HELP PARA CONOCER M?S SOBRE LA LIBRER?A
library(dplyr)

#3. PREGUNTA 3: Â¿QUÃ HACE LA SIGUIENTE SENTENCIA?
datosSAR<-rename(datosSAR, Horas=X11.58.21)

#Cambia x11.58.21 por Horas

#4. PREGUNTA 4: Â¿QUÃ HACEN LAS SIGUIENTES SENTENCIAS?
mediasSAR<-datosSAR[31,] #Guarda la fila 31 en mediasSar
datosSAR<-datosSAR[-31,] #Guarda todo menos la fila 31 en datosSar
datosSAR<-datosSAR[,-2] #Quita la segunda columna (la de la CPU)

#5. PREGUNTA 5: Â¿QUÃ HACEN LAS SIGUIENTES SENTENCIAS?
datosUS<-select(datosSAR, X.user, X.system) #Guarda solo los datos del us y del sys
plot(datosUS$X.user, type='l', col='red') #Crea una gráfica en funcion de x.user
plot(datosUS$X.system,type='l',col='blue') #Crea una gráfica en funcion de x.System
hist(datosUS$X.user, col='red') #Histograma en funcion del usuario
hist(datosUS$X.system,col='blue') #Histograma en funcion del system

#6. PREGUNTA 6: Â¿QUÃ HACEN LAS SIGUIENTES SENTENCIAS?
datosUS<-datosUS[-25,] #Quita la fila 25
X<-datosUS$X.user #Guarda en x x.user
Y<-datosUS$X.system #Guarda en y x.system(

plot(X, Y, type="p", xlab="USUARIO", ylab="SISTEMA",col="red")
#Crea una grafica en funcion del usuario(x) y del sistema(y)
title("Relacci?n %usuario y %sistema") #Añade titulo

# 7. PREGUNTA 7: Â¿QUÃ HACEN LAS SIGUIENTES SENTENCIAS?
msistema<-mean(datosUS$X.system) #Hace la media aritmetica recortada y la guarda
str(mediasSAR) #Muestra mediaSar
msistemareal<-mediasSAR$X.system #Guarda la media del sistema
musuario<-mean(datosUS$X.user) #Calcula la media del usuario (recortada)
musuarioreal<-mediasSAR$X.user #Guarda la media del usuario 
df.sistema<-data.frame(msistema, msistemareal)#Crea una tabla para comparar la real con la recortada
df.usuario<-data.frame(musuario, musuarioreal) #igual
df.tabla<-data.frame(df.sistema, df.usuario) #Crea una tabla para comparar las dos anteriores
ErrorUsuarioReal<-abs(mean(datosSAR$X.system)-musuarioreal) #Calcula el error
ErrorUsuarioSuavizado<-abs(mean(datosSAR$X.user)-musuario) #Calcula el error suavizado
print(df.tabla)
print(ErrorUsuarioReal)
print(ErrorUsuarioSuavizado)