# 1. LEE, COMENTA Y COMPLETA PARA CONSEGUIR 
# CALCULAR EL INTERVALO DE CONFIANZA PARA LA MEDIA DE LAS DIFERENCIAS 
# DE LOS DATOS A NIVEL alfa:
#Pasos hechos en clase 
#Calcular las medias de la diferencias y diferencias al cuadrado
#Varianza S^2 = E[di^2]-E[di]^2
#Calcular la t de student 1-alfa/2,n-1
#Hacer la desviación típica mediad +- t *S/RAIZ(N)
ICalpha<-function(ModeloA, ModeloB, alfa)
{
  n<-length(xxxx)
  diferencias<-ModeloA-ModeloB
  mediad<-mean(diferencias) #calcular la media de las diferencias
  mediad2<-mean(diferencias^2)#calcular la media de las diferencias al cuadrado
  #s<-sqrt(var(diferencias))#calcula la varianza
  s<-sqrt(mediad2-mediad^2)#Calcula la varianza
  valort<-qt(alfa/2,n-1,lower.tail = F)#Calcula la t de student
  valor<-valort *(s/sqrt(n))#Calcular la parte de la varianza / raiz(n)
  cotaInf<-mediad-valor 
  cotaSup<- mediad + valor
  df<-data.frame(cotaInf, cotaSup)
  return(df)
}

# 2. ESTOS SON LOS DATOS DEL EJERCICIO 8. APLICA LA FUNCION ANTERIOR 
# CON 3 NIVELES DE CONFIANZA: alfa=0.05, alfa=0.01 y alfa = 0.1
# OBSERVA Y COMENTA LOS RESULTADOS
ModeloA<-c(23.6,33.7,10.1,12.9,67.8,9.3,47.4,54.9)
ModeloB<-c(24,41.6,8.7, 13.5, 66.4,15.2, 50.5, 52.3)
plot(ModeloA, ModeloB)

#Este es un ejemplo:
IC<-ICalpha(ModeloA, ModeloB, 0.1)
print('Intervalo de confianza al 90%')
IC
xxxxxxxxx
xxxxxxxxx
xxxxxxxxx
xxxxxxxxx

# 3. RESUELVE el EJERCICIO 9  UTILIZANDO LA FUNCION ANTERIOR. 

ModeloA9<-c(58.2,32.1,42.9,11,54.2,46.6,49.3,25.8,52)
ModeloB9<-c(43.6,21.3,32.8,8.2,40.2,45.1,46.3,32.8,58.3)
xxxxxxxxx
xxxxxxxxx
xxxxxxxxx
xxxxxxxxx


# 4. RESUELVE el EJERCICIO 10  UTILIZANDO LA FUNCION ANTERIOR. 
ModeloA10<-c(228,213,198,239,217,245)
ModeloB10<-c(182,181,276,122,105,298)
xxxxxxxxx
xxxxxxxxx
xxxxxxxxx
xxxxxxxxx