#1. OBSERVA EL CÓDIGO ABAJO. INCLUYE COMENTARIOS DICIENDO PARA QUÉ SIRVEN 
# LAS TRES FUNCIONES. COMENTA TAMBIÉN LAS LÍNEAS DE CODIGO RESTANTES
#---------------------------------
mean1<-function(datos)
{return(sum(datos)/length(datos))}
#---------------------------------
mean2<-function(datos)
  {cuenta<-sum(datos^-1)
  return(length(datos)/cuenta)
}
#---------------------------------
mean3<-function(datos)
{cuenta<-prod(datos)
return(cuenta^(1/length(datos)))
}
#---------------------------------
datos<-c(1,2,3,4,5,6)
m1<-mean1(datos)
m2<-mean2(datos)
m3<-mean3(datos)
m1
m2
m3
# OBSERVA TU WORKSPACE Y COMPRUEBA LA COHERENCIA DE LOS RESULTADOS
#---------------------------------

#2. Generaliza una de las funciones anteriores añadiendo un parámetro adicional
# como se indica en la cabecera de la función a continuación.
# Implementa una función que sirva para resolver la primera parte del ejercicio 4
# del bloque ANALISIS COMPARATIVO. Los datos se proporcionan:
datos<-c(1,10,100)
pesos<-c(0.3,0.2,0.5)
# ten en cuenta que las operaciones entre vectores con el mismo número de elementos
# se realizan elemento a elemento, haz alguna prueba para comprobar el resultado
mean4<-function(datos,pesos)
  {ratios<-*****************
   cuenta<-*****************
   return(1/cuenta)
}

sol<- mean4(datos, pesos)
# Comprueba la solución obtenida con la solución de clase.
sol