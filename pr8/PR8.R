#Rocío García y Rubén Izquierdo
#Practica 8
#Raid0
AlmaDisk<-40
p<-0.9 
n<-14 #nº discos
t<-1 #Expresado en años
#Raid 0
Raid0<-function(p,n){
  return(p^n)
}
PVRaid0<-Raid0(p,n)
PVRaid0
#---------------Mirroring----------------------------------
#14 hdd configuration RAID-1 array
Raid1<-function(p,n){
  return((2*(p)-(p^2))^n)
}
PVRaid1<-Raid1(p,n/2)
PVRaid1
#---------------Duplexing-------------------------------------
RSCSI<-0.99
#Para un PVRaid1 con 14 discos
#desde i = 1 to 7 (2(0.891)-0.891^2)
pAux=p*RSCSI
pAux
PVRaid1dup<-Raid1(pAux,n/2)
PVRaid1dup
#-----------------RAID 0 + 1-------------------------------------
Raid01<-function(p,n){
  return((1-(1-(p^2))*(1-(p^2)))^n)
}
PVRaid01<-Raid01(p,3)
PVRaid01
#----------------RAID 3-------------------------------------------
#a en el pdf es n
#b en el pdf es j
asobreb<-function(a,b){
  return(factorial(a)/(factorial(b)*factorial(a-b)))
}
#Se podría hacer con un for XD
a<-13
b<-12
Raid3<-function(a,b,p,n){
  parte1<-asobreb(a,b)*(p^(n-2))*((1-p)^(a-b))
  parte2<-asobreb(a,13)*(p^(n-1))*((1-p)^(a-13))
  return(parte1+parte2)
  
}
PVRaid3<-Raid3(a,b,p,n)
PVRaid3
#Ejercicio2
#p probabilidad
#i inicio
#m numElemfentos
m<-8
Edges<-function(p,i, m){
  comb<-c(i:m)
  res<-sum(asobreb(m,comb)*(p^comb)*(1-p)^(m-comb))
  return(res)
  #choose(m,comb)
}

EdgeRaid0<-Raid0(p,m)
EdgeRaid0

EdgeRaid1<-Raid1(p,m)
EdgeRaid1

EdgeRaid01<-Raid01(p,m)
EdgeRaid01

EdgeRaid1dup<-Raid1(pAux,m)
EdgeRaid1dup

EdgeRaid3<-Edges(p,3,m)
EdgeRaid3
#Ejercicio 3

PowerVault_raid<-c(PVRaid0,PVRaid1, PVRaid1dup, PVRaid01, PVRaid3)
PowerEdge_raid<-c(EdgeRaid0,EdgeRaid1,EdgeRaid1dup, EdgeRaid01, EdgeRaid3)
dfPV<-data.frame(c("Raid0", "Raid1", "Raidup", "Raid01", "Raid3"), PowerVault_raid)
plot(dfPV, type="h",main="Modelo PowerVault" ,xlab="Configuración RAID", ylab="Fiabilidad " )
dfPE<-data.frame(c("Raid0", "Raid1", "Raidup", "Raid01", "Raid3"), PowerEdge_raid)
plot(dfPE, type="h",main="Modelo PowerEdge", xlab="Configuración RAID", ylab="Fiabilidad" )

#Podemos decir que Raid3PowerEdge es que este último es completamente 
#fiable desde su inicio en 3 y con 8 discos durante 5 aaños
#y el Raid3PowerVault tiene una fiabilidad de 0,6 aprox de un 60%
