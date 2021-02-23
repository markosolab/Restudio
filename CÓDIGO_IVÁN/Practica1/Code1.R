###############################################################################
#Title: Punto_Recta utilizando desigualdades
#Author: Iván Martín Gómez
#Date: Saturday 13th February 2021
#Descriptions: Dado un punto p=(p1,p2) y una recta y=mx+b, decidir si el punto
#              está por encima, debajo o sobre la recta. No se contempla el caso de
#              de rectas verticales
###############################################################################

funcion_PuntoRecta_Desigualdades <- function(coordenada_x_punto = NULL,coordenada_y_punto = NULL, pendiente_recta = NULL, ordenada_origen_recta = NULL){

#Control de errores
if(is.null(ordenada_origen_recta)){
  cat("Error: no se contempla el caso de rectas verticales")
  return -1
}
p1=coordenada_x_punto
p2=coordenada_y_punto
m=pendiente_recta
b=ordenada_origen_recta


#Dibujamos
x1=p1-3
x2=p1+3
x=(x1:x2)
plot(x,m*x+b, type='l', xlab="x", ylab="y=mx+b", col="blue")
points(p1,p2, col = "red", pch=19)
text(-1,m*(-1)+b+2,'arriba')
text(1,m*(1)+b-2,'abajo')

#Salida función por ventana de comandos
if(p2>m*p1+b){
  cat("El punto (",p1,",",p2,")"," está por ENCIMA de la recta", "y=",m,"x","+",b)
}else if(p2<m*p1+b){
  cat("El punto (",p1,",",p2,")"," está por DEBAJO de la recta", "y=",m,"x","+",b)
  
}else{
  cat("El punto (",p1,",",p2,")"," está por SOBRE de la recta", "y=",m,"x","+",b)
}
}
#Probamos la función
#Probamos la función
rm(list=ls())#Limpia Global Environment
dev.off()#Limpia los plots
#Ejemplo 1: p=(1,2), recta == y=3x+2
funcion_PuntoRecta(1,2,3,2)
  



