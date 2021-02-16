#################################################################
#Title: Posición Punto respecto a Recta utilizando Determinante
#Authors: Iván Martín Gómez and Markos Aguirre Elorza
#Date: Saturday 13th February 2021
#Descriptions: Dado un punto p=(p1,p2) y una recta y=mx+b, decidir si el punto
#está por encima, debajo o sobre la recta. No se contempla el caso
#de rectas verticales
#################################################################
funcion_PuntoRecta_Determinante <- function(coordenada_x_punto = NULL,coordenada_y_punto = NULL, pendiente_recta = NULL, ordenada_origen_recta = NULL){
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
  y1=p2
  y2=m*p1+b
  if(y1<y2){
    y=(y1:y2)
  }else{
    y=(y2:y1)
  }
  plot(x,m*x+b, type='l', xlab="x", ylab="y", col="blue")
  points(p1,p2, col = "red", pch=19)
  text(p1-1,(m*(p1-1)+b)+2,'arriba')
  text(p1-1,(m*(p1-1)+b)-2,'abajo')
  
  #Construimos punto a=(a1,a2) y dibujamos
  a=c(p1,m*p1+b)
  points(p1,m*p1+b, col = "yellow", pch=19)
  #Construimos punto c=(c1,c2); c1>a1 y dibujamos
  c=c(p1+1,m*(p1+1)+b)
  points(p1+1,m*(p1+1)+b, col = "yellow", pch=19)
  #Construimos vector ac  y dibujamos
  ac=a-c
  lines(c(a[1],c[1]),c(a[2],c[2]), col="green")
  #Construimos vector ap  y dibujamos
  ap=c(a[1]-p1,a[2]-p2)
  lines(c(a[1],p1),c(a[2],p2), col="green")
  #Montamos la matriz A
  A<-matrix(c(ac,ap), nrow=2, ncol=2, byrow = TRUE)
  #Calculamos determinante de la matriz A
  determinante=det(A)
  #Comprobamos signo determinate para decidir si el punto p esta arribo, abajo o
  #sobre la recta
  if(determinante>0){
    cat("El punto (",p1,",",p2,")"," está por ENCIMA de la recta", "y=",m,"x","+",b)
  }else if(determinante<0){
    cat("El punto (",p1,",",p2,")"," está por DEBAJO de la recta", "y=",m,"x","+",b)
    
  }else{
    cat("El punto (",p1,",",p2,")"," está por SOBRE de la recta", "y=",m,"x","+",b)
  }
}
#Probamos la función
rm(list=ls())#Limpia Global Environment
dev.off()#Limpia los plots
#Problema 1: p=(1,2), recta == y=3x+2
funcion_PuntoRecta_Determinante(1,2,3,2)
#Problema 2: p=(-1,3), recta == y=3x+2
funcion_PuntoRecta_Determinante(-1,3,3,2)
#Problema 3: p=(1,4), recta == y=3x+2
funcion_PuntoRecta_Determinante(1,4,3,2)
#Problema 4: p=(1,1), recta == y=5x+1
funcion_PuntoRecta_Determinante(1,1,5,1)
#Problema 5: p=(2,3), recta == y=-2x+1
funcion_PuntoRecta_Determinante(2,3,-2,1)
#Problema 6: p=(0,-4), recta == y=-2x+1
funcion_PuntoRecta_Determinante(0,-4,-2,1)
