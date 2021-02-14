###############################################################################
#Title: Punto_Recta utilizando Dibujo y criterio visual Usuario
#Author: Iván Martín Gómez
#Date: Saturday 13th February 2021
#Descriptions: Dado un punto p=(p1,p2) y una recta y=mx+b, decidir si el punto
#              está por encima, debajo o sobre la recta. No se contempla el caso de
#              de rectas verticales
###############################################################################

funcion_PuntoRecta_Dibujo <- function(coordenada_x_punto = NULL,coordenada_y_punto = NULL, pendiente_recta = NULL, ordenada_origen_recta = NULL){
  
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
  plot(x,m*x+b, type='l', xlab="x", ylab="y=mx+b", col="blue")
  points(p1,p2, col = "red", pch=19)
  text(p1-1,(m*(p1-1)+b)+2,'arriba')
  text(p1-1,(m*(p1-1)+b)-2,'abajo')
  
}

#Probamos la función
rm(list=ls())#Limpia Global Environment
dev.off()#Limpia los plots
#Ejemplo 1: p=(1,2), recta == y=3x+2
funcion_PuntoRecta_Dibujo(1,2,3,2)
