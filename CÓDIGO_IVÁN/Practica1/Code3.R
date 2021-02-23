#################################################################
#Title: Posición Punto respecto a Recta utilizando Dibujo y criterio visual Usuario
#Authors: Iván Martín Gómez and Markos Aguirre Elorza
#Date: Saturday 13th February 2021
#Descriptions: Dado un punto p=(p1,p2) y una recta y=mx+b, decidir si el punto
#está por encima, debajo o sobre la recta. No se contempla el caso de
#de rectas verticales
#################################################################
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
  x1=p1-2
  x2=p1+2
  x=(x1:x2)
  y1=funcion_construir_recta_con_pendiente_ordenada_origen()
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
  
}

#Probamos la función
rm(list=ls())#Limpia Global Environment
dev.off()#Limpia los plots
#Problema 1: p=(1,2), recta == y=3x+2
funcion_PuntoRecta_Dibujo(1,2,3,2)
#Problema 2: p=(-1,3), recta == y=3x+2
funcion_PuntoRecta_Dibujo(-1,3,3,2)
#Problema 3: p=(1,4), recta == y=3x+2
funcion_PuntoRecta_Dibujo(1,4,3,2)
#Problema 4: p=(1,1), recta == y=5x+1
funcion_PuntoRecta_Dibujo(1,1,5,1)
#Problema 5: p=(2,3), recta == y=-2x+1
funcion_PuntoRecta_Dibujo(2,3,-2,1)
#Problema 6: p=(0,-4), recta == y=-2x+1
funcion_PuntoRecta_Dibujo(0,-4,-2,1)
