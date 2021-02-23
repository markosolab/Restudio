#################################################################
#Title: Posición Punto respecto a Recta utilizando proyecciones
#Authors: Iván Martín Gómez and Markos Aguirre Elorza
#Date: Saturday 13th February 2021
#Descriptions: Dado un punto p=(p1,p2) y una recta y=mx+b, decidir si el punto
#está por encima, debajo o sobre la recta. No se contempla el caso de
#de rectas verticales
#################################################################
funcion_PuntoRecta_Proyeccion <- function(coordenada_x_punto = NULL,coordenada_y_punto = NULL, pendiente_recta = NULL, ordenada_origen_recta = NULL){
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
  interseccion=m*p1+b
  puntos_vertical=rep(p1,length(y))
  plot(x,m*x+b, type='l', xlab="x", ylab="y", col="blue")
  lines(puntos_vertical,y, col="green")
  points(p1,p2, col = "red", pch=19)
  points(p1,interseccion, col = "yellow", pch=19)
  text(p1-1,(m*(p1-1)+b)+2,'arriba')
  text(p1-1,(m*(p1-1)+b)-2,'abajo')
  
  #Salida función por ventana de comandos
  if(p2>interseccion){
    cat("El punto (",p1,",",p2,")"," está por ENCIMA de la recta", "y=",m,"x","+",b)
  }else if(p2<interseccion){
    cat("El punto (",p1,",",p2,")"," está por DEBAJO de la recta", "y=",m,"x","+",b)
    
  }else{
    cat("El punto (",p1,",",p2,")"," está SOBRE de la recta", "y=",m,"x","+",b)
  }
}



#Probamos la función
rm(list=ls())#Limpia Global Environment
dev.off()#Limpia los plots
#Problema 1: p=(1,2), recta == y=3x+2
funcion_PuntoRecta_Proyeccion(1,2,3,2)
#Problema 2: p=(-1,3), recta == y=3x+2
funcion_PuntoRecta_Proyeccion(-1,3,3,2)
#Problema 3: p=(1,4), recta == y=3x+2
funcion_PuntoRecta_Proyeccion(1,4,3,2)
#Problema 4: p=(1,1), recta == y=5x+1
funcion_PuntoRecta_Proyeccion(1,1,5,1)
#Problema 5: p=(2,3), recta == y=-2x+1
funcion_PuntoRecta_Proyeccion(2,3,-2,1)
#Problema 6: p=(0,-4), recta == y=-2x+1
funcion_PuntoRecta_Proyeccion(0,-4,-2,1)
