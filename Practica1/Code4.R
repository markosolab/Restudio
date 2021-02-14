###############################################################################
#Title: Procedimiento de los determinantes
#Author: Iván Martín Gómez
#Date: Saturday 13th February 2021
#Descriptions: Dados dos vectores con origen en el origen, construimos el
#              una matriz A que tiene como filas los vectores, dibujamos el paralelograma que
#              definen los dos vectores y calculamos el área del paralelogramo mediante el uso
#              del determinante de la matriz A
###############################################################################
funcion_procedimiento_determinantes <- function(v1,v2){
  
  #Construimos Matriz A
  dim1=length(v1)
  dim2=length(v2)
  if(dim1!=dim2){
    cat("Error, los vectores deben tener la misma longitud")
    return -1
  }
  A<-matrix(c(v1,v2), nrow=dim1, ncol=dim2, byrow = TRUE)
  x <- c(0,A[1,1],A[1,1]+A[2,1],A[2,1])
  y <- c(0,A[1,2],A[1,2]+A[2,2],A[2,2])
  min_x=min(x)-1
  max_x=max(x)+1
  min_y=min(y)-1
  max_y=max(y)+1
  #Dibujamos
  plot(0,0,xlim=c(min_x,max_x),ylim=c(min_y,max_y),main = paste("Poligono"))
  points(A[1,1],A[1,2], main = paste("Poligono"))
  points(A[2,1],A[2,2], main = paste("Poligono"))
  points(A[1,1]+A[2,1],A[1,2]+A[2,2], main = paste("Poligono"))
  
  polygon(x, y, col = "orange", lty = 1, lwd = 2, border = "blue")
  area=det(A)
  cat("El área es: ",area)
}

#Probamos la función
rm(list=ls())#Limpia Global Environment
dev.off()#Limpia los plots
#Ejemplo 1:
v1=c(1,0)
v2=c(0,1)
funcion_procedimiento_determinantes(v1,v2)
#Ejemplo 2:
v1=c(0,1)
v2=c(1,0)funcion_procedimiento_determinantes(v1,v2)
#Ejemplo 3:
v1=c(1,2)
v2=c(-1,1)
funcion_procedimiento_determinantes(v1,v2)
#Ejemplo 4:
v1=c(-1,1)
v2=c(1,2)
funcion_procedimiento_determinantes(v1,v2)
