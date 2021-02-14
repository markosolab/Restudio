###############################################################################
#Title: Ejercicio área paralelogramo descomponiendo en triángulos
#Author: Iván Martín Gómez
#Date: Saturday 13th February 2021
#Descriptions: Descomponemos el paralelogramo en dos triángulos y mediante la
#              fórmula conocida para obtener el área de un triangulo, calculamos
#              el área del paralelogramo.
#              ÁreaParalelogramo= ÁreaTriángulo1 + ÁreaTriángulo2
#              donde: ÁreaTriánguloi= (base x altura) /2
#              Tomamos como base la longitud del vector suma para ambos triángulos
#              Tomamos como altura triángulo 1 la longitud del segmento de recta
#              perpendicular a la diagonal que pasa por el vértice definido por v1
#              Tomamos como altura triángulo 2 la longitud del segmento de recta
#              perpendicular a la diagonal que pasa por el vértice definido por v2
###############################################################################
funcion_procedimiento_triangulos <- function(v1,v2){
dim1=length(v1)
dim2=length(v2)
#Control de errores
  if(dim1!=dim2){
    cat("Error, los vectores deben tener la misma longitud")
    return -1
  }
A<-matrix(c(v1,v2), nrow=dim1, ncol=dim2, byrow = TRUE)
#Dibujamos
  x_s_triangulo1 <- c(0,A[1,1],A[1,1]+A[2,1])
  y_s_triangulo1 <- c(0,A[1,2],A[1,2]+A[2,2])
  x_s_triangulo2 <- c(0,A[2,1],A[1,1]+A[2,1])
  y_s_triangulo2 <- c(0,A[2,2],A[1,2]+A[2,2])
  
  min_x_triangulo1=min(x_s_triangulo1)-1
  max_x_triangulo1=max(x_s_triangulo1)+1
  min_y_triangulo1=min(y_s_triangulo1)-1
  max_y_triangulo1=max(y_s_triangulo1)+1
  
  min_x_triangulo2=min(x_s_triangulo2)-1
  max_x_triangulo2=max(x_s_triangulo2)+1
  min_y_triangulo2=min(y_s_triangulo2)-1
  max_y_triangulo2=max(y_s_triangulo2)+1
  
  min_x=min(c(min_x_triangulo1,min_x_triangulo2))
  max_x=min(c(max_x_triangulo1,max_x_triangulo2))
  min_y=min(c(min_y_triangulo1,min_y_triangulo2))
  max_y=min(c(max_y_triangulo1,max_y_triangulo2))
  
  plot(0,0,xlim=c(min_x,max_x),ylim=c(min_y,max_y),main = paste("Triangulos"))
  points(A[1,1],A[1,2])
  points(A[1,1]+A[2,1],A[1,2]+A[2,2])
  polygon(x_s_triangulo1, y_s_triangulo1, col = "orange", lty = 1, lwd = 2, border = "blue")
  
  points(A[2,1],A[2,2])
  polygon(x_s_triangulo2, y_s_triangulo2, col = "green", lty = 1, lwd = 2, border = "blue")
  
  #Obtenemos pendiente 'm' y ordenada en el origen de la recta que contiene al vector suma
  #En este caso particular estamos ante una recta vertical por lo que la recta es x=0
  lines(-1:0,c(1,1), col="red")#altura triángulo 1
  lines(0:1,c(2,2), col="red")#altura triángulo 2
  base=sqrt((A[1,1]+A[2,1])^2+(A[1,2]+A[2,2])^2) #base de ambos triángulos
  altura_1 = 1 #Verde
  altura_2 = 1 #Naranja
  
  area_1=(base*altura_1)/2
  area_2=(base*altura_2)/2
  
  area=area_1+area_2
  
  cat("El área del Triángulo 1 (Verde) es: ", area_1,"\n")
  cat("El área del Triángulo 2 (Naranje) es: ", area_2,"\n")
  cat("El área de paralelogramo es: ", area,"\n")
  
}
#Probamos la función
rm(list=ls())#Limpia Global Environment
dev.off()#Limpia los plots
v1=c(1,2)
v2=c(-1,1)
funcion_procedimiento_triangulos(v1,v2)
