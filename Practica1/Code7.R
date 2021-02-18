#################################################################
#Title: Área Lemniscata de Bernoulli por Triangulación  
#Authors: Iván Martín Gómez and Markos Aguirre Elorza
#Date: Saturday 17th February 2021
#Descriptions: 
#################################################################
funcion_Area_Lemniscata_TD <- function(numeroPuntos){

#Definimos la curva y obtenemos su área utilizando la fórmulas de teoría
  d=8;
  areaTeoria=2*d^2;
  #Parte Izquierda
  x1=seq(-d*sqrt(2),0,by=0.00001);
  #Parte Derecha
  x2=seq(0,d*sqrt(2),by=0.00001);
  #Parte Superior Izquierda  
  y_sup_izq=d*sqrt(sqrt(1+4*(x1/d)^2)-(1+(x1/d)^2));
  #Parte Superior Derecha 
  y_sup_der=d*sqrt(sqrt(1+4*(x2/d)^2)-(1+(x2/d)^2));
  #Parte Inferior Izquierda  
  y_inf_izq=-d*sqrt(sqrt(1+4*(x1/d)^2)-(1+(x1/d)^2));
  #Parte Inferior Derecha 
  y_inf_der=-d*sqrt(sqrt(1+4*(x2/d)^2)-(1+(x2/d)^2));
  
  if(numeroPuntos%%2!=0){
    numeroPuntos=numeroPuntos+1;
  }
  x_puntosDiscretos_izq=seq(-d*sqrt(2),0,by=(0-(-d*sqrt(2)))/(numeroPuntos/4));
  x_puntosDiscretos_der=seq(0,d*sqrt(2),by=(d*sqrt(2)-(0))/(numeroPuntos/4));
  y_puntosDiscretos_sup_izq=d*sqrt(sqrt(1+4*(x_puntosDiscretos_izq/d)^2)-(1+(x_puntosDiscretos_izq/d)^2));
  y_puntosDiscretos_sup_der=d*sqrt(sqrt(1+4*(x_puntosDiscretos_der/d)^2)-(1+(x_puntosDiscretos_der/d)^2));
  y_puntosDiscretos_inf_izq=-d*sqrt(sqrt(1+4*(x_puntosDiscretos_izq/d)^2)-(1+(x_puntosDiscretos_izq/d)^2));
  y_puntosDiscretos_inf_der=-d*sqrt(sqrt(1+4*(x_puntosDiscretos_der/d)^2)-(1+(x_puntosDiscretos_der/d)^2));

#Dibujamos Lemniscata de Bernoulli
  #Parte Superior Izquierda
  plot(x1,y_sup_izq,xlim=c(-d*sqrt(2),d*sqrt(2)),ylim=c(-d*0.5,d*0.5), type='l', xlab="x", ylab="y", col="blue",lwd=3);
  #Parte Superior Derecha 
  lines(x2,y_sup_der, col="blue",lwd=3)
  #Parte Inferior Izquierda  
  lines(x1,y_inf_izq, col="blue",lwd=3)
  #Parte Inferior Derecha 
  lines(x2,y_inf_der, col="blue",lwd=3)
  
  #Parte Superior Izquierda
  points(x_puntosDiscretos_izq,y_puntosDiscretos_sup_izq, col = "red", pch=19)
  #Parte Superior Derecha 
  points(x_puntosDiscretos_der,y_puntosDiscretos_sup_der, col = "red", pch=19)
  #Parte Inferior Izquierda  
  points(x_puntosDiscretos_izq,y_puntosDiscretos_inf_izq, col = "red", pch=19)
  #Parte Inferior Derecha 
  points(x_puntosDiscretos_der,y_puntosDiscretos_inf_der, col = "red", pch=19)
  
  
  #Triangulación Parte Superior Izquierda
  i=1
  for(i in 1:length(x_puntosDiscretos_izq)){
    
  }
    #Triangulo 1
    lines(c(x_puntosDiscretos_izq[1],x_puntosDiscretos_izq[2]),c(y_puntosDiscretos_sup_izq[1],y_puntosDiscretos_sup_izq[2]), col = "green", pch=9);
    lines(c(x_puntosDiscretos_izq[2],x_puntosDiscretos_izq[3]),c(y_puntosDiscretos_sup_izq[2],y_puntosDiscretos_sup_izq[3]), col = "green", pch=9);
    lines(c(x_puntosDiscretos_izq[1],x_puntosDiscretos_izq[3]),c(y_puntosDiscretos_sup_izq[1],y_puntosDiscretos_sup_izq[3]), col = "green", pch=9);
    m_base_1=(y_puntosDiscretos_sup_izq[3]-y_puntosDiscretos_sup_izq[1])/(x_puntosDiscretos_izq[3]-x_puntosDiscretos_izq[1]);
    b_base_1=y_puntosDiscretos_sup_izq[3]-m_base_1*x_puntosDiscretos_izq[3];
    m_perpendicular_1=-1/m_base_1;
    b_perpendicular_1=-m_perpendicular_1*x_puntosDiscretos_izq[2]+y_puntosDiscretos_sup_izq[2]
    x_interseccion_1=(b_perpendicular_1-b_base_1)/(m_base_1-m_perpendicular_1)
    y_intersección_1=m_perpendicular_1*x_interseccion_1+b_perpendicular_1;
    points(x_interseccion_1,y_intersección_1, col = "yellow", pch=19)
    base_Tr1=sqrt((x_puntosDiscretos_izq[1]-x_puntosDiscretos_izq[3])^2+(y_puntosDiscretos_sup_izq[1]-y_puntosDiscretos_sup_izq[3])^2);
    altura_Tr1=sqrt((x_interseccion_1-x_puntosDiscretos_izq[2])^2+(y_intersección_1-y_puntosDiscretos_sup_izq[2])^2)
    area_Tr1=base_Tr1*altura_Tr1/2
    #Triangulo 2
    lines(c(x_puntosDiscretos_izq[3],x_puntosDiscretos_izq[4]),c(y_puntosDiscretos_sup_izq[3],y_puntosDiscretos_sup_izq[4]), col = "green", pch=9);
    lines(c(x_puntosDiscretos_izq[1],x_puntosDiscretos_izq[4]),c(y_puntosDiscretos_sup_izq[1],y_puntosDiscretos_sup_izq[4]), col = "green", pch=9);
    m_base_2=(y_puntosDiscretos_sup_izq[4]-y_puntosDiscretos_sup_izq[1])/(x_puntosDiscretos_izq[4]-x_puntosDiscretos_izq[1]);
    b_base_2=y_puntosDiscretos_sup_izq[4]-m_base_2*x_puntosDiscretos_izq[4];
    m_perpendicular_2=-1/m_base_2;
    b_perpendicular_2=-m_perpendicular_2*x_puntosDiscretos_izq[3]+y_puntosDiscretos_sup_izq[3]
    x_interseccion_2=(b_perpendicular_2-b_base_2)/(m_base_2-m_perpendicular_2)
    y_intersección_2=m_perpendicular_2*x_interseccion_2+b_perpendicular_2;
    points(x_interseccion_2,y_intersección_2, col = "yellow", pch=19)
    base_Tr2=sqrt((x_puntosDiscretos_izq[1]-x_puntosDiscretos_izq[4])^2+(y_puntosDiscretos_sup_izq[1]-y_puntosDiscretos_sup_izq[4])^2);
    altura_Tr2=sqrt((x_interseccion_2-x_puntosDiscretos_izq[3])^2+(y_intersección_2-y_puntosDiscretos_sup_izq[3])^2)
    area_Tr2=base_Tr2*altura_Tr2/2
    #Triangulo 3
    lines(c(x_puntosDiscretos_izq[4],x_puntosDiscretos_izq[5]),c(y_puntosDiscretos_sup_izq[4],y_puntosDiscretos_sup_izq[5]), col = "green", pch=9);
    lines(c(x_puntosDiscretos_izq[1],x_puntosDiscretos_izq[5]),c(y_puntosDiscretos_sup_izq[1],y_puntosDiscretos_sup_izq[5]), col = "green", pch=9);
    m_base_3=(y_puntosDiscretos_sup_izq[5]-y_puntosDiscretos_sup_izq[1])/(x_puntosDiscretos_izq[5]-x_puntosDiscretos_izq[1]);
    b_base_3=y_puntosDiscretos_sup_izq[5]-m_base_3*x_puntosDiscretos_izq[5];
    m_perpendicular_3=-1/m_base_3;
    b_perpendicular_3=-m_perpendicular_3*x_puntosDiscretos_izq[4]+y_puntosDiscretos_sup_izq[4]
    x_interseccion_3=(b_perpendicular_3-b_base_3)/(m_base_3-m_perpendicular_3)
    y_intersección_3=m_perpendicular_3*x_interseccion_3+b_perpendicular_3;
    points(x_interseccion_3,y_intersección_3, col = "yellow", pch=19)
    base_Tr3=sqrt((x_puntosDiscretos_izq[1]-x_puntosDiscretos_izq[5])^2+(y_puntosDiscretos_sup_izq[1]-y_puntosDiscretos_sup_izq[5])^2);
    altura_Tr3=sqrt((x_interseccion_3-x_puntosDiscretos_izq[4])^2+(y_intersección_3-y_puntosDiscretos_sup_izq[4])^2)
    area_Tr3=base_Tr3*altura_Tr3/2
    #Triangulo 4
    lines(c(x_puntosDiscretos_izq[5],x_puntosDiscretos_izq[6]),c(y_puntosDiscretos_sup_izq[5],y_puntosDiscretos_sup_izq[6]), col = "green", pch=9);
    lines(c(x_puntosDiscretos_izq[1],x_puntosDiscretos_izq[6]),c(y_puntosDiscretos_sup_izq[1],y_puntosDiscretos_sup_izq[6]), col = "green", pch=9);
    m_base_4=(y_puntosDiscretos_sup_izq[6]-y_puntosDiscretos_sup_izq[1])/(x_puntosDiscretos_izq[6]-x_puntosDiscretos_izq[1]);
    b_base_4=y_puntosDiscretos_sup_izq[6]-m_base_2*x_puntosDiscretos_izq[6];
    x_interseccion_4=x_puntosDiscretos_izq[5]
    y_intersección_4=m_base_4*x_interseccion_4+b_base_4;
    points(x_interseccion_4,y_intersección_4, col = "yellow", pch=19)
    base_Tr4=sqrt((x_puntosDiscretos_izq[1]-x_puntosDiscretos_izq[6])^2+(y_puntosDiscretos_sup_izq[1]-y_puntosDiscretos_sup_izq[6])^2);
    altura_Tr4=sqrt((x_interseccion_4-x_puntosDiscretos_izq[5])^2+(y_intersección_4-y_puntosDiscretos_sup_izq[5])^2)
    area_Tr4=base_Tr4*altura_Tr4/2
    
    area_Triangulacion=area_Tr1+area_Tr2+area_Tr3+area_Tr4;
    error=100*abs(area_Triangulacion-areaTeoria/4)/(areaTeoria/4);
    cat("Parte Superior Izquierda = ","%\n");
    cat("--->Área Tr1 = ", area_Tr1,"\n");
    cat("--->Área Tr2 = ", area_Tr2,"\n");
    cat("--->Área Tr3 = ", area_Tr3,"\n");
    cat("--->Área Tr4 = ", area_Tr4,"\n");
    cat("--->Área triangulacion = ", area_Triangulacion,"\n");
    cat("--->Área teoría = ", areaTeoria/4,"\n");
    cat("--->Error cometido = ", error,"%\n");
    
    #Triangulación Parte Superior derecha
    #Triangulo 1
    lines(c(x_puntosDiscretos_der[1],x_puntosDiscretos_der[2]),c(y_puntosDiscretos_sup_der[1],y_puntosDiscretos_sup_der[2]), col = "green", pch=9);
    lines(c(x_puntosDiscretos_der[2],x_puntosDiscretos_der[3]),c(y_puntosDiscretos_sup_der[2],y_puntosDiscretos_sup_der[3]), col = "green", pch=9);
    lines(c(x_puntosDiscretos_der[1],x_puntosDiscretos_der[3]),c(y_puntosDiscretos_sup_der[1],y_puntosDiscretos_sup_der[3]), col = "green", pch=9);
    m_base_1=(y_puntosDiscretos_sup_der[3]-y_puntosDiscretos_sup_der[1])/(x_puntosDiscretos_der[3]-x_puntosDiscretos_der[1]);
    b_base_1=y_puntosDiscretos_sup_der[3]-m_base_1*x_puntosDiscretos_der[3];
    m_perpendicular_1=-1/m_base_1;
    b_perpendicular_1=-m_perpendicular_1*x_puntosDiscretos_der[2]+y_puntosDiscretos_sup_der[2]
    x_interseccion_1=(b_perpendicular_1-b_base_1)/(m_base_1-m_perpendicular_1)
    y_intersección_1=m_perpendicular_1*x_interseccion_1+b_perpendicular_1;
    points(x_interseccion_1,y_intersección_1, col = "yellow", pch=19)
    base_Tr1=sqrt((x_puntosDiscretos_der[1]-x_puntosDiscretos_der[3])^2+(y_puntosDiscretos_sup_der[1]-y_puntosDiscretos_sup_der[3])^2);
    altura_Tr1=sqrt((x_interseccion_1-x_puntosDiscretos_der[2])^2+(y_intersección_1-y_puntosDiscretos_sup_der[2])^2)
    area_Tr1=base_Tr1*altura_Tr1/2
    #Triangulo 2
    lines(c(x_puntosDiscretos_der[3],x_puntosDiscretos_der[4]),c(y_puntosDiscretos_sup_der[3],y_puntosDiscretos_sup_der[4]), col = "green", pch=9);
    lines(c(x_puntosDiscretos_der[1],x_puntosDiscretos_der[4]),c(y_puntosDiscretos_sup_der[1],y_puntosDiscretos_sup_der[4]), col = "green", pch=9);
    m_base_2=(y_puntosDiscretos_sup_der[4]-y_puntosDiscretos_sup_der[1])/(x_puntosDiscretos_der[4]-x_puntosDiscretos_der[1]);
    b_base_2=y_puntosDiscretos_sup_der[4]-m_base_2*x_puntosDiscretos_der[4];
    m_perpendicular_2=-1/m_base_2;
    b_perpendicular_2=-m_perpendicular_2*x_puntosDiscretos_der[3]+y_puntosDiscretos_sup_der[3]
    x_interseccion_2=(b_perpendicular_2-b_base_2)/(m_base_2-m_perpendicular_2)
    y_intersección_2=m_perpendicular_2*x_interseccion_2+b_perpendicular_2;
    points(x_interseccion_2,y_intersección_2, col = "yellow", pch=19)
    base_Tr2=sqrt((x_puntosDiscretos_der[1]-x_puntosDiscretos_izq[4])^2+(y_puntosDiscretos_sup_der[1]-y_puntosDiscretos_sup_izq[4])^2);
    altura_Tr2=sqrt((x_interseccion_2-x_puntosDiscretos_der[3])^2+(y_intersección_2-y_puntosDiscretos_sup_der[3])^2)
    area_Tr2=base_Tr2*altura_Tr2/2
    #Triangulo 3
    lines(c(x_puntosDiscretos_der[4],x_puntosDiscretos_der[5]),c(y_puntosDiscretos_sup_der[4],y_puntosDiscretos_sup_der[5]), col = "green", pch=9);
    lines(c(x_puntosDiscretos_der[1],x_puntosDiscretos_der[5]),c(y_puntosDiscretos_sup_der[1],y_puntosDiscretos_sup_der[5]), col = "green", pch=9);
    m_base_3=(y_puntosDiscretos_sup_der[5]-y_puntosDiscretos_sup_der[1])/(x_puntosDiscretos_der[5]-x_puntosDiscretos_der[1]);
    b_base_3=y_puntosDiscretos_sup_der[5]-m_base_3*x_puntosDiscretos_der[5];
    m_perpendicular_3=-1/m_base_3;
    b_perpendicular_3=-m_perpendicular_3*x_puntosDiscretos_der[4]+y_puntosDiscretos_sup_der[4]
    x_interseccion_3=(b_perpendicular_3-b_base_3)/(m_base_3-m_perpendicular_3)
    y_intersección_3=m_perpendicular_3*x_interseccion_3+b_perpendicular_3;
    points(x_interseccion_3,y_intersección_3, col = "yellow", pch=19)
    base_Tr3=sqrt((x_puntosDiscretos_der[1]-x_puntosDiscretos_der[5])^2+(y_puntosDiscretos_sup_der[1]-y_puntosDiscretos_sup_der[5])^2);
    altura_Tr3=sqrt((x_interseccion_3-x_puntosDiscretos_der[4])^2+(y_intersección_3-y_puntosDiscretos_sup_der[4])^2)
    area_Tr3=base_Tr3*altura_Tr3/2
    #Triangulo 4
    lines(c(x_puntosDiscretos_der[5],x_puntosDiscretos_der[6]),c(y_puntosDiscretos_sup_der[5],y_puntosDiscretos_sup_der[6]), col = "green", pch=9);
    lines(c(x_puntosDiscretos_der[1],x_puntosDiscretos_der[6]),c(y_puntosDiscretos_sup_der[1],y_puntosDiscretos_sup_der[6]), col = "green", pch=9);
    m_base_4=(y_puntosDiscretos_sup_der[6]-y_puntosDiscretos_sup_der[1])/(x_puntosDiscretos_der[6]-x_puntosDiscretos_der[1]);
    b_base_4=y_puntosDiscretos_sup_der[6]-m_base_2*x_puntosDiscretos_der[6];
    x_interseccion_4=x_puntosDiscretos_der[5]
    y_intersección_4=0;
    points(x_interseccion_4,y_intersección_4, col = "yellow", pch=19)
    base_Tr4=sqrt((x_puntosDiscretos_der[1]-x_puntosDiscretos_der[6])^2+(y_puntosDiscretos_sup_der[1]-y_puntosDiscretos_sup_der[6])^2);
    altura_Tr4=sqrt((x_interseccion_4-x_puntosDiscretos_der[5])^2+(y_intersección_4-y_puntosDiscretos_sup_der[5])^2)
    area_Tr4=base_Tr4*altura_Tr4/2
    
    area_Triangulacion=area_Tr1+area_Tr2+area_Tr3+area_Tr4;
    error=100*abs(area_Triangulacion-areaTeoria/4)/(areaTeoria/4);
    cat("Parte Superior Derecha = ","\n");
    cat("--->Área Tr1 = ", area_Tr1,"\n");
    cat("--->Área Tr2 = ", area_Tr2,"\n");
    cat("--->Área Tr3 = ", area_Tr3,"\n");
    cat("--->Área Tr4 = ", area_Tr4,"\n");
    cat("--->Área triangulacion = ", area_Triangulacion,"\n");
    cat("--->Área teoría = ", areaTeoria/4,"\n");
    cat("--->Error cometido = ", error,"%\n");
}

#Probamos la función
rm(list=ls())#Limpia Global Environment
dev.off()#Limpia los plots
funcion_Area_Lemniscata_TD(20)#Llamamos a la función
