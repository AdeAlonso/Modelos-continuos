#Para construir el retrat de fase necesitas graficar x vs y y construir las rectas del sistema
#homogéneo para hallar los equilibrios
x=seq(-2,2,0.01)
library(deSolve)
yini=c(x=1,y=2)#vector de condiciones iniciales
movimiento=function(t,y,parms){
  with(as.list(y),{
    #dx=2*x+6*y
    #dx=-3*x-4*y
    dx=-3*x-5*y
    
    #dy=-2*x-5*y
    #dy=x+y
    dy=5*x+3*y
    list(c(dx,dy))})}
tiempo=seq(-10,10,0.001)
solucion=ode(y=yini,times=tiempo,func=movimiento,parms=NULL)
#plot(solucion)
#plot(solucion[,'x'],solucion[,'y'],type = 'l',col='orange',lwd='4')
lines(solucion[,'x'],solucion[,'y'],type = 'l',col='orange',lwd='4')
#plot(tiempo,solucion[,'x'],type='l',col='green',lwd='3')
#plot(tiempo,solucion[,'y'],type = 'l',col='brown',lwd='3')
#lines(x,(-3)*x,col='purple',lty=2,lwd=3)
#lines(x,((-2/5)*x),col='purple',lty=2,lwd=3)
#lines(x,(-3/4)*x,col='purple',lty=2,lwd=3)
#lines(x,(-1*x),col='purple',lty=2,lwd=3)
lines(x,(-3/5)*x,col='purple',lty=2,lwd=3)
lines(x,(-5/3)*x,col='purple',lty=2,lwd=3)