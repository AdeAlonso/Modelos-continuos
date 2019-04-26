library(deSolve)
yini=c(v=0.5)#vector de condiciones iniciales
movimiento=function(t,y,parms){
  with(as.list(y),{
    dv=v*(1-v)*(2-v)
    list(c(dv))})}
tiempo=seq(0,5,0.001)
solucion=ode(y=yini,times=tiempo,func=movimiento,parms=NULL)
#plot(solucion)
plot(tiempo,solucion[,'v'],type='l',col='green',lwd='2')

library(deSolve)
yini=c(v=-0.5)#vector de condiciones iniciales
movimiento=function(t,y,parms){
  with(as.list(y),{
    dv=(v^2)*(1+v)
    list(c(dv))})}
tiempo=seq(0,30,0.01)
solucion=ode(y=yini,times=tiempo,func=movimiento,parms=NULL)
#plot(solucion)
plot(tiempo,solucion[,'v'],type='l',col='green',lwd='2')

library(deSolve)
yini=c(v=1)#vector de condiciones iniciales
movimiento=function(t,y,parms){
  with(as.list(y),{
    dv=sin(v)
    list(c(dv))})}
tiempo=seq(0,pi,0.001)
solucion=ode(y=yini,times=tiempo,func=movimiento,parms=NULL)
#plot(solucion)
plot(tiempo,solucion[,'v'],type='l',col='green',lwd='2')