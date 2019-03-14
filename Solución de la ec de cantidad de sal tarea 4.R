library(deSolve)
s=1
yini=c(s=1)#vector de condiciones iniciales

sal=function(t,y,parms){
  with(as.list(y),{
    ds=((9*t)-90+(16*s))/(4*t-40)
    list(c(ds))})}
tiempo=seq(0,10,0.001)
solucion=ode(y=yini,times=tiempo,func=sal,parms=NULL)
plot(solucion)
#plot(tiempo,solucion[,'v'],type='l')
lines(tiempo,solucion[,'s'],col='blue')