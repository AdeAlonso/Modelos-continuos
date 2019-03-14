
library(deSolve)
w=c(1,10,100,200,400,1000)
par(mfrow=c(2,3))
A=10
R=1000
C=(0.1)*(10^(-6))
yini=c(i=0)#vector de condiciones iniciales
#i=A/R #posición inicial
for(k in 1:6){
i=function(t,y,parms){
  with(as.list(y),{
    di=(1/R)*(-A*sin(w[k]*t)*w[k])-(i/R*C)
    list(c(di))})}
tiempo=seq(0,10,0.01)
solucion=ode(y=yini,times=tiempo,func=i,parms=NULL)
#plot(tiempo,solucion[,'i'],type='l')
plot(tiempo,R*solucion[,'i'],type='l')
lines(tiempo,A*sin(w[k]*tiempo)-R*solucion[,'i'],col='blue')
lines(tiempo,A*sin(w[k]*tiempo),col='red')
}