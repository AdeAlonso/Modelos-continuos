library(deSolve)
par(mfrow=c(1,3))
A=c(1,0,-1)
for(k in 1:3){
  yini=c(x=0)
  x=function(t,y,parms){
    with(as.list(y),{
      dx=A[k]-x*x
      list(c(dx))})}
  tiempo=seq(0,1,0.01)
  solucion=ode(y=yini,times=tiempo,func=x,parms=NULL)
  plot(tiempo,solucion[,'x'],type='l')
  # plot(tiempo,R*solucion[,'i'],type='l')
  #lines(tiempo,A*sin(w[k]*tiempo)-R*solucion[,'i'],col='blue')
  #lines(tiempo,A*sin(w[k]*tiempo),col='red')
}