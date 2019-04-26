library(deSolve)
yini=c(p=10)#vector de condiciones iniciales
par(mfrow=c(2,4))
A=c(100,50)
for(i in 1:2){
  r=c(1.1,0.5)
  for(j in 1:2){
    H=c(2,8)
    for(k in 1:2){
      modelo=function(t,y,parms){
        with(as.list(y),{
          dp=r[j]*p*(1-p/A[i])-H[k]
          list(c(dp))})}
      tiempo=seq(0,5,0.001)
      solucion=ode(y=yini,times=tiempo,func=modelo,parms=NULL)
      plot(tiempo,solucion[,'p'],type='l',col='red',grid(),ylab='Peces',ylim =range(min(solucion[,'p']),max(solucion[,'p'])))
      legend(3,max(solucion[,'p']),legend =c(A[i],r[j],H[k]),cex=0.8)
    }
  }  
}


library(deSolve)
yini=c(p=10)#vector de condiciones iniciales
par(mfrow=c(2,4))
A=c(100,50)
for(i in 1:2){
  r=c(2,1.2)
  for(j in 1:2){
    H=c(1,2)
    for(k in 1:2){
      modelo=function(t,y,parms){
        with(as.list(y),{
          dp=r[j]*p*(1-p/A[i])-H[k]*p
          list(c(dp))})}
      tiempo=seq(0,5,0.001)
      solucion=ode(y=yini,times=tiempo,func=modelo,parms=NULL)
      plot(tiempo,solucion[,'p'],type='l',col='red',grid(),ylab='Peces',ylim =range(min(solucion[,'p']),max(solucion[,'p'])))
      legend(3,max(solucion[,'p']),legend =c(A[i],r[j],H[k]),cex=0.8)
    }
  }  
}



library(deSolve)
yini=c(S=100,I=1)#vector de condiciones iniciales
par(mfrow=c(2,3))
B=c(1,0.5,0.2,0.05,0.01,0)
for(i in 1:6){
  modelo=function(t,y,parms){
    with(as.list(y),{
      dS=-B[i]*S*I
      dI=B[i]*S*I
      list(c(dS,dI))})}
    tiempo=seq(0,5,0.001)
    solucion=ode(y=yini,times=tiempo,func=modelo,parms=NULL)
    plot(tiempo,solucion[,'S'],type='l',col='red',grid(),ylab='Población',main=B[i],lwd='2',ylim=range(min(solucion[,'S'],solucion[,'I']),max(solucion[,'S'],solucion[,'I'])))
    lines(tiempo,solucion[,'I'],type = 'l',col='blue',lwd='2',grid())
    legend(3,100,legend =c("Suceptibles","infecciosos"),col=c("red","blue"),lty=1:1,cex=0.6)
}


