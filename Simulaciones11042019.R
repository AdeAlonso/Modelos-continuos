library(deSolve)
yini=c(S=10000,I=2)#vector de condiciones iniciales
par(mfrow=c(2,2))
B=c(0.01,0.005,0.001,0.0005)
for(i in 1:4){
  modelo=function(t,y,parms){
    with(as.list(y),{
      dS=-B[i]*S*I
      dI=B[i]*S*I
      list(c(dS,dI))})}
  tiempo=seq(0,5,0.001)
  solucion=ode(y=yini,times=tiempo,func=modelo,parms=NULL)
  plot(tiempo,solucion[,'S'],type='l',col='red',ylab='Población',main=B[i],lwd='2',ylim=range(0,10000))
  lines(tiempo,solucion[,'I'],type = 'l',col='blue',lwd='2',grid())
  legend(3,max(solucion[,'S']),legend =c("Suceptibles","infecciosos"),col=c("red","blue"),lty=1:1,cex=0.8)
}


library(deSolve)
yini=c(S=10000,I=2)
par(mfrow=c(2,2))
b=c(0.001,0.0005)
for(i in 1:2){
  g=c(0.005,0.0001)
  for(j in 1:2){
    
    
      modelo=function(t,y,parms){
        with(as.list(y),{
          dS=-b[i]*S*I+g[j]*I
          dI=b[i]*S*I-g[j]*I
          list(c(dS,dI))})}
      tiempo=seq(0,5,0.001)
      solucion=ode(y=yini,times=tiempo,func=modelo,parms=NULL)
      plot(tiempo,solucion[,'S'],type='l',col='red',grid(),ylab='Población',ylim =range(min(solucion[,'S']),max(solucion[,'S'])))
      lines(tiempo,solucion[,'I'],type='l',col='blue',grid())
      legend(3,max(solucion[,'S']),legend =c(b[i],g[j]),cex=0.8)
    
  }  
}





library(deSolve)
yini=c(S=10000,I=2,R=0)
par(mfrow=c(2,2))
b=c(0.001,0.0005)
for(i in 1:2){
  g=c(0.005,0.5)
  for(j in 1:2){
    
    
    modelo=function(t,y,parms){
      with(as.list(y),{
        dS=-b[i]*S*I
        dI=b[i]*S*I-g[j]*I
        dR=g[j]*I
        list(c(dS,dI,dR))})}
    tiempo=seq(0,5,0.001)
    solucion=ode(y=yini,times=tiempo,func=modelo,parms=NULL)
    plot(tiempo,solucion[,'S'],type='l',col='red',grid(),ylab='Población',ylim =range(0,10000))
    lines(tiempo,solucion[,'I'],type='l',col='blue',grid())
    lines(tiempo,solucion[,'R'],type = 'l',col='green',grid())
    legend(4,max(solucion[,'S']),legend =c(b[i],g[j]),cex=0.8)
    
  }  
}



library(deSolve)
yini=c(S=10000,I=2,R=0)
par(mfrow=c(2,2))
b=c(0.001,0.0005)
for(i in 1:2){
  g=c(0.005,0.5)
  for(j in 1:2){
    
    
    modelo=function(t,y,parms){
      with(as.list(y),{
        dS=-b[i]*S*I
        dI=b[i]*S*I-g[j]*I
        dR=g[j]*I
        list(c(dS,dI,dR))})}
    tiempo=seq(0,5,0.001)
    solucion=ode(y=yini,times=tiempo,func=modelo,parms=NULL)
    plot(tiempo,solucion[,'S'],type='l',col='red',grid(),ylab='Población',ylim =range(0,10000))
    lines(tiempo,solucion[,'I'],type='l',col='blue',grid())
    lines(tiempo,solucion[,'R'],type = 'l',col='green',grid())
    legend(4,max(solucion[,'S']),legend =c(b[i],g[j]),cex=0.8)
    
  }  
}



library(deSolve)
yini=c(S=10000,E=200,I=2,R=0)
par(mfrow=c(2,4))
a=c(0.1,1)
for(i in 1:2){
  b=c(1,0.5)
  for(j in 1:2){
    g=c(0.05,0.8)
    for(k in 1:2){
    modelo=function(t,y,parms){
      with(as.list(y),{
        dS=-a[i]*S
        dE=a[i]*S-b[j]*E
        dI=b[j]*E-g[k]*I
        dR=g[k]*I
        list(c(dS,dE,dI,dR))})}
    tiempo=seq(0,20,0.001)
    solucion=ode(y=yini,times=tiempo,func=modelo,parms=NULL)
    plot(tiempo,solucion[,'S'],type='l',col='red',grid(),ylab='Población',ylim =range(0,10000))
    lines(tiempo,solucion[,'E'],type='l',col='blue',grid())
    lines(tiempo,solucion[,'I'],type = 'l',col='green',grid())
    lines(tiempo,solucion[,'R'],type = 'l',col='purple',grid())
    legend(4,max(solucion[,'S'])+300,legend =c(a[i],b[j],g[k]),cex=0.8)
    legend(4.5,max(solucion[,'S'])-1000,legend =c("Suceptibles","Expuestos","Infecciosos","Recuperados"),col=c("red","blue","green","purple"),lty=1:1:1:1,cex=0.8)
           
    }
    
  }  
}





