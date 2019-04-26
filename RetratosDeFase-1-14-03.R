y=seq(-10,10,0.5)
x=seq(-10,10,0.5)
pendiente=rep(0,length(x))
e=0.25
plot(x,pendiente,xlim =c(-10,10), ylim=c(-10,10),type='l',col='red',lwd=2,xlab = 't',ylab = 'x')
lines(pendiente,y,xlim =c(-10,10), ylim=c(-10,10),type='l',col='red',lwd=2)
for(j in 1:length(y)){
  for(i in 1:length(x)){
    if(-2*x[i]-5*y[j]!=0){
    #pendiente[j]=(2*x[i]+6*y[j])/(-2*x[i]-5*y[j])
    #pendiente[j]=(-3*x[i]-4*y[j])/(x[i]+y[j])  
    pendiente[j]=(-3*x[i]-5*y[j])/(5*x[i]+3*y[j])
    arrows(x[i],y[j],((pendiente[j]*x[i])-y[j]+y[j]+e)/(pendiente[j]),y[j]+e,length = 0.05,angle = 5,col = 'blue')
    }else{
      arrows(x[i],y[j],x[i]+e,y[j],length = 0.05,angle = 5,col = 'blue')
    }
  }
}
