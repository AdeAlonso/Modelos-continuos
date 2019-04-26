y=seq(-10,10,0.5)
x=seq(-10,10,0.5)
e=0.20
pendiente=rep(0,length(x))
ye=rep(0,length(x))
par(mfrow=c(2,2))
for(k in 1:2){
a=c(2,-1)
for(l in 1:2){
b=c(-3,2)
plot(x,ye,xlim =c(-10,10), ylim=c(-10,10),type='l',col='red',lwd=2,xlab = 'y',ylab = 'x')
legend(8.5,8,legend =c(a[k],b[l]),cex=0.8)
lines(ye,y,xlim =c(-10,10), ylim=c(-10,10),type='l',col='red',lwd=2)
for(j in 1:length(y)){
  for(i in 1:length(x)){
    pendiente[j]=a[k]*x[i]/b[l]*y[j]
    arrows(x[i],y[j],((pendiente[j]*x[i])-y[j]+y[j]+e)/(pendiente[j]),y[j]+e,length = 0.05,angle = 5,col = 'blue')
    #points(x[i],y[j])
  }
}
}
}



y=seq(-10,10,0.5)
x=seq(-10,10,0.5)
pendiente=rep(0,length(x))
a=2
e=0.20
b=2
c=-3
k=-2
plot(x,pendiente,xlim =c(-10,10), ylim=c(-10,10),type='l',col='red',lwd=2,xlab = 't',ylab = 'x')
lines(pendiente,y,xlim =c(-10,10), ylim=c(-10,10),type='l',col='red',lwd=2)
for(j in 1:length(y)){
  for(i in 1:length(x)){
    pendiente[j]=(a*x[i]+b*y[j])/(c*x[i]+k*y[j])
    arrows(x[i],y[j],((pendiente[j]*x[i])-y[j]+y[j]+e)/(pendiente[j]),y[j]+e,length = 0.05,angle = 5,col = 'blue')
    #points(x[i],y[j])
  }
}




y=seq(-10,10,0.5)
x=seq(-10,10,0.5)
pendiente=rep(0,length(x))
a=1
e=0.20
b=2
c=-1
k=1
plot(x,pendiente,xlim =c(-10,10), ylim=c(-10,10),type='l',col='red',lwd=2,xlab = 't',ylab = 'x')
lines(pendiente,y,xlim =c(-10,10), ylim=c(-10,10),type='l',col='red',lwd=2)
for(j in 1:length(y)){
  for(i in 1:length(x)){
    pendiente[j]=(a*x[i]+b*y[j])/(c*x[i]+k*y[j])
    arrows(x[i],y[j],((pendiente[j]*x[i])-y[j]+y[j]+e)/(pendiente[j]),y[j]+e,length = 0.05,angle = 5,col = 'blue')
    #points(x[i],y[j])
  }
}