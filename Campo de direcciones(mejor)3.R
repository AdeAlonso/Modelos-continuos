y=seq(-10,10,0.5)
x=seq(-10,10,0.5)
pendiente=rep(0,length(x))
A=-2
e=0.20
#plot(x,2*x,xlim =c(-10,10), ylim=c(-10,10),type='l',col='red')
#plot(x,exp(A*x),xlim =c(-10,10), ylim=c(-10,10),type='l',col='red')
#plot(x,(sqrt(A)*exp(2*sqrt(A)*x)-sqrt(A))/(exp(2*sqrt(A)*x)+1),xlim =c(-10,10), ylim=c(-10,10),type='l',col='red')
plot(x,pendiente,xlim =c(-10,10), ylim=c(-10,10),type='l',col='red',lwd=2,xlab = 't',ylab = 'x')
lines(pendiente,y,xlim =c(-10,10), ylim=c(-10,10),type='l',col='red',lwd=2)
for(j in 1:length(y)){
  for(i in 1:length(x)){
    pendiente[j]=A-(y[j]^2)
    arrows(x[i],y[j],((pendiente[j]*x[i])-y[j]+y[j]+e)/(pendiente[j]),y[j]+e,length = 0.05,angle = 5,col = 'blue')
    #points(x[i],y[j])
  }
}
