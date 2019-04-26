y=seq(0,0.8,0.01)
y1=rep(0,length(y))
y1=y*(1-y)
plot(y,y,type='l',lwd='2',col='blue',xlab='xn',ylab='xn+1')
lines(y,y1,type='l',col='green',lwd='2')
yn=0.5
for(m in 1:20){
  pn=yn*(1-yn)
  arrows(yn,yn,yn,pn,length = 0.1,angle = 30,col = 'red')
  arrows(yn,pn,pn,pn,length = 0.1,angle = 30,col='red')
  yn=pn
}

a=c(-0.3,0.8,1.8)
par(mfrow=c(1,3))
for(i in 1:3){
y=seq(-4,3,0.01)
y1=rep(0,length(y))
y1=y*(1-y)*(2-y)
plot(y,y,type='l',lwd='2',col='blue',xlab='yn',ylab='yn+1')
lines(y,y1,type='l',col='green',lwd='2')
yn=a[i]
for(m in 1:20){
  pn=yn*(1-yn)*(2-yn)
  arrows(yn,yn,yn,pn,length = 0.1,angle = 30,col = 'red')
  arrows(yn,pn,pn,pn,length = 0.1,angle = 30,col='red')
  yn=pn
}
}

b=c(-1.2,-0.3,0.8)
par(mfrow=c(1,3))
for(i in 1:3){
y=seq(-2,2,0.01)
y1=rep(0,length(y))
y1=(y^2)*(1+y)
plot(y,y,type='l',lwd='2',col='blue',xlab='yn',ylab='yn+1')
lines(y,y1,type='l',col='green',lwd='2')
yn=b[i]
for(m in 1:20){
  pn=(yn^2)*(1+yn)
  arrows(yn,yn,yn,pn,length = 0.1,angle = 30,col = 'red')
  arrows(yn,pn,pn,pn,length = 0.1,angle = 30,col='red')
  yn=pn
}
}


d=c(1,2)
par(mfrow=c(1,2))
for(i in 1:2){
y=seq(0,pi,0.01)
y1=rep(0,length(y))
y1=sin(y)
plot(y,y,type='l',lwd='2',col='blue',xlab='yn',ylab='yn+1')
lines(y,y1,type='l',col='green',lwd='2')
yn=d[i]
for(m in 1:20){
  pn=sin(yn)
  arrows(yn,yn,yn,pn,length = 0.1,angle = 30,col = 'red')
  arrows(yn,pn,pn,pn,length = 0.1,angle = 30,col='red')
  yn=pn
}
}