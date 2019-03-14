

x=seq(-5,5,0.001)
#y=dunif(x)
y=dunif(x,-1,1)
plot(x,y,col='green',type='l')

#Distribución exponencial
x=seq(0,5,0.001)
y1=dexp(x,1)
y2=dexp(x,2)
y3=dexp(x,0.5)
z1=pexp(x,1)
z2=pexp(x,2)
z3=pexp(x,0.5)
plot(x,z1,type='l')
lines(x,z2,col='blue')
lines(x,z3,col='red')
#Distribución gamma
x=seq(0,5,0.001)
y1=dgamma(x,1,1)
lines(x,y1,col='red')
y2=dgamma(x,1,3)
lines(x,y2,col='blue')
y3=dgamma(x,2,3)
lines(x,y3,col='green')
y4=dgamma(x,3,2)
lines(x,y4,col='pink')

# Distribución normal
x=seq(-5,5,0.001)
y=dnorm(x)
plot(x,y,type='l',col='red')
y1=dnorm(x,1,1)
lines(x,y1,col='blue')
y2=dnorm(x,0,4)
lines(x,y2,col='orange')
y3=dnorm(x,0,0.25)
lines(x,y3,col='purple')