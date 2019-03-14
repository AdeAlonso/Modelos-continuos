n=seq(1,21)
x=rep(0,20)
r=c(1,1.5,2,2.5,3,3.5)
A=100
x[1]=5
par(mfrow=c(3,2))
for(k in 1:6){
for(j in 2:length(n)){
  x[j]=r[k]*x[j-1]*(1-(x[j-1]/A))
}
plot(n,x,col= 'blue')
}