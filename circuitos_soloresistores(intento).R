#u=rep(0,20)
w=c(2000,20000,40000,80000,100000,120000)
par(mfrow=c(2,3))
for(i in 1:6){
t=seq(0,5,0.01)
#w=2000
  u=127*sin(w[i]*t)
  v3=u/13
plot(u, type='l',col='red')
lines(v3,type = 'l',col='blue')
}