s1=1/3
s2=1/8
s3=3/4
s4=1/4
s5=1/2
g1=3/4
g2=1/2
g3=3/5

n=seq(0,19)
a1=rep(0,20)
a2=rep(0,20)
a3=rep(0,20)
a4=rep(0,20)
a5=rep(0,20)

a1[1]=10
a2[1]=5
a3[1]=7
a4[1]=3
a5[1]=5

for(j in 1:19){
	a1[j+1]=0
	a2[j+1]=s1*a1[j]+(s2*(1-g2)*a2[j])
	a3[j+1]=a2[j]*s2*g2+(a3[j]*s3*(1-g3))+s5*a5[j]
	a4[j+1]=s3*g3*a3[j]
	a5[j+1]=s4*a4[j]}

plot(n, a1, type='b', lwd=2)
lines(n, a2, type='b', col='purple', lwd=2)
lines(n, a3, type='b', col='orange', lwd=2)
lines(n, a4, type='b', col='red', lwd=2)
lines(n, a5, type='b', col='pink', lwd=2)
