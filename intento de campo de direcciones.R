f=rep(0,10)
t=seq(-10,10)
a=1
c=0
l=10plot
M=matrix(nrow=l,ncol=10)
for(i in 1:10){
  for(j in 1:10){
    # M[i,j]=a*j+c
    arrows(i-(1/5),j,i+(1/5),j+i-1,length = 0.1)
  }
}