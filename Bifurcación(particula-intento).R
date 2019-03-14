r=seq(0,4,0.001)
A=10
fil=length(r)
M=matrix(nrow=fil, ncol=21)
M[,1]=.2
for(k in 1:fil){
  for(j in 2:21){
    #M[k,j]=r[k]*M[k,(j-1)]*(1-(M[k,(j-1)]))
    M[k,j]=(sqrt(r[k])*A*exp(2*sqrt(r[k])*(j))-sqrt(r[k]))/(A*exp(2*sqrt(r[k])*(j))+1)
  }    
}
plot(0,0, xlim=range(0,0.1), ylim=range(0,1), xlam='r', ylam='Pn')
for(f in 2:fil){
  for(c in 2:21){
    points(r[f],M[f,c], pch='.')}
}  