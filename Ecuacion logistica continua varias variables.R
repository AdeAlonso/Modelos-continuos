library(deSolve)
r=c(1,1.5,2,2.5,3,3.5)
A=100
p=5 #poblacion inicial
yini=c(p=5)#vector de condiciones inicieles
par(mfrow=c(3,2))
for(k in 1:6){
poblacion=function(t,y,parms){ #funcion que tiene estos parametros
  with(as.list(y),{
    dp=r[k]*p*(1-(p/A))          #Ecuacion diferencial a resolver
    list(c(dp))})}            #Ecuaciones diferenciales
tiempo=seq(0,10,0.001)        #Tiempo de simulacion
solucion=ode(y=yini,times=tiempo,func=poblacion,parms=NULL) #Resuelve la ecuacion 
#con condicios ininiales yini, por cierto tiempo, dela fucion poblacion, cn otros 
#otros parametros no dados
plot(solucion) 
#Imprimir solucion
#plot(tiempo,solucion[,'v'],type='l')
lines(tiempo,solucion[,'p']) #
}