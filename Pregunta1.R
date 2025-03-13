#Solucion Pregunta 1

x<- c(0,1)
fx<- c(0.68, 0.32)

#tabla de prob 
cbind(x,fx) #poner los vectores en columnas
plot(x,fx, pch=16, col="red",ylim=c(0,1)) #pch pone la bola negra completa
lines(x,fx, type="h", col="orange")

mu<-sum(x*fx); mu #media (probabilidad de que alguien cuando entre por la puerta diga que tiene 2 o +)

sigmasq <- sum((x-mu)^2*fx); sigmasq 



n<-43
sum(sample(x,n, prob=fx, replace=TRUE))

Y<-function(i)sum(sample(x,n, prob=fx, replace=TRUE)) #sample() se utiliza para realizar muestreo aleatorio. Si es FALSE, no se repiten elementos.
Y
#Y=NUM DE si EN UNA ENCUESTA

#bucle
m<- 40000
muestra<- sapply(1:m,Y)
fi<- table(muestra)/m
fi

#frecuencias relativas  

data.frame(fi, Fi=cumsum(fi))

barplot(fi)

##

dbinom(13,43,0.32)
###

#tabla de probabilidad
data.frame(Y=0:43, Prob=dbinom(0:43,43,0.32))

y<-0:43
fy<- dbinom(y,43,0.32)

plot(y,fy,pch=19, col="blue") #binomial
lines(y,fy, col="purple", type="h")
dbinom(13,43,0.32)#respuesta pregunta 1

#tabla de prob
y<- 0:44
Pi<-dbinom(y,44,0.32)
df<-data.frame(Y=y,Prob=Pi);df

#Frecuencia acumulada
Fi<-cumsum(df$Prob)
pbinom(16,44,0.32)#respuesta pregunta 2
plot(y,Fi,type="s", col="blue")

###

x<- 0:24
Pi<- dbinom(x,24,0.68)
mu<-sum(x*Pi);mu
sum((x-mu)^2*Pi)
Fi<-cumsum(Pi);Fi
plot(x,Fi,type="s", col="orange")
qbinom(0.25,24,0.68)
