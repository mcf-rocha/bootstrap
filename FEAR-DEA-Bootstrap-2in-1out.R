require(FEAR)

# The data
x <- t(matrix(c(2,2,6,3,6, 5,4,6,2,2), ncol=2)) 
y <- t(matrix(c(1,2,3,1,2)))
N <- dim(x)[2]
x1 = x[1,]/y
x2 = x[2,]/y

# The frontier for the technologies 
dea.plot.isoquant(t(x1),t(x2),txt=1:N)
# The observations have dotted lines from origo 
for ( i in 1:length(y) ) {
  lines(c(0,x1[i]), c(0,x2[i]),lty="dotted") 
}

# bootstrap
b <-boot.sw98(rbind(x1,x2),matrix(rep(1,N),nrow=1),NREP=2000,RTS=3) 

#Bias-corrected frontier
dea.plot.isoquant(t(x1)/b$dhat.bc,t(x2)/b$dhat.bc,lty="dashed",add=T)
#Upper 95% confidence frontier
dea.plot.isoquant(t(x1)/b$conf.int[,2],t(x2)/b$conf.int[,2],lty="dotted",add=T)

#RTS: indicator for returns to scale
#1 for variable returns to scale, 
#2 for non-increasing returns to scale, 
#3 for constant returns to scale, or 
#4 for non-decreasing returns to scale.

#ORIENTATION: indicates direction in which efficiency is to be evaluated 
#1 for input orientation, 
#2 for output orientation, or 
#3 for hyperbolic graph orientation.


#Marca, usando uma bola, a eficiência de cada firma
plot(1/b$dhat, ylim = c(.3,1),main = "Resultado da análise",xlab = "DMUs",ylab = "Eficiência")
#Marca, usando uma losango, a "bias corrected" eficiência de cada firma
points(1/b$dhat.bc,pch=5)
#Traça o intervalo de confiança
for (i in 1:5 ) lines(rep(i,2),1/b$conf.int[i,],type = "o", pch=3)

