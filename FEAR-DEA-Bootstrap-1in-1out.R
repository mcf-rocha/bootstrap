library(FEAR)
# Data
y <- cbind(1,2,3,4,5)
x <- cbind(2,4,3,5,6)

# DEA, Shephard input distance function,
d <- FEAR::dea(x,y, RTS=3, ORIENTATION=1) 

dea.plot(t(x),t(y),RTS="vrs",ORIENTATION="in-out",txt=1:length(x), GRID=TRUE)
dea.plot(t(x),t(y),RTS="crs",ORIENTATION="in-out",add=TRUE,lty="dashed", GRID=TRUE)

# Efficiencies
print(1/d,digits=3) 
print(mean(1/d),digits=3)


# Bootstrap
b <- boot.sw98(x,y, RTS=3, ORIENTATION=1, NREP=2000) 
#NREP é o que? O tamanho da amostra ou a quantidade de amostras que foram geradas?
#Se for um, qual é o outro?



print(b,digits=3)
print(sqrt(b$var),digits=3)


#DEA Frontier
dea.plot.frontier(t(x),t(y),RTS="crs",txt=1:dim(t(x))[1], GRID=TRUE)
#Bias-corrected frontier
dea.plot.frontier(t(x)/b$dhat.bc,t(y),RTS="crs",lty="dashed",add=T, GRID=TRUE) 
#Upper 95% confidence frontier
dea.plot.frontier(t(x)/b$conf.int[,2],t(y),RTS="crs",lty="dotted",add=T, GRID=TRUE)


#Marca, usando uma bola, a eficiência de cada firma
plot(1/b$dhat, ylim = c(.45,1),main = "Resultado da análise",xlab = "Firms",ylab = "Efficiency")
#Marca, usando uma losango, a "bias corrected" eficiência de cada firma
points(1/b$dhat.bc,pch=5)
#Traça o intervalo de confiança
for (i in 1:5 ) lines(rep(i,2),1/b$conf.int[i,],type = "o", pch=3)