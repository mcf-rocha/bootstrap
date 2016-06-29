library(Benchmarking)
# data
x <- c(2, 4, 5, 7, 9, 11, 14, 16, 17, 18)
y <- c(2, 4, 14, 11, 19, 15, 14, 10, 20, 16) 
t(data.frame(x,y, row.names=LETTERS[1:10]))

N <- length(y)
n <- 6

M <- choose(N,n) #quantas amostras de tamanho n podemos ter (todas as possíveis)

#  A B  C  D  E  F  G  H  I  J
#x 2 4  5  7  9 11 14 16 17 18
#y 2 4 14 11 19 15 14 10 20 16

d <- round(dea(x,y,RTS=1,ORIENTATION="in")$eff,3) 
names(d) <- LETTERS[1:10];d
#    A     B     C     D     E     F     G     H     I     J 
#1.000 0.625 1.000 0.607 1.000 0.527 0.357 0.250 1.000 0.367 

#create a sample of size n
set.seed(7)
#gerando os indices
s <- sort(sample(1:10,n));s

#pegando os valores
xs <- x[s];xs
ys <- y[s];ys

ds <- round(dea(xs,ys,RTS=1,ORIENTATION="in")$eff,3) 
names(ds) <- LETTERS[s];ds
#    A     B     D     H     I     J 
#1.000 0.778 1.000 0.403 1.000 0.698

rbind(population=d[s],sample=ds)
#           A     B     D     H I     J
#population 1 0.625 0.607 0.250 1 0.367
#saple      1 0.778 1.000 0.403 1 0.698

dea.plot(x,y,txt=LETTERS[1:10],xlab="x",ylab="y", cex=1.2,pch=19,col="darkgrey",ylim=c(0,25))
points(x[s],y[s],pch=19,col=1,cex=0.5) 
lines(x[s][ds==1],y[s][ds==1],lty=2) 
legend(0.5,25,c("population (solid line)","sample (dashed line)"), pch=c(19),col=c("darkgrey","black"),bty="n")

############################

#gerando os indices
sp.index <- combn(N,n)

#only the fraction n/N = 6/10 = 0.6 of all M = 210 samples contain firm D.
withD <- apply(sp.index,2,function(z) 4%in%z)
sum(withD) # [1] 126
mean(withD) # [1] 0.6

# take all samples (126) which include firm D
sp.index4 <- sp.index[,withD]
# input/output data for these samples
sp4.y <- matrix(y[sp.index4],ncol = M*n/N,byrow=F) 
sp4.x <- matrix(x[sp.index4],ncol = M*n/N,byrow=F) 

# DEA for the first sample which includes firm D 
dea(matrix(sp4.x[,1]),matrix(sp4.y[,1])) # [1] 1.0000 0.6250 1.0000 0.6071 1.0000 0.5273

# DEA for each sample which includes firm D
M4 <- (M*n/N)
e4 <- rep(NA,M4) 
for (i in 1:M4){
  #The object e4 holds all 126 efficiency scores for firm D from each sample.
  e4[i] <- dea(matrix(sp4.x[,i]), 
               matrix(sp4.y[,i]))$eff[sp.index4[,i]==4]
}

plot(table(round(e4,2))/M4,lwd=1, xlab="Efficiency scores for firm D", ylab="Probability")

############################################

Mk <- M*n/N # number of samples including element k 
ie <- (1:N)[d!=1] # inefficient firms (d tem as eficiencias de toda a populacao)
nie <- length(ie) # number of inefficient firms
ee <- matrix(NA,Mk,nie)
for (j in 1:nie){
  k <- ie[j]
  withk <- apply(sp.index,2,function(z) k%in%z) 
  sp.indexk <- sp.index[,withk]
  spk.y <- matrix(y[sp.indexk],ncol = Mk,byrow=F) 
  spk.x <- matrix(x[sp.indexk],ncol = Mk,byrow=F) 
  for (i in 1:Mk) ee[i,j] <- dea(matrix(spk.x[,i]),
                                 matrix(spk.y[,i]))$eff[sp.indexk[,i]==k]
}

for (i in 1:nie){ 
  plot(table(round(ee[,i],2))/Mk,lwd=1,
      xlab=paste("Efficiency scores, firm", LETTERS[ie][i]), 
      ylab="Probability")
}

eet <- data.frame(round(rbind( 
        apply(ee,2,min),
        apply(ee,2,max), 
        apply(ee,2,mean),
        d[ie], 
        apply(ee,2,mean)-d[ie]),3))
names(eet) <- LETTERS[ie]
rownames(eet) <- c("min","max","mean","pop. efficiency","bias") 
eet
