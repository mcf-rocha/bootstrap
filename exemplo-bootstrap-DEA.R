library(Benchmarking)
# data
x <- c(2, 4, 5, 7, 9, 11, 14, 16, 17, 18)
y <- c(2, 4, 14, 11, 19, 15, 14, 10, 20, 16) 
t(data.frame(x,y, row.names=LETTERS[1:10]))

#  A B  C  D  E  F  G  H  I  J
#x 2 4  5  7  9 11 14 16 17 18
#y 2 4 14 11 19 15 14 10 20 16

d <- round(dea(x,y,RTS=1,ORIENTATION="in")$eff,3) 
names(d) <- LETTERS[1:10];d
#    A     B     C     D     E     F     G     H     I     J 
#1.000 0.625 1.000 0.607 1.000 0.527 0.357 0.250 1.000 0.367 

#create a sample
set.seed(7)
#gerando os indices
s <- sort(sample(1:10,n));s

#pegando os valores
xs <- x[s]
ys <- y[s]

ds <- round(dea(xs,ys,RTS=1,ORIENTATION="in")$eff,3) 
names(ds) <- LETTERS[s];ds
#    A     B     D     H     I     J 
#1.000 0.778 1.000 0.403 1.000 0.698

rbind(population=d[s],saple=ds)
#           A     B     D     H I     J
#population 1 0.625 0.607 0.250 1 0.367
#saple      1 0.778 1.000 0.403 1 0.698

dea.plot(x,y,txt=LETTERS[1:10],xlab="x",ylab="y", cex=1.2,pch=19,col="darkgrey",ylim=c(0,25))
points(x[s],y[s],pch=19,col=1,cex=0.5) 
lines(x[s][ds==1],y[s][ds==1],lty=2) 
legend(0.5,25,c("population (solid line)","sample (dashed line)"), pch=c(19),col=c("darkgrey","black"),bty="n")

withD <- apply(sp.index,2,function(z) 4%in%z)
sum(withD)

