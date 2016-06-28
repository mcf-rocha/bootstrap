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
s <- sort(sample(1:10,n));s