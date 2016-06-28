#x <- c(2, 4, 5, 7, 9, 11, 14, 16, 17, 18)

y <- c(2, 4, 14, 11, 19, 15, 14, 10, 20, 16) 

N <- length(y)
n <- 6

####################################################################

m.y <- mean(y);m.y #media da populacao
v.m.y <- (1-n/N)/n*var(y);v.m.y #variancia da media amostral

####################################################################
#Gerando todas as amostras possiveis
####################################################################

M <- choose(N,n) #quantas amostras de tamanho n podemos ter (todas as possíveis)

#gera, em cada uma das M colunas,  indices de todas as amostras possiveis de tamanho n, 
#como se o conjunto fosse [1..N]
sp.index <- combn(N,n)

#gera, em cada uma das M colunas, as amostra propriamente dita com os elementos
sp <- matrix(y[sp.index],ncol=M,byrow=F) 

mu <- apply(sp,2,mean) #obtem as medias de cada amostra/coluna

tab0 <- table(mu)/length(mu)

#variancia da media amostral. deve ser exatamente o mesmo valor de v.m.y
var(mu)*(M-1)/M
v.m.y

#calcula a variancia de cada uma das M amostras
va <- apply(sp, 2, var)

#o menor desvio de var(y)
no1 <- which(abs(va-var(y)) == min(abs(va-var(y))) )[1];no1

#o maior desvio de var(y)
no2 <- which(abs(mu-mean(y)) == max(abs(mu-mean(y))) )[1];no2

####################################################################
#Gerando B amostras, aleatoriamente
####################################################################

B <- 1000
v <- rep(NA,B) #Repete NA B vezes

set.seed(123)

#sample takes a sample of the specified size from the elements of y without replacement
for(i in 1:B) v[i] <- mean(sample(y,n))

tab1 <- table(v)/B #WTF?
#table(sample(paste("Center", LETTERS[1:5]), 100, replace = TRUE))

#Variancia da media amostral. deveria ser um valor proximo a v.m.y
var(v)*(B-1)/B
v.m.y

#####################################################################

#Distribution of sample mean. 
#The black dot represents the sample mean of the original y-vector.

#exact
plot(tab0,type="h",xlab="Sample mean", ylab="Rel. frequency",axes=F, col="darkgrey")
points(m.y,0,pch=19)
axis(1);axis(2)
# approximated 
plot(tab1,type="h",xlab="Sample mean",ylab="Rel. frequency",axes=F,col="darkgrey") 
points(m.y,0,pch=19)
axis(1);axis(2)

#####################################################################

#no1 é a amostra, dentre todas as possíveis, a que possui o menor desvio de var(y)
ys = sp[,no1];ys # [1]  2 14 11 19 10 16

mean(ys) #The mean 12

(1-n/N)/n*var(ys) #The variance [1] 2.32

m.y # The true mean [1] 12.5

v.m.y #The true variance [1] 2.314815

#####################################################################
#The Bootstrap
#The idea is that the sample relates to the population in a 
#similar way as a sub-sample of the sample relates to the sample
#####################################################################

#usando ys, a amostra que, dentre todas as possíveis, possui o menor desvio de var(y)
B <- 1000
e <- rep(NA,B)
set.seed(7)
for(i in 1:B) e[i] <- mean(sample(ys,n,replace=T))
#media das B amostras bootstrap
mean(e) # [1] 11.91283

#media da amostra (original) usada para gerar as bootstrap
mean(ys) # [1] 12

#bootstrap estimator of variance of sample mean
#variancia da media das amostras bootstrap
(1-n/N)*var(e) # [1] 1.958619

#analytic estimator of variance of sample mean
#variancia da media da amostra (original) usada para gerar as bootstrap
(1-n/N)/n*var(ys) # [1] 2.32

#variancia da media da populacao
v.m.y # [1] 2.314815

#####################################################################

#no2 é a amostra, dentre todas as possíveis, a que possui o maior desvio de var(y)
ys = sp[,no2];ys # [1] 14 19 15 14 20 16

mean(ys) #[1] 16.33333 (30.67% maior do que a real)
m.y # The true mean [1] 12.5

(1-n/N)/n*var(ys) #The variance [1] 0.4444444 (80.8% menor do que a real)
v.m.y #The true variance [1] 2.314815 

B <- 1000
e <- rep(NA,B)
set.seed(7)
for(i in 1:B) e[i] <- mean(sample(ys,n,replace=T))

#bootstrap estimator of variance of sample mean
#variancia da media das amostras bootstrap
n/(n-1)*(1-n/N)*var(e) # [1] 2.350343

#analytic estimator of variance of sample mean
#variancia da media da amostra (original) usada para gerar as bootstrap
(1-n/N)/n*var(ys) # [1] 2.32

#variancia da media da populacao
v.m.y # [1] 2.314815
