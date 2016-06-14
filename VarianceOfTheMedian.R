library(boot)
amostra <- c(94,197,16,38,99,141,23)
funcao <- function(d,i) {median(d[i])}
quantidadeReplicates <- 200
bootstrap <- boot(amostra,funcao,quantidadeReplicates)
#O resultado da "funcao" aplicado a cada uma das "quantidadeReplicates" Bootstrap Replicas
bootstrap$t
#O histograma das "quantidadeReplicates" Bootstrap Replicas
hist(bootstrap$t,main=NULL)
#O Erro padr??o da "funcao"
sqrt(var(boo$t))

