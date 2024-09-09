# visualizando o df
library(RCurl)
dhfr <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/dhfr.csv") )
View(dhfr)

# vendo se tem valores nulos no df
sum(is.na(dhfr))

# inserindo valores nulos no df
na.gen <- function(data,n) {
  i <- 1
  while (i < n+1) {
    idx1 <- sample(1:nrow(data), 1)
    idx2 <- sample(1:ncol(data), 1)
    data[idx1,idx2] <- NA
    i = i+1
  }
  return(data)
}

dhfr <- dhfr[,-1]
dhfr <- na.gen(dhfr,100)

# checando novamente se tem valores nulos
sum(is.na(dhfr))
colSums(is.na(dhfr))
str(dhfr)

# criando um df com as linhas que possuem NA
missingdata <- dhfr[!complete.cases(dhfr), ]
sum(is.na(missingdata))

# deletando todas as linhas com NA
clean.data <- na.omit(dhfr)
sum(is.na(clean.data))

# imputation
dhfr.impute <- dhfr

# função para identificar onde tem NA e imputar com a média da coluna
for (i in which(sapply(dhfr.impute, is.numeric))) { 
  dhfr.impute[is.na(dhfr.impute[, i]), i] <- mean(dhfr.impute[, i],  na.rm = TRUE) 
}

# ou mediana
# for (i in which(sapply(dhfr.impute, is.numeric))) { 
#   dhfr.impute[is.na(dhfr.impute[, i]), i] <- median(dhfr.impute[, i],  na.rm = TRUE) 
# }

sum(is.na(dhfr.impute))




