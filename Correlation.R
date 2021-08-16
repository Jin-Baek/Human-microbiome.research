setRepositories(ind = 1:8)

library(corrplot)

data <- read.csv("Your data path../Data.txt",sep="\t")

corr <- cor(data)
corrplot(corr,method="square",type="upper")
