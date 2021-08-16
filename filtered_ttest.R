## Feature selection with T.test & evaluate with KNN algorithm
## https://www.youtube.com/watch?v=j5oPzAJvnVI

library(dplyr)
library(caret)
library(e1071)
library(stats)
library(tictoc)
library(limma)

setRepositories(ind = 1:8)

data <- read.csv("Your Path.../",sep="\t")
origin <- data

origin <- origin[,2:length(origin)] 

#View(origin)
#View(data)
dim(data)

# Store index of feature 
index <- c()

# Select feature that has constant value in whole class with "index" vector

### constant value in whole class == No variance ###
for(i in 3:length(data)){
  min <- summary(data[,i])[[1]]
  max <- summary(data[,i])[[6]]

  if((min!=max)){
    index <- append(index,i)
  }
}

# Apply "index" vector to original data
data <- data[,c(2,index)]

data<-data %>%
  mutate(Disease=as.factor(data$Disease))


## choice 1
############### Perform with t.test (Heteroscedasticity of variance) ################# 
tic()
pvli <- c()
for(i in 2:length(data)){
  result <- t.test(data[,i]~Disease,data=data)
  pvli <- append(pvli,result$p.value)
}
toc()

pvli

############### Perform with t.test (Homogeneity of variance) ################### 
tic()
pvli <- c()
for(i in 2:length(data)){
  result <- t.test(data[,i]~Disease,data=data,var.equal=TRUE)
  pvli <- append(pvli,result$p.value)
}
toc()

pvli

################ Perform with simple linear model #############################
tic()
pvli <- c()
for(i in 2:length(data)){
  model <- lm(data[,i]~Disease,data=data)
  pvli <- append(pvli,summary(model)$coefficient[2,4])
}
toc()


# Add value temporary to adjust number of index to data 
pvli <- append(pvli,0,0)
pvli

# Get index that P-value is lower than 0.05
ind <- which(pvli<0.05)

data <- data[,ind]

# Shuffle data
randomIdx <- sample(1:nrow(data))
data <- data[randomIdx,]
origin <- origin[randomIdx,]

# KNN algorithm

## feature selected data
ctrl <- trainControl(method = "repeatedcv",repeats = 3,number=10)
customGrid <- expand.grid(k=c(2,3,5,7,9))
KNNfs <- train(Disease~.,data=data,method="knn",trControl=ctrl,tuneGrid=customGrid)
KNNfs


## original data
KNNori <- train(Disease~.,data=origin,method="knn",trControl=ctrl,tuneGrid=customGrid)
KNNori

# Compare the result between feature selected data & Original data by KNN