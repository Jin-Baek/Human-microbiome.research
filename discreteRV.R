## Discrete probability random variable & distribution 

library(dplyr)
library(caret)
library(e1071)
library(ggplot2)

setRepositories(ind = 1:8)

# Visualization function
visualize <- function(events,pmf,cdf){
  ggplot(df, aes(x = factor(events), y = pmf)) + geom_col() +
    geom_text(
      aes(label = round(pmf,2), y = pmf),
      position = position_dodge(0.9),
      size = 3,
      vjust = 0
    ) +
    labs(title = "PMF and CDF Distribution",
         x = "Events (x)",
         y = "Probability") +
    geom_line(data = df, aes(x = events, y = cdf))
}


# events = 사건발생 횟수
# size = 시행 횟수
# prob = 사건발생 확률 or 성공확률


# Poisson distribution 
# avg.count = 평균 발생 횟수
events = 1:20
avg.count <- 10
pmf <- dpois(events,lambda=avg.count,log=FALSE)
cdf <- ppois(events,lambda=avg.count,lower.tail = T)
df <- data.frame(events,pmf,cdf)

visualize(df$events,df$pmf,df$cdf)

# Geometric distribution 
size = 1:20
prob <- 0.2
pmf <- dgeom(size,prob = prob,log = F)
cdf <- pgeom(size,prob = prob,lower.tail = T)
df<-data.frame(size,pmf,cdf)

visualize(df$size,df$pmf,df$cdf)

# Negative binomial distribution (Using binomial function)
# success = 성공 횟수 
size = 11:30
success <- 5
pmf <- dbinom(success,size-success,prob = 0.3,log = F)
cdf <- pbinom(success,size-success,prob = 0.3,lower.tail = T)
df<-data.frame(size,pmf,cdf)

visualize(df$size,df$pmf,df$cdf)

# Binomial distribution
events <- 1:20 
size <- 30
pmf <- dbinom(events,size,prob=0.3,log = F)
cdf <- pbinom(events,size,prob=0.3, lower.tail = T)
df<-data.frame(events,pmf,cdf)

visualize(df$events,df$pmf,df$cdf)

# Hypergeometric distribution
population.size <- 100 
population.wanted <- 50 
sample.size <- 30 
events <- 5:15
pmf <- dhyper(events,sample.size,population.size,population.wanted,log = F)
cdf <- phyper(events,sample.size,population.size,population.wanted,lower.tail = T)
df<-data.frame(events,pmf,cdf)

visualize(df$events,df$pmf,df$cdf)
