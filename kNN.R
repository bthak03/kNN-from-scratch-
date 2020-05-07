# kNN from scratch 

# Loading libraries and dataset 

library(tidyverse)
library(psych)

data <- read.csv("franchisesales.csv", stringsAsFactors = F)

data1<-data[,-1]

# Creating a a function to Normalize
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

data_norm<- as.data.frame(scale(data1[1:5]))

##Creating a function called neighbours to calculate the nearest neighbours 
neighbors_1 <- function (df1,v, k)
{
  m <- nrow(df1) 
  ds <- numeric(m) 
  q <- as.numeric(v) 
  for(i in 1:m)
  {
    p <- as.numeric(df1[i,])
    ds[i] <- sqrt(sum((p - q)^2))
    
    
  }
  return(neighbors_1 <- order(ds)[1:k]) 
}

# Creating a kk_pred function to predict for a vector or one row of data frame using neighbours function 

knn_pred <- function(df1,values, v, k){
  closest_nb <- neighbors_1(df1, v, k)
  avg <- mean(values[closest_nb])
  return(avg)
}

# Creating a vector to predict 

d_1 <- data.frame('StoreSize' = 4.2,'InvValue' = 601,'AdvBudget'= 7.8,'DistrictSize' = 14.2,'NumComp' = 6) 

new_df <- d_1

for(i in 1:ncol(data1))
{
  new_df[,i] <- (d_1[,i] - mean(data1[,i]) )/sd(data1[,i]) 
}
new_df

# Implementing knn_predict 
knn_pred(data_norm,data$NetSales, new_df,k=3)
##The predicted value we get is 420

for(i in 1:nrow(data))
{
  data$forecast1[i] <- knn_pred(data_norm, data$NetSales, data_norm[i,], 3)
}
#For-loop is used to calculate the error using "NetSales - forecast".
for(i in 1:nrow(data))
{
  data$error1[i] <- data$NetSales[i] - data$forecast1[i]
}
MSE <- mean((data$error1)^2)
MSE
##MSE using this model is 463.2695 

# Performing knn_pred for an entire dataset if had a larger data set,  would split it into training and validation data to avoid overfitting

data$pred <- 0
data$error2 <- 0
for(k in 1:7)
{
  for(i in 1:nrow(data))
  {
    data$pred[i] <- knn_pred(data_norm, data$NetSales, data_norm[i,], k)
    data$error2[i] <- data$NetSales[i] - data$pred[i]
  }
  MSE[k] <- mean((data$error2)^2)
}
MSE
k <- c(1:7)

#Plotting for optimal k-value 

ggplot(data = NULL, mapping = aes(x=k, y=MSE)) + geom_point() + xlab("k-value") + ylab("MSE")+ geom_line() + scale_x_continuous(breaks = 1:7) + scale_y_continuous(limits = c(0,1000), breaks = scales::pretty_breaks(n = 25) )


