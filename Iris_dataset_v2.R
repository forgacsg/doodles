library(ggplot2)

data<-iris

#data landscape analysis - check na's etc.

head(data,10)

summary(data)

str(data)

numeric.inputs<- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")

categorical.inputs<-c("Species")

# create a matrix of scatter plots

data.num<-data[numeric.inputs]

data.cat<-data[categorical.inputs]

plot(data.num) 

ggplot(data=data)+geom_point(aes(x=Sepal.Length,y=Sepal.Width,colour=Species))
ggplot(data=data)+geom_point(aes(x=Petal.Length,y=Petal.Width,colour=Species))

#create a correlation plot

library(corrplot)
library(RColorBrewer)

correlation<-cor(data.num)
round(correlation,2)

corrplot.mixed(correlation, lower="number",upper="circle",lower.col="darkgrey",
               upper.col=brewer.pal(n=9, name="Spectral"),tl.cex=0.7,cl.ratio=0.4)


# Normalizing the data - create a function and then use lapply() to apply it to the data
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

data_norm <- as.data.frame(lapply(data.num, normalize))

# Check the data
summary(data_norm)

# Create a KNN with the caret library

library(caret)

# split data into training and test set with indexation
  
index <- createDataPartition(data$Species, p=0.75, list=FALSE)

data.training <- data[index,]

data.test <- data[-index,]

# Train a model
model_knn <- train(data.training[, 1:4], data.training[, 5], method='knn')

# Predict the labels of the test set
predictions<-predict.train(object=model_knn, data.test[,1:4], type="raw")

# Evaluate the predictions
table(predictions)

# Confusion matrix 
confusionMatrix(predictions,data.test[,5])

# visualize the results of the experiment

head(data.test)

ggplot(data=data.test)+geom_point(aes(x=Sepal.Length,y=Sepal.Width,colour=Species))
ggplot(data=data.test)+geom_point(aes(x=Petal.Length,y=Petal.Width,colour=Species))

#Checking the normalized data