#The Libraries required
#install.packages("datarium")
#install.packages("caTools")
#install.packages("ggplot2")            # Packages need to be installed only once
#install.packages("GGally")

#Global Variable
splitRatio = 0.75

#Loading The Data
data("marketing", package = "datarium")
data_size = dim(marketing)

#Understanding the data
head(marketing)
summary(marketing)
#Pairwise plotting technique 1
plot(marketing, col="purple", main="Plotting Pairs Against Each Other")
#Pairwise plotting technique 2
ggpairs(marketing)

#Splitting The Data
set.seed(101)# Set Seed so that same sample can be reproduced in future also

#Now Selecting 75% of data as sample from total 'n' rows of the data
sample = sample.split(marketing$youtube, SplitRatio = splitRatio)
train = subset(marketing, sample == TRUE)
test = subset(marketing, sample == FALSE)
train_size = dim(train)
test_size = dim(test)

#creating the model
Model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(Model)

#Predicting
pred <- predict(Model, test)
numx <- data_size[1]*(1 - splitRatio)
x_axis <- seq(numx)
df <- data.frame(x_axis, pred,test$sales)

#Plotting the predicted values against the actual values
g <- ggplot(df, aes(x=x_axis))
g <- g + geom_line(aes(y=pred, colour="Predicted"))
g <- g + geom_point(aes(x=x_axis, y=pred, colour="Predicted"))
g <- g + geom_line(aes(y=test$sales, colour="Actual"))
g <- g + geom_point(aes(x=x_axis, y=test$sales, colour="Actual"))
g <- g + scale_colour_manual("", values = c(Predicted="red", Actual="blue"))
g


#Evaluation
original = test$sales
predicted = pred
d = original-predicted
mse = mean((d)^2)
mae = mean(abs(d))
rmse = sqrt(mse)
R2 = 1-(sum((d)^2)/sum((original-mean(original))^2))

cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", R2)