            #The Sparks Foundation
    #To Explore Supervised Machine Learning

#Reading the Data:

study <- read.csv("http://bit.ly/w-data")
View(study)
summary(study)

#Graphical Analysis of Data:

hist(study$Hours, xlab = "Hours Studied", col = "blue", main = "Histogram of Hours Studied")

hist(study$Scores, xlab = "Score", col = "green", main = "Histogram of Scores")

plot(study$Hours, study$Scores, xlab = "Hours Studied", ylab = "Score", pch = 18, col = "maroon", main = "Score as per Hours Studied")
abline(lm(study$Scores ~ study$Hours))

#Statistical Analysis of Data:

cor(study$Hours, study$Scores)

var(study$Hours, study$Scores)

#Linear Regression Model:

mod = lm(Scores ~ Hours, data = study) 
print(mod)
summary(mod)

#Predicting the score on full data:
new.hour <- data.frame(Hours = c(9.25))
prediction.study <- cbind(new.hour, Score = predict(mod, newdata = new.hour))
prediction.study

#Building a Predictive Model

set.seed(19)
training.index <- sample(1:nrow(study), 0.8*nrow(study))
training.data <- study[training.index, ]
test.data <- study[- training.index, ]

#Building model on training data and using it to predict scores on test data:

mod2 <- lm(Scores ~ Hours, data = training.data)
Score.predict <- predict(mod2, test.data)
summary(mod2)

#Calculating prediction accuracy and error rates:

actual.predict <- data.frame(cbind(actuals = test.data$Scores, predicteds = Score.predict))
correlation.accuracy <- cor(actual.predict)
head(actual.predict)

DMwR::regr.eval(actual.predict$actuals, actual.predict$predicteds)

#Visualizing the Training and Test Sets:

library(ggplot2)
ggplot(training.data, aes(Hours, Scores)) + geom_point() + geom_smooth(method = "lm") + 
  ggtitle("Training Data") +
  xlab("Hours Studied") +
  ylab("Score")

ggplot(test.data, aes(Hours, Scores)) + geom_point() + geom_smooth(method = "lm") +
  ggtitle("Test Data") +
  xlab("Hours Studied") +
  ylab("Score")

#Prediction of Score:

new.hour2 <- data.frame(Hours=c(9.25))
Prediction <- cbind(new.hour2, Score= predict(mod2,newdata = new.hour2))
Prediction
