              #The Sparks Foundation
      #To Explore Supervised Machine Learning

#Importing the Data:

study <- read.csv("D:/data.csv")
View(study)

#Graphical Analysis of Data:

hist(study$Hours, xlab = "Hours Studied", col = "blue", main = "Histogram of Hours Studied")

hist(study$Scores, xlab = "Score", col = "green", main = "Histogram of Scores")

plot(study$Hours, study$Scores, xlab = "Hours Studied", ylab = "Score", pch = 18, col = "maroon", main = "Score as per Hours Studied")
abline(lm(study$Scores ~ study$Hours))

library(ggplot2)
ggplot(study, aes(Hours, Scores)) + geom_point() + geom_smooth(method = "lm")

#Statistical Analysis of Data:

cor(study$Hours, study$Scores)

var(study$Hours, study$Scores)

#Linear Regression Model:

mod <- lm(study$Scores ~ study$Hours)
summary(mod)

#Building a Predictive Model and 
#Calculating the score of a student who studies 9.25 hours:

library(caret)
mod <- train(Scores ~ Hours, data = study, method = "lm")
new.hour <- data.frame(Hours = c(9.25))
predict(mod, newdata = new.hour)

