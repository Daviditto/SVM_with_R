df <- read.csv('loan_data.csv')
summary(df)
str(df)

df$inq.last.6mths <- factor(df$inq.last.6mths) 
df$delinq.2yrs <- factor(df$delinq.2yrs)
df$pub.rec <- factor(df$pub.rec) 
df$not.fully.paid <- factor(df$not.fully.paid)
df$credit.policy <- factor(df$credit.policy)

# data EDA
library(ggplot2)
ggplot(df, aes(fico)) +geom_histogram(aes(fill = not.fully.paid), bins=50, color='black')
ggplot(df, aes(purpose)) +geom_bar(aes(fill = not.fully.paid), position = 'dodge') +scale_x_discrete(guide = guide_axis(angle = 90))
ggplot(df, aes(fico, int.rate)) +geom_point(aes(color=not.fully.paid), alpha=0.5)

# train test split
library(caTools)
split <- sample.split(df, SplitRatio = 0.7)
train <- subset(df, split=T)
test <- subset(df, split=F)

#build the model 
library(e1071)
sv <- svm(not.fully.paid~., data=train)
summary(sv)
colnames(test)

pred <- predict(sv,test[1:13])
table(test$not.fully.paid, pred)

# tune the model
tune.result <- tune(svm, train.x=not.fully.paid~., data=train, kernel='radial',ranges = list(cost=c(1,10), gamma=c(0.1, 1)), tunecontrol = tune.control(sampling = "fix"))
??tune

final.model <- svm(not.fully.paid~., data=train, cost=1,gamma=0.1)
pred.new <- predict(final.model, test[1:13])
table(test$not.fully.paid, pred.new)





