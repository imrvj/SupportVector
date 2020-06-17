loans<-read.csv('loan_data.csv')
summary(loans)
str(loans)
loans$credit.policy <- factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)

##histogram of fico scores colored by not.fully.paid
library(ggplot2)

pl<-ggplot(loans,aes(fico))+geom_histogram(aes(fill=not.fully.paid),bins = 30,color='black')
print(pl)

# barplot of purpose counts, colored by not.fully.paid. Use position=dodge in the geom_bar argument
pl2<-ggplot(loans,aes(purpose))+geom_bar(aes(fill=not.fully.paid),position = 'dodge')
print(pl2)

#scatterplot of fico score versus int.rate.
pl3<-ggplot(loans,aes(int.rate,fico))+geom_point(aes(color=not.fully.paid),alpha=0.3)
print(pl3)


#MOdel

library(caTools)
set.seed(101)

spl = sample.split(loans$not.fully.paid, 0.7)

train = subset(loans, spl == TRUE)

test = subset(loans, spl == FALSE)

library(e1071)
model<- svm(not.fully.paid~.,data = train)

summary(model)

predicted.values<-predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)


#Tuning MOdel

tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',
                     ranges=list(cost=c(1,10), gamma=c(0.1,1)))

model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)
