mydata <- read.csv(file.choose())
library("ggplot2")
library("plotly")
library("tidyr")
library("caTools")
library("dplyr")
View(mydata)
dim(mydata) #rows and columns
ggplot(data=mydata, aes(x=country, y=value)) +
 geom_point(col="blue")
barplot(mydata$value,type = "b",main = 'Export analysis',xlab = 'country',ylab = 'Price',col='blue')
#scatter plot type point p or line l or both b
boxplot(mydata$value)$out

mean(mydata$value)
median(mydata$value)
summary(mydata$value)
summary(mydata$HSCode)
mydata <- filter(mydata, value>100)

any(is.na(mydata)) #any missing value check in data
any(is.na(mydata$value)) #missing value check in column
sum(is.na(mydata)) #total missing values
sum(is.na(mydata$value)) #total missing value in column
na.omit(mydata) #eliminating missing values
mydata$value[is.na(mydata$value)]<-0
boxplot(mydata$value)
med <- boxplot.stats(mydata$value)$out #outliers
View(med)
#mydata1 <- unite(data=mydata,col=Country_with_HSCode,country,HSCode) #to combine two columns
#View(mydata)
#View(mydata1)


#Splitting data 

sample.split(mydata$value,SplitRatio = 0.70) -> split_index
trainingdata <- subset(mydata,split_index==T)
testdata <- subset(mydata,split_index==F)
nrow(trainingdata)
nrow(testdata)


#Model1 Linear Regression used(y=mx+c) price dependent variable
#(.dot) means all other columns except price i.e. all independent variables
lm(value~.-Commodity-country,data=trainingdata)->modell
predict(model1,testdata)->result
cbind(actual=testdata$value,predicted=result)->compare_res
View(compare_res)
as.data.frame(compare_res)->compare_res
compare_res$actual-compare_res$predicted->error
cbind(compare_res,error) -> compare_res
View(compare_res)
sqrt(mean(compare_res$error^2))->rmse1
View(rmse1)
summary(model1)
#model 2 with Linear Regression
lm(value~.,data=trainingdata)->model2
predict(model2,testdata)->result2
cbind(actual=testdata$value,predicted=result2)->compare_res2
as.data.frame(compare_res2)->compare_res2
compare_res2$actual-compare_res2$predicted->error
cbind(compare_res2,error) ->compare_res2
View(compare_res2)
sqrt(mean(compare_res2$error^2))->rmse2
View(rmse2)
boxplot(compare_res2)



#plot(mydata$value)#scatterplot for whole data
#barplot(mydata$value, main = 'Export analysis', col = 'red',horiz = FALSE)
#barplot
#summary(mydata) #mean median
#densityplot(mydata$value) #density plot
#hist(mydata$value, col = 'red')
#hist(mydata$HSCode)

#x<-rnorm(1000)
#boxplot(x,horizontal=FALSE,axes=FALSE)$out
#boxplot.stats(mydata$x)$out 
#mydata$value[mydata$value%in%med]<- median(mydata$value) outliers to median
#mydata$value[is.na(mydata$value)]<- median(mydata$value) NA value to median
