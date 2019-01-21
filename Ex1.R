#class 1

boston.df <- read.csv("BostonHousing.csv",header = TRUE)
dim(boston.df) #dimension of data frame
head(boston.df) #first 6 rows
tail(boston.df) #last 6 rows
View(boston.df) #show all data in new tab
boston.df[1:10,1] #show first 10 rows of first column
boston.df[1:10,] #show first 10 rows of each columns
boston.df[5,1:10] #show some columns of 5th row
boston.df[5,c(1:2,4,8:10)] #show the fifth row and some columns
boston.df$CRIM #show the whole first column
boston.df$CRIM[1:10] #Sjow the first 10 rows of the first column.
length(boston.df$MEDV) #find length of MEDV column
mean(boston.df$MEDV) #Find mean value of the column MEDV
summary(boston.df) #find the summary statistics of each column
names(boston.df) #print a list of variables to the screen
t(t(names(boston.df))) #print the list in a useful column format(column names)
plot(boston.df$MEDV~boston.df$LSTAT,xlab="MEDV",ylab="LSTAT") #Scatter plot with axes names
hist(boston.df$MEDV,xlab="MEDV") #histogram of MEDV
boxplot(boston.df$MEDV~boston.df$CHAS,xlab="CHAS",ylab="MEDV") #boxplot of MEDV for different values of CHAS

#class 2

set.seed(1) #to get the same parrtitions when re running the R code
train.rows<-sample(rownames(boston.df),dim(boston.df)[1]*0.6) #partitioning into training 60% and validation 40%
train.data<-boston.df[train.rows,] #collect all the columns with training row ID into training set
valid.rows<-setdiff(rownames(boston.df),train.rows) #assign row ID that are not already in the training set, into validation
valid.data <-boston.df[valid.rows,]
valid.data<-boston.df[-train.rows,] #train.rows is string , so can't be used in this database

#fit a linear model for Boston Dataset
reg<-lm(MEDV~CRIM,data=boston.df,subset=train.rows) #Fit a linear model for MEDV and CRIM
reg$coefficients #inspect the coefficients
tr.res<-data.frame(train.data$MEDV,reg$fitted.values,reg$residuals)
head(tr.res)

pred<-predict(reg,newdata=valid.data)
vl.res<-data.frame(valid.data$MEDV,pred,residuals=valid.data$MEDV - pred)
head(vl.res)

reg2<-lm(MEDV~CRIM+AGE,data=boston.df,subset=train.rows) #adding AGE too to the model increased the efficiency: more valid data added => more efficiency
reg2$coefficients

reg3<-lm(MEDV~CRIM+AGE+ZN+INDUS+NOX+CHAS+RM+DIS+RAD+TAX+PTRATIO+LSTAT,data=boston.df,subset=train.rows)
reg3$coefficients
#sample- command for taking a random sample from database

args(lm)
example(lm)
?lm

#class 3

library(psych)
describe(boston.df)

#https://r4ds.had.co.nz/index.html

#Systems.Submit@gmail.com
#vivekmenon@am.amrita.edu