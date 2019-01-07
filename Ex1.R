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

#https://r4ds.had.co.nz/index.html

#Systems.Submit@gmail.com
#vivekmenon@am.amrita.edu