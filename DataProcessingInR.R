#####################################################################
Bhargav Deshpande
Data Processing Using R
#####################################################################

# 1. Data Loading 
library(utils)

data.df = read.csv("../train.csv")

#2. Data Summarization
data_arr = dim(data.df)
data.df.n_rows = data_arr[1]
data.df.n_cols = data_arr[2]

#3. Data Subsetting
data.df.subset = data.df[,c("PassengerId","Age","Fare","Embarked")]

#4. Data cleaning
#4a. For the column 'Age', replacing missing/NA value with the median
data.df.subset$Age[is.na(data.df.subset$Age) | data.df.subset$Age ==""] = median(data.df.subset$Age,na.rm = TRUE)


#4b. For the column 'Embarked' replacing missing/NA value with the mode
#find the unique values from the column
uniqueTemp = unique(data.df.subset$Embarked)
#match function will return vector with position matched, tabulate will return the array with count
#which.max will will pick position of hightest count value
#Replacing blank or NA with mode
data.df.subset$Embarked[is.na(data.df.subset$Embarked) | data.df.subset$Embarked == ""] = uniqueTemp[which.max(tabulate(match(data.df.subset$Embarked, uniqueTemp)))]

#4c.
#Looking at the subset, there are some entries for 'Fare' where values are '0.0000'
# best way to handle this data is to repalce these values by "mean"
# So, the clean the data, replacing 0.0000 values with the "mean" value
# Need to take care to omit zero values while calcuating the mean
data.df.subset$Fare[data.df.subset$Fare == 0] = mean(data.df.subset$Fare[data.df.subset$Fare != 0])

#5. Data visualization
#5a. histogram plot based on the 'Age' variable from data.df.subset
hist(data.df.subset$Age)
#5b. scatter plot with 'Age' on x-axis and 'Fare' on yaxis
plot(data.df.subset$Age, data.df.subset$Fare, xlab = "Age", ylab = "Fare")

#6. anomalies in the 'Age' variable
#Finding the mean first
m = mean(data.df.subset$Age)
#Finding the standard deviation
d = sd(data.df.subset$Age)
# anomalies are values less than m-2*d or greater than m+2*d
anomalous_indices = data.df.subset$PassengerId [data.df.subset$Age < m-2*d | data.df.subset$Age > m+2*d]

#7 Data subsetting part 2
#7a and 7b combined.Age >=25 and Age <=80 and contain only the following columns: Age, Fare and Embarked
data.df.subset.v2 = subset(data.df.subset[,c("Age","Fare","Embarked")], data.df.subset$Age >=25 & data.df.subset$Age <=80)

#8 Rescale the column 'Fare' in data.df.subset.v2 into the 0-100 range
#MAthematical Formula for rescaling is
#[(max_n - min_n)/(max_o - min_o)]*(x - min_o) + min_n
# max_n = max value of new scale (100 in our case)
# min_n = min value of new scale (0 in our case)
# max_o = max value of old scale
# min_0 = min value of old scale
# x = current value
# In our case, formula becomes 100/(max_o - min_o) * (data.df.subset.v2$Fare - min_o)
# Assigned to new column data.df.subset.v2$Fare_Rescaled
max_o = max(data.df.subset.v2$Fare)
min_o = min(data.df.subset.v2$Fare)
data.df.subset.v2$Fare_Rescaled = 100/(max_o - min_o) * (data.df.subset.v2$Fare - min_o)




