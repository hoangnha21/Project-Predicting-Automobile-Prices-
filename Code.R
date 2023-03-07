install.packages('corrr')
install.packages('nnet')
install.packages('NeuralNetTools')
install.packages("fastDummies")
install.packages("caret")
install.packages("neuralnet")
install.packages("ROSE")
# R libraries 
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrr)
library(nnet)
library(NeuralNetTools)
library(fastDummies)
library(caret)
library(neuralnet)
library(ROSE)
#importing the dataset
data <- read_excel("/Users/hoangnha218/Desktop/IDS572 Homework 4/Assignment4Data.xlsx")
View(data)

######## Exploratory Data Analysis (EDA)  #########
str(car) # 31 columns, 28 rows including 2 categorical and 29 numerical variables

# Remove unecessary columns
df <- data[, !names(data) %in% c("Fuel","Drs", "Cyl","Abag_1","ABS","PStr")]

# Check unique values for each categorical column
unique(df$Colour)

# Check missing values
sapply(df,function(x) sum(is.na(x))) #There are no missing values in all columns.

# Factor categorical variables
df$Colour <- as.factor(df$Colour)

# Check outliers with boxplot
boxplot(df,las=2.6,vertical = TRUE,main = "Boxplot of car data") #no significant outliers

# Calculate correlation coefficient
correlate(df)

# Test relationship between categorical and numerical variables using Chi-squared test
chisq.test(df$Price, df$Colour)

# Compute the statistics of all numerical variables
summary(df)

### Answers
#Important Factors: "HP", "CC","Grs", "Wght", "AC", "M_rim" 
