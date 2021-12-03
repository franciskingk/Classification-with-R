# ads analysis


# A Kenyan entrepreneur has created an online cryptography course and
#would want to advertise it on her blog. She currently targets audiences
#originating from various countries. In the past, she ran ads to advertise
#a related course on the same blog and collected data in the process. 
#She would now like to employ your services as a Data Science Consultant
#to help her identify which individuals are most likely to click on her ads. 

# installing packages needed 
library(dplyr)
library(ggplot2)
library("data.table")



# loading the dataset provided
ads<- read.csv('/home/francis/Downloads/advertising.csv')
head(ads)


# checking for duplicate rows in the dataset
duplicated_rows <- ads[duplicated(ads),]

duplicated_rows
# there are no duplicate rows or columns in the dataset

# checking for null values in the dataset
colSums(is.na(ads))

# There are no missing values in the dataset



# finding average income range of people being targeted 
income.mean <- mean(ads$Area.Income)

income.mean
# The average income of people targeted is 55,000.

#Checking for outliers in the income column
boxplot(ads$Area.Income)

# checking the frequency of income in the ads dataset using a histogram
hist(ads$Area.Income)

# The income range thet is most frequent is from 50,000 to 70,000.

# Finding the average age of people in our dataset
ads.age <- mean(ads$Age)

ads.age

# The average age of the people in the dataset is 36 years.

# Checking for outliers in the dataset
boxplot(ads$Age)

#there are no outliers in the age column

#checking the most frequent age in the ads dataset using a histogram.
hist(ads$Age)

# the most frequent age ranges between 25 and 40 years.

# We can see the most recurrent city with mode, this will show use the city
# that is most popular with the ads.
# mode function
mode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

# getting the mode
top.city.mode <- mode(ads$City)
top.city.mode

# The most popular city in terms of ads shown is Lisa mouth

# We can find the most recurring country using mode as well

top.country <- mode(ads$Country)
top.country
# From the above we can see that Czech Republic has the most ads shown to them


# Checking the distribution in terms of gender where 1 is Male and 0 is Female
gender <- (ads$Male)
gender.frequency <- table(gender)
gender.frequency
# plotting to visualize the distribution
barplot(gender.frequency)

#From the table and bar graph above, we can see that there are more female
#viewers than male.

# Checking the most frequent time spent on site in the dataset
hist(ads$Daily.Time.Spent.on.Site)

# From the histogram we can see that the time spent on the particular site
# ranges from 70 to 85 minutes per day

# Creating scatter plot to check the correlation between age of user and
# the time they spend on the site.

age.user <- ads$Age
time.spent <- ads$Daily.Time.Spent.on.Site

plot(time.spent,age.user)

# from the graph we can see that the older people spend less time on the site,
# therefore more ads should be shown to the people of the ages of 20 to 40
# years since the group spents more time on the site.

## performing regression on the data to see which users are more likely to 
#click on the ads.

# Getting the summary of the dataset
summary(ads)
# we can see the average time spent on website is 65, the average age of the 
#customer is  36, the are less males on average.


# dropping unnecessary columns
df_drop <- c("Timestamp","Country","City","Ad.Topic>line")
new_features <- ads[-c(5,6,8,9)]
new_features
# setting the random number to get started with
set.seed(123)

# randomizing the data 
random_ads <- runif(150)
ads_random <- new_features[order(random_ads),]
head(ads_random)

# normalizing the data
normal <- function(x) (
  return( ((x-min(x)) /(max(x)-min(x))) )
)
normal(1:4)
new_fet <- normal(ads_random[,1:4])
summary(new_fet)

# splitting the train test data
train <- new_features[1:130,]
test <- new_features[131:150,]
train_sp <- ads_random[1:130,6]
test_sp <- ads_random[131:150,6]

# calling the Knn through "class" package

library(class)
require(class)
# applying Knn
model <- knn(train= train,test=test, ,cl= train_sp,k=13)

# viewing the classification results
table(factor(model))
table(test_sp,model)

# Ch ecking the accuracy of the model
mean(test_sp==model)

#