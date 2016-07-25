
data = read.csv('Collection_Data_Science_TakeHome_Challenges/conversion_data.csv')
head(data)
str(data)
summary(data)
sort(unique(data$age),decreasing=TRUE)
subset( data ,age>79)
data =subset(data,age<80)
library(ggplot2)
library (magrittr)

data_country = data %>% group_by(country) %>%
  summarise(conversion_rate = mean(converted))
ggplot(data=data_country, aes(x=country, y=conversion_rate))+ geom_bar(stat = "identity", aes(fill = country))

data_pages = data %>%
  group_by(total_pages_visited) %>%
  summarise(conversion_rate = mean(converted))
qplot(total_pages_visited, conversion_rate, data=data_pages, geom="line")

data$converted = as.factor(data$converted) # let's make the class a factor
data$new_user = as.factor(data$new_user) #also this a factor
levels(data$country)[levels(data$country)=="Germany"]="DE" # Shorter name, easier to plot.


train_sample = sample(nrow(data), size = nrow(data)*0.66)
train_data = data[train_sample,]
test_data = data[-train_sample,]
library(randomForest)
rf =randomForest(y=train_data$converted,x=train_data[,-ncol(train_data)],
                 ytest=test_data$converted,xtest=test_data[,-ncol(test_data)],
                 ntree=100,mtry=3, keep.forset=TRUE)
rf
varImpPlot(rf,type=2)
# Total pages visited is the most important one by far. Unfortunetly , it is probabaly th eleast "actionalable"
# People visit many pages cause they alreadt want to buy. Also in order to buy you have 
# to click on the multiple pages.
# Let's rebuild the RF without the variable. Since classes are heavily unbalanced and we 
# don't have the very powefull variable anymore. Let's change the 
# weight a bit just to make we will get something classified as 1

rf = randomForest(y=train_data$converted, x=train_data[,-c(5,ncol(train_data))],
                  ytest=test_data$converted,xtest=test_data[,-c(5,ncol(train_data))],
                  ntree=100,mtry=3,keep.forest = TRUE, classwt=c(0.7,0.3))
rf
# Accuracy went down, but thats's fine. The model is still good enough to give us insight
# Let's recheckvariable importance
varImpPlot(rf, type=2)
# Interesting ! new user is most important one. Sources doesn't seem
# to matter at all. Let's check partial dependence plots for the 4 vars:
op <- par(mfrows=c(2,2))
partialPlot(rf,train_data, country,1)
partialPlot(rf,train_data, age,1)
partialPlot(rf,train_data, new_user,1)
partialPlot(rf,train_data, source,1)


# In partial dependence plots, we just care about the trend, not the actual y value. So this shows that:
# Users with an old account are much better than new users
# China is really bad, all other countries are similar with Germany being the best
# The site works very well for young people and bad for less young people (>30 yrs
# old) Source is irrelevant
library(rpart)
tree = rpart(data$converted ~ ., data[, -c(5,ncol(data))],
             control = rpart.control(maxdepth = 3),
             parms = list(prior = c(0.7, 0.3))
            )
tree
library("rpart.plot")         
library("rattle")
library("RColorBrewer")
fancyRpartPlot(tree)
