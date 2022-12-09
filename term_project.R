### LIBRARIES
library(ggplot2)
library(dplyr)
library(ggpubr)
library(rpart)
library(rpart.plot)
library(randomForest)

### DATA MUNGING
# read files
by_city <- read.csv(file="./data/2015_CHP_PUD.csv", nrows=6)
by_district <- read.csv(file="./data/2015_CHP_PUD.csv")
by_district <- by_district[-(1:6),]
by_district <- head(by_district, -3)

# column summaries
summary(by_district$Schoolabsent_rate)
summary(by_district$Sugary_Drink)
summary(by_district$Fruit_Veg)
summary(by_district$Exercise)
summary(by_district$Obesity)
summary(by_district$Diabetes)

# extract columns
new_city <- by_city[c('ID', 'Name', 'Schoolabsent_rate','Sugary_Drink','Fruit_Veg', 'Exercise', 'Obesity', 'Diabetes')]
new_district <- by_district[c('ID', 'Name', 'Schoolabsent_rate','Sugary_Drink','Fruit_Veg', 'Exercise', 'Obesity', 'Diabetes')]

# add column for borough
new_district <- new_district %>% mutate(Borough = substr(ID, 0, 1))


### EXPLORATORY DATA ANALYSIS
# correlation coefficient
cor(new_city$Schoolabsent_rate, new_city$Sugary_Drink)
cor(new_city$Schoolabsent_rate, new_city$Fruit_Veg)
cor(new_city$Schoolabsent_rate, new_city$Exercise)
cor(new_city$Schoolabsent_rate, new_city$Obesity)
cor(new_city$Schoolabsent_rate, new_city$Diabetes)

cor(new_district$Schoolabsent_rate, new_district$Sugary_Drink)
cor(new_district$Schoolabsent_rate, new_district$Fruit_Veg)
cor(new_district$Schoolabsent_rate, new_district$Exercise)
cor(new_district$Schoolabsent_rate, new_district$Obesity)
cor(new_district$Schoolabsent_rate, new_district$Diabetes)

hist(new_district$Schoolabsent_rate, main="School Absent Rate Histogram", xlab="School Absent Rate")
hist(new_district$Sugary_Drink, main="Sugary Drink Consumption Histogram", xlab="Sugary Drink Consumption")
hist(new_district$Fruit_Veg, main="Fruit/Vegetable Consumption Histogram", xlab="Fruit/Vegetable Consumption")
boxplot(new_district$Schoolabsent_rate, new_district$Sugary_Drink, new_district$Fruit_Veg, names=c("Absent Rate", "Sugary Drink", "Fruit/Veg"), main="Boxplots")

hist(new_district$Exercise, main="Exercise Histogram", xlab="Exercise")
hist(new_district$Obesity, main="Obesity Histogram", xlab="Obesity")
hist(new_district$Diabetes, main="Diabetes Histogram", xlab="Diabetes")

# scatter plot
ggplot(new_district, aes(x=Schoolabsent_rate, y=value, color=variable)) + 
  geom_point(aes(y=Sugary_Drink, col="Sugary Drink Consumption")) + 
  geom_point(aes(y=Obesity, col="Obesity")) +
  geom_point(aes(y=Diabetes, col="Diabetes")) +
  labs(title="Variables Compared to Elementary School Absenteeism", subtitle="Diabetes, Obesity, Sugary Drink Consumption") +
  ylab("Elementary School Absent Rate") + 
  xlab("Value")

ggplot(new_district, aes(x=Schoolabsent_rate, y=value, color=variable)) + 
  geom_point(aes(y=Fruit_Veg, col="Fruit/Vegetable Consumption")) + 
  geom_point(aes(y=Exercise, col="Exercise")) +
  labs(title="Variables Compared to Elementary School Absenteeism", subtitle="Fruit/Vegetable Consumption, Exercise") +
  ylab("Elementary School Absent Rate") + 
  xlab("Value")


### MODELS
# multivariate regression
# predicted positive correlation
pos_cor <- lm(Schoolabsent_rate ~ Sugary_Drink + Obesity + Diabetes, data = new_district)
pos_cor
summary(pos_cor)

#predicted negative correlation
neg_cor <- lm(Schoolabsent_rate ~ Fruit_Veg + Exercise, data = new_district)
neg_cor
summary(neg_cor)

# all variables
all_var <- lm(Schoolabsent_rate ~ Sugary_Drink + Fruit_Veg + Exercise + Obesity + Diabetes, data = new_district)
all_var
summary(all_var)

# linear regression
sugar <- lm(Schoolabsent_rate ~ Sugary_Drink, data = new_district)
summary(sugar)
obesity <- lm(Schoolabsent_rate ~ Obesity, data = new_district)
summary(obesity)
diabetes <- lm(Schoolabsent_rate ~ Diabetes, data = new_district)
summary(diabetes)
fruitveg <- lm(Schoolabsent_rate ~ Fruit_Veg, data = new_district)
summary(fruitveg)
exercise <- lm(Schoolabsent_rate ~ Exercise, data = new_district)
summary(exercise)

ggplot(new_district, aes(x=Schoolabsent_rate, y=Sugary_Drink, col=Borough)) + geom_point() +
  scale_color_discrete(labels=c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Staten Island')) +
  labs(title="School Absenteeism vs. Sugary Drink Consumption", subtitle="Separated by Region") +
  ylab("Elementary School Absent Rate") +
  xlab("Sugary Drink Consumption") +
  geom_smooth(data=new_district[new_district$Borough == "1",], method='lm', formula=y~x, se=F) +
  geom_smooth(data=new_district[new_district$Borough == "2",], method='lm', formula=y~x, se=F) +
  geom_smooth(data=new_district[new_district$Borough == "3",], method='lm', formula=y~x, se=F) +
  geom_smooth(data=new_district[new_district$Borough == "4",], method='lm', formula=y~x, se=F) +
  geom_smooth(data=new_district[new_district$Borough == "5",], method='lm', formula=y~x, se=F)

ggplot(new_district, aes(x=Schoolabsent_rate, y=Fruit_Veg, col=Borough)) + geom_point() +
  scale_color_discrete(labels=c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Staten Island')) +
  labs(title="School Absenteeism vs. Fruit/Vegetable Consumption", subtitle="Separated by Region") +
  xlab("Elementary School Absent Rate") +
  ylab("Fruit/Vegetable Consumption") +
  geom_smooth(data=new_district[new_district$Borough == "1",], method='lm', formula=y~x, se=F) +
  geom_smooth(data=new_district[new_district$Borough == "2",], method='lm', formula=y~x, se=F) +
  geom_smooth(data=new_district[new_district$Borough == "3",], method='lm', formula=y~x, se=F) +
  geom_smooth(data=new_district[new_district$Borough == "4",], method='lm', formula=y~x, se=F) +
  geom_smooth(data=new_district[new_district$Borough == "5",], method='lm', formula=y~x, se=F)

# cook's distance
pos_cooks <- cooks.distance(pos_cor)
plot(pos_cooks, col="dark green", pch=20, main="Cook's Distance - Positive Correlation", ylab="Value")
text(pos_cooks, cex=0.4, adj=c(-0.5,0.5))

neg_cooks <- cooks.distance(neg_cor)
plot(neg_cooks, col="dark red", pch=20, main="Cook's Distance - Negative Correlation", ylab="Value")
text(neg_cooks, cex=0.4, adj=c(-0.5,0.5))

all_cooks <- cooks.distance(all_var)
plot(all_cooks, col="navy", pch=20, main="Cook's Distance - All Variables", ylab="Value")
text(all_cooks, cex=0.4, adj=c(-0.5,0.5))


# kmeans
kmeans_sugarydrink <- ggplot(new_district, aes(x = Sugary_Drink, y = Schoolabsent_rate, col = Borough)) + geom_point() + stat_ellipse() +
  scale_color_discrete(labels=c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Staten Island')) +
  ylab("Elementary School Absent Rate") +
  xlab("Sugary Drink Consumption")

kmeans_fruitveg <- ggplot(new_district, aes(x = Fruit_Veg, y = Schoolabsent_rate, col = Borough)) + geom_point() + stat_ellipse() + 
  scale_color_discrete(labels=c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Staten Island')) +
  ylab("Elementary School Absent Rate") +
  xlab("Fruit/Vegetable Consumption")

kmeans_obesity <- ggplot(new_district, aes(x = Obesity, y = Schoolabsent_rate, col = Borough)) + geom_point() + stat_ellipse() +
  scale_color_discrete(labels=c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Staten Island')) +
  ylab("Elementary School Absent Rate") +
  xlab("Obesity")

kmeans_diabetes <- ggplot(new_district, aes(x = Diabetes, y = Schoolabsent_rate, col = Borough)) + geom_point() + stat_ellipse() +
  scale_color_discrete(labels=c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Staten Island')) +
  ylab("Elementary School Absent Rate") +
  xlab("Diabetes")

kmeans_exercise <- ggplot(new_district, aes(x = Exercise, y = Schoolabsent_rate, col = Borough)) + geom_point() + stat_ellipse() +
  scale_color_discrete(labels=c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Staten Island')) +
  ylab("Elementary School Absent Rate") +
  xlab("Exercise")

figure <- ggarrange(kmeans_sugarydrink, kmeans_fruitveg, kmeans_obesity, kmeans_diabetes, kmeans_exercise, labels = c("Sugary Drink Consumption", "Fruit/Vegetable Consumption", "Obesity", "Diabetes", "Exercise"), ncol = 2, nrow = 3)
figure

summary(new_district$Schoolabsent_rate) # min = 4, max = 40 -> 4:15, 16:28, 29:40
# split data into three levels of absent rate
new_district$absent_distribution <- cut(new_district$Schoolabsent_rate, br=c(0, 15, 28, 40), labels=c("Low", "Moderate", "High"))
barplot(table(new_district$absent_distribution), ylim=c(0, 25), main="Absent Level Barplot", ylab="Frequency")

absent_kmeans <- ggplot(new_district, aes(x=Sugary_Drink, y=Obesity, col = absent_distribution)) + geom_point() + stat_ellipse() +
  scale_color_discrete(labels=c('Low', 'Moderate', 'High')) +
  ylab("Obesity") +
  xlab("Sugary Drink Consumption") +
  labs(title="Sugary Drink Consumption vs. Obesity", subtitle="Separated by Absentee Level")
absent_kmeans

# DECISION TREE
# simple
rpart_data <- rpart(absent_distribution ~ Sugary_Drink + Fruit_Veg + Obesity + Diabetes + Exercise, data=new_district, method="class")
rpart_data
summary(rpart_data)
rpart.plot(rpart_data)

# all variables
rpart_data2 <- rpart(absent_distribution ~ Sugary_Drink + Fruit_Veg + Obesity + Diabetes + Exercise, data=new_district, control = rpart.control(maxdepth = 30, minsplit = 1, minbucket = 1, cp=0))
rpart_data2
summary(rpart_data2)
rpart.plot(rpart_data2)

# RANDOM FOREST
set.seed(324)
rf_train <- sample(nrow(new_district), 0.7 * nrow(new_district), replace = FALSE)
rf_train_set <- new_district[rf_train, ]
rf_valid_set <- new_district[-rf_train, ]

rf1 <- randomForest(absent_distribution ~ Sugary_Drink + Fruit_Veg + Obesity + Diabetes + Exercise, data = rf_train_set, importance = TRUE)
rf1
rf2 <- randomForest(absent_distribution ~ Sugary_Drink + Fruit_Veg + Obesity + Diabetes + Exercise, data = rf_train_set, ntree = 350, importance = TRUE)
rf2
rf3 <- randomForest(absent_distribution ~ Sugary_Drink + Fruit_Veg + Obesity + Diabetes + Exercise, data = rf_train_set, ntree = 80, mtry = 3, importance = TRUE)
rf3

rf_pred_valid1 <- predict(rf1, rf_valid_set, type = "class")
table(rf_pred_valid1, rf_valid_set$absent_distribution)

rf_pred_valid2 <- predict(rf2, rf_valid_set, type = "class")
table(rf_pred_valid2, rf_valid_set$absent_distribution)

rf_pred_valid3 <- predict(rf3, rf_valid_set, type = "class")
table(rf_pred_valid3, rf_valid_set$absent_distribution)
