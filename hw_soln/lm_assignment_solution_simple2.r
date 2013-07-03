library('tm')

mae <- function(x,y)
{
  mean( abs(x-y))
}




location_info <- read.table('~/Dropbox/src/GADS4/data/kaggle_salary/Location_Tree2.csv', fill=T)
test.orig <- read.csv("~/Dropbox/src/GADS4/data/kaggle_salary/test-full.csv")
train.orig <- read.csv("~/Dropbox/src/GADS4/data/kaggle_salary/train.csv")
train.N <- nrow(train.orig)
all_data <- rbind(train.orig, test.orig)

##Start with basic feature set
features <- all_data[,c('SalaryNormalized','ContractTime','ContractType')]

#Add category features
category.counts <- summary(all_data$Category)
top.categories <- names(category.counts[order(category.counts, decreasing= TRUE)][1:25])
features$NewCategoryFeature <- factor(all_data$Category, levels=top.categories)
features$NewCategoryFeature <- addNA(features$NewCategoryFeature)

#Add location features
location.counts <- summary(all_data$LocationNormalized)
top.locations <- names(location.counts[order(location.counts, decreasing= TRUE)][1:25])
features$Location <- factor(all_data$LocationNormalized, levels=top.locations)
features$Location <- addNA(features$Location)

#Split data into train and test
train <- features[(1:train.N),]
test <- features[(train.N:nrow(features)),]


#Build and test model
model <- lm(log(SalaryNormalized) ~ Location + NewCategoryFeature + ContractTime + ContractType, data=train)
test.predict <- predict(model, test)
mae(exp(test.predict), test$SalaryNormalized)