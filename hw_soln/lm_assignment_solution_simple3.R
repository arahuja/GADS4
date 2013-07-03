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

create.dtm <- function(src)
{
  c <- Corpus(src)
  c <- tm_map(c, removeNumbers)
  c <- tm_map(c, removePunctuation)
  c <- tm_map(c, stripWhitespace)
  c <- tm_map(c, tolower)
  c <- tm_map(c, removeWords, stopwords("english"))  
  dtm <-DocumentTermMatrix(c)
  dtm <- removeSparseTerms(dtm, 0.99)
  return(dtm)
}

create.title.features <- function(data)
{
  src <- DataframeSource(data.frame(data$Title))
  dtm <- create.dtm(src)
  text_data <- as.matrix(dtm)
  return(text_data)
}

create.desc.features <- function(data)
{
  src <- DataframeSource(data.frame(data$FullDescription))
  dtm <- create.dtm(src)
  text_data <- as.matrix(dtm) 
  return(text_data)
}

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

#Bind on text features
#features <- cbind(features, create.desc.features(all_data))

features <- cbind(features, create.title.features(all_data))

#Split data into train and test
train <- features[(1:train.N),]
test <- features[(train.N:nrow(features)),]


#Build and test model
model <- lm(log(SalaryNormalized) ~., data=train)
test.predict <- predict(model, test)

#library('glmnet')
#reg.model <- cv.glmnet(model.matrix(~., data=train[, -1]), log(train$SalaryNormalized))
#test.predict <- predict(reg.model, model.matrix(~., data=test[, -1]), s=reg.model$lambda.min)

mae(exp(test.predict), test$SalaryNormalized)
