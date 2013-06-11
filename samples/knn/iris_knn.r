# load required library
library(class)

#################################################
# PREPROCESSING
#################################################

data <- iris                # create copy of iris dataframe
labels <- data$Species      # store labels
data$Species <- NULL        # remove labels from feature set (note: could
                            # alternatively use neg indices on column index in knn call)

#################################################
# TRAIN/TEST SPLIT
#################################################

set.seed(1)         # initialize random seed for consistency
                    # NOTE -- run for various seeds --> need for CV!

train.pct <- 0.7    # pct of data to use for training set
N <- nrow(data)     # total number of records (150)

train.index <- sample(1:N, train.pct * N)       # random sample of records (training set)

train.data <- data[train.index, ]       # perform train/test split
test.data <- data[-train.index, ]       # note use of neg index...different than Python!

train.labels <- as.factor(as.matrix(labels)[train.index, ])     # extract training set labels
test.labels <- as.factor(as.matrix(labels)[-train.index, ])     # extract test set labels

#################################################
# APPLY MODEL
#################################################

err.rates <- data.frame()       # initialize results object

max.k <- 100
for (k in 1:max.k)              # perform fit for various values of k
{
    knn.fit <- knn(train = train.data,          # training set
                    test = test.data,           # test set
                    cl = train.labels,          # true labels
                    k = k                       # number of NN to poll
               )

    cat('\n', 'k = ', k, ', train.pct = ', train.pct, '\n', sep='')     # print params
    print(table(test.labels, knn.fit))          # print confusion matrix

    this.err <- sum(test.labels != knn.fit) / length(test.labels)    # store gzn err
    err.rates <- rbind(err.rates, this.err)     # append err to total results
}

#################################################
# OUTPUT RESULTS
#################################################

results <- data.frame(1:max.k, err.rates)   # create results summary data frame
names(results) <- c('k', 'err.rate')        # label columns of results df

# create title for results plot
title <- paste('knn results (train.pct = ', train.pct, ')', sep='')

# create results plot
results.plot <- ggplot(results, aes(x=k, y=err.rate)) + geom_point() + geom_line()
results.plot <- results.plot + ggtitle(title)

# draw results plot (note need for print stmt inside script to draw ggplot)
print(results.plot)

#################################################
# NOTES
#################################################

# R docs
# http://cran.r-project.org/web/packages/class/class.pdf
# http://cran.r-project.org/web/packages/DMwR/DMwR.pdf
