buys <- c("no", "no", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "no")
credit <- c("fair", "excellent", "fair", "fair", "fair", "excellent", "excellent", "fair", "fair", "fair", "excellent", "excellent", "fair", "excellent")
student <- c("no", "no", "no","no", "yes", "yes", "yes", "no", "yes", "yes", "yes", "no", "yes", "no")
income <- c("high", "high", "high", "medium", "low", "low", "low", "medium", "low", "medium", "medium", "medium", "high", "medium")
age <- c(25, 27, 35, 41, 48, 42, 36, 29, 26, 45, 23, 33, 37, 44) # we change the age from categorical to numeric

data <- data.frame(age, income, student, credit, buys) # create a data frame using the attributes defined above, where buys is the class label
data

attach(data) # attach the dataset so that you can use the column names directly in the later coding (for example, using age instead of data$age)


############### Calculate Information Gain ###########

# Entropy before split

info <- function(CLASS.FREQ){
  freq.class <- CLASS.FREQ
  info <- 0
  for(i in 1:length(freq.class)){
    if(freq.class[[i]] != 0){ 
      entropy <- -freq.class[[i]]/sum(freq.class) * log2(freq.class[[i]]/sum(freq.class))
    }else{ 
      entropy <- 0
    }
    info <- info + entropy # sum up entropy from all classes
  }
  return(info)
}

# for example
buys.freq <- table(buys)
buys.freq
info.buys <- info(buys.freq)
info.buys


# Entropy after split
info.target <- function(ATTRIBUTE, CLASS){
  input <- data.frame(ATTRIBUTE, CLASS)
  freq <- as.matrix(table(input))
  print(freq) # here you can check the contingency table for ATTRIBUTE and CLASS
  info.target <- 0
  for(j in 1:nrow(freq)){
    ## calculate the entropy of splitting on this ATTRIBUTE
    info.target <- info.target + sum(freq[j,])/sum(freq) * info(freq[j,])
  }
  return(info.target)
}

# for example
info.income <- info.target(income, buys)
info.income

# Information gain
gain <- function(ATTRIBUTE, CLASS){
  CLASS.FREQ <- table(CLASS)
  if(class(ATTRIBUTE) == "numeric"){ # Numeric attributes
    input <- data.frame(ATTRIBUTE, CLASS)
    input <- input[order(ATTRIBUTE),] # sort the examples based on the numeric attribute
    rownames(input) <- seq(length = nrow(input)) # reorder the row names (indexes)
    gain.num <- c()
    # in this for loop, we calculate the information gain for each split point
    # that is generated between two consecutive examples
    for(i in 1:(nrow(input) - 1)){ 
      split <- (input[i, 1] + input[i+1, 1])/2
      Small <- ifelse(input[, 1] <= split, "Yes", "No")
      gain.num[i] <- info(CLASS.FREQ) - info.target(Small, CLASS)
    }
    return(max(gain.num))
  }else{ # Categorical attributes
    ## the code to calculate the information gain for this ATTRIBUTE, using the info and info.target function 
    gain.cat <-  info(CLASS.FREQ) - info.target(ATTRIBUTE, CLASS)
    return(gain.cat)
  }  
}

# for example
gain(age, buys)
gain(student, buys)

detach(data) # corresponding to the attach(data) earlier 

############# Perceptron Learning Algorithm ###############

# Create 100 uniformly distributed points in [-1,1]x[-1,1]
set.seed(5)
x1 <- runif(100, -1, 1)
x2 <- runif(100, -1, 1)
X <- data.frame(x1, x2)
# Randomly select two points to create a line going through them, points on one side
# of the line get class label -1, the ones on the other side get class label 1
p1x <- runif(1, -1, 1)
p1y <- runif(1, -1, 1)
p2x <- runif(1, -1, 1)
p2y <- runif(1, -1, 1)
slope <- (p1y-p2y)/(p1x-p2x)
intercept <- p1y - slope * p1x
y <- ifelse((slope*X[,1]+intercept) >= X[,2], -1, 1) # assign class label 
# plot the data
data <- cbind(X, y)
plot(data[data$y == -1, 1:2], xlim=c(-1,1), ylim=c(-1,1), col="red")
points(data[data$y == 1, 1:2], col="green")
abline(intercept, slope)

# Perceptron learning algorithm  
perceptron <- function(DATA, CLASS){
  X.new <- cbind(1, DATA)
  w <- matrix(0,1,3)
  while(TRUE){
    # use matrix product to calculate the hypothesis 
    hypothesis <- sign(as.matrix(w) %*% t(X.new))
    label.new <- ifelse(hypothesis >= 0, 1, -1) # use the sign of hypothesis to decide the class label
    if(all(label.new==CLASS)){
      return(w)
      break
    }else{
      where <- label.new == CLASS
      misclass <- sample(grep("FALSE", where), 1) # randomly select a misclassified point
      # update the weight vector using this randomly selected misclassified point
      w <- w + CLASS[misclass]*X.new[misclass, ]
    }   
  }  
} 

perceptron(X, y)


############# Different Classifiers #############

install.packages("tree")
install.packages("e1071")
adult_db <- read.table(file = "adult.data.txt", header=FALSE, sep=",", na.strings=c("?","NA","-"), strip.white=TRUE, stringsAsFactors=FALSE)
names(adult_db) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "class")
adult_db_nomiss <- na.omit(adult_db) # remove all the rows with missing values
row.names(adult_db_nomiss) <- 1:nrow(adult_db_nomiss)
adult_db_nomiss$class <- as.factor(adult_db_nomiss$class)
set.seed(5)
data <- adult_db_nomiss[sample(1:nrow(adult_db_nomiss), 100),] # randomly select 100 rows to be used later

attach(data)
summary(data)
set.seed(5)
data.train <- c(sample(1:100, 50)) # randomly select 50 examples as training data

## Decision Tree
library(tree)
tree.data <- tree(class~., data[data.train,])
plot(tree.data)
text(tree.data)

## Naive Bayes
library(e1071)
nb.data <- naiveBayes(class~., data[data.train,]) # 3 points 
prediction <- predict(nb.data, data[-data.train, ], type="class") # predict the class labels for the test data
res.nb <- table(prediction, data[-data.train, "class"]) # generate a confusion matrix using the predictions and the real labels of the test data

# calculate the accuracy, precision and recall by using the confusion matrix res.nb 5 points
accuracy <- sum(diag(res.nb))/sum(res.nb) 
recall <-  res.nb[2,2]/(res.nb[2,1] + res.nb[2,2])
precision <- res.nb[2,2]/(res.nb[1,2] + res.nb[2,2])

data.select <- subset(data, select=c(age, fnlwgt, education_num, capital_gain, capital_loss, hours_per_week, class))

## Support Vector Machine 
svm.data <- svm(class~., data = data.select[data.train,], cost = 1, kernel = "radial") # 3 points
#generate predictions using the predict() function
prediction <- predict(svm.data, data.select[-data.train, ], type="class") # 1 point 
#generate the confusion matrix
res.svm <- table(prediction, data.select[-data.train, "class"]) # 1 point 
#calculate accuracy
accuracy.svm <- sum(diag(res.svm))/sum(res.svm) # 1 point 

## Random Forest
install.packages("randomForest")
install.packages("ROCR")
library(randomForest)
library(ROCR)rf.data <- randomForest(class~., data = data.select[data.train,], ntree = 100, importance = TRUE)
#generate predictions using the predict() function
prediction <- predict(rf.data, data.select[-data.train, ], type="class")
#generate the confusion matrix
res.rf <- table(prediction, data.select[-data.train, "class"]) # 1 point 
#calculate accuracy
accuracy.rf <- sum(diag(res.rf))/sum(res.rf) # 1 point 
#generate the probabilities of each prediction
probability <- predict(rf.data, data.select[-data.train, ], type="prob")
# if we are interested in the class >50K, we can calculate its AUC score like this:
auc.rf <- as.numeric(performance(prediction(probability[,2], data.select[-data.train, "class"]), measure='auc')@y.values) 

dettach(data)
################################### END #####################################