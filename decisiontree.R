library(MASS) 
library(rpart) 
head(birthwt) 
hist(birthwt$bwt) 
table(birthwt$low) 
#build model 
#all the variables are stored as numeric. 
#we need to convert the categorical variables to factor. 
cols <- c('low', 'race', 'smoke', 'ht', 'ui') 
birthwt[cols] <- lapply(birthwt[cols], as.factor) 
#test and training set 
set.seed(1)#pseudo random number starts from 1 
train <- sample(1:nrow(birthwt), 0.75 * nrow(birthwt)) 
#birthwtTree <- rpart(low ~ smoke+ bwt, data = birthwt[train, ], method = 'class') 
birthwtTree <- rpart(low ~ . - bwt, data = birthwt[train, ], method = 'class') 
#Since low = bwt <= 2.5, we exclude bwt from the model,  
#and since it is a classification task, we specify method = 'class' 
plot(birthwtTree) 
text(birthwtTree, pretty = 0) 
summary(birthwtTree) 
birthwtPred <- predict(birthwtTree, birthwt[-train, ], type = 'class') 
table(birthwtPred, birthwt[-train, ]$low) 