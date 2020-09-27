library(dplyr)
library(corrplot)
library(Metrics)
library(ggplot2)
library(car)
library(caret)
#Loading the test and train data
train <- read.csv("Train.csv")
test <- read.csv("Test.csv")

dim(train)
dim(test)

names(train)
names(test)

head(train)
summary(train)

miss_val_train <- sapply(train, function(x) sum(is.na(x)))
sapply(train, class)

miss_val_test <- sapply(test, function(x) sum(is.na(x)))
sapply(train, class)

g1 <- ggplot(train, aes(x=(Item_MRP), y=Item_Outlet_Sales))
g1 <- g1 + geom_point(col= "blue", alpha=0.5)
g1 <- g1 + geom_smooth(method = "lm",formula= y~x, col = "black")
g1

#In the above graph we see a fairly increasing trend between Item_MRP and Item_Outlet_Sales

g <- ggplot(train, aes(x=Item_Visibility, y= Item_Outlet_Sales))
g <- g + geom_point(aes(col=train$Outlet_Type))
g <- g + ggtitle("Item_MRP vs Item_Type")
g

#From the above graph we can infer that Items with less that 0.2% visibility are sold. Also difference between sales of Grocery Store and other Outlet_Type is evident

g1 <- ggplot(train, aes(x=Item_Weight, y=Item_Outlet_Sales))
g1 <- g1 + geom_point(col= "blue", alpha=0.5)
g1 <- g1 + geom_smooth(method = "lm",formula= y~x, col = "black")
g1

#We see a constant relationship between Item_Weight and Sales, or in other words increse in item weight is not impacting sales 

g <- ggplot(train, aes(x=Outlet_Identifier, y= Item_Weight))
g <- g + geom_boxplot(aes(group=train$Outlet_Identifier, fill= factor(train$Outlet_Identifier)))
g <- g + theme(axis.text.x = element_text(angle = 70,vjust = 0.5, color = "red"))
g <- g + ggtitle("Outlet_Identifier vs Item_Weight")
g

#In the above graph we can see that data for Item_Weight is missing for two outlets

g <- ggplot(train, aes(x=Outlet_Identifier, y= Item_Outlet_Sales))
g <- g + geom_boxplot(aes(group=train$Outlet_Identifier, fill= factor(train$Outlet_Identifier)))
g <- g + theme(axis.text.x = element_text(angle = 70,vjust = 0.5, color = "red"))
g <- g + ggtitle("Outlet_Identifier vs Item_Outlet_Sales")
g

#From the above graph we can infer that sales are particularly low for two outlets

g <- ggplot(train, aes(x=Item_Type, y= Item_Outlet_Sales))
g <- g + geom_bar(stat="identity", aes(fill= factor(train$Item_Type)))
g <- g + theme(axis.text.x = element_text(angle = 70,vjust = 0.5, color = "red"))
g <- g + ggtitle("Item_Type vs Item_Outlet_Sales")
g

#From the above graph, we can infer that Fruits and Vegetables contribute to the highest amount of outlet sales followed by snack foods and household products

g <- ggplot(train, aes(x=Outlet_Type, y= Item_Outlet_Sales))
g <- g + geom_boxplot(aes(group=train$Outlet_Type))
g <- g + theme(axis.text.x = element_text(angle = 70,vjust = 0.5, color = "red"))
g <- g + ggtitle("Outlet_Identifier vs Item_Outlet_Sales")
g

#From the above graph we can infer that sales of Grocery Store is the least 

g <- ggplot(train, aes(x=Outlet_Location_Type, y= Item_Outlet_Sales))
g <- g + geom_boxplot(aes(group=train$Outlet_Location_Type))
g <- g + theme(axis.text.x = element_text(angle = 70,vjust = 0.5, color = "red"))
g <- g + ggtitle("Outlet_Identifier vs Item_Outlet_Sales")
g

#We can see that there is no much impact of Outlet_Location_Type on sales

g <- ggplot(train, aes(x=Item_Type, y= Item_MRP))
g <- g + geom_boxplot(aes(group=train$Item_Type))
g <- g + theme(axis.text.x = element_text(angle = 70,vjust = 0.5, color = "red"))
g <- g + ggtitle("Item_Type vs Item_MRP")
g


n1 <- nrow(train)
n2<-nrow(test)

test$Item_Outlet_Sales <- 1
complete <- rbind(train, test)
miss_val<-sapply(complete,function(x) sum(is.na(x)))
sapply(complete,class)

sum_item_weight <- sum(complete[[2]][!is.na(complete[[2]])])
mean_item_weight <- sum_item_weight/(nrow(complete)-miss_val[2])
complete$Item_Weight[is.na(complete$Item_Weight)] <- mean_item_weight

sum_item_visibility <- sum(complete[[4]][!is.na(complete[[4]])])
mean_item_visibility <- sum_item_visibility/(nrow(complete)-sum(complete[[4]]==0))

set.seed(125)
x <- sample(levels(complete[[9]]),sum(is.na(complete[[9]])), replace = TRUE)
complete[[9]][is.na(complete[[9]])]<- x

levels(complete$Item_Fat_Content)
complete$Item_Fat_Content <- gsub("reg", "Regular",complete[[3]])
complete$Item_Fat_Content <- gsub("LF|low fat", "Low Fat",complete[[3]])
complete[[3]] <- ifelse(complete[[3]]=="Regular",0,1)

h <- as.numeric(as.character(complete$Outlet_Establishment_Year))
complete$Outlet_Estd_YearPassed <- 2013 - h

ot_count <- complete %>% group_by(Outlet_Identifier) %>% tally()
names(ot_count)[2] <- "Outlet_Count"
complete <- full_join(complete,ot_count, by = "Outlet_Identifier")

it_count <- complete %>% group_by(Item_Identifier) %>% tally()
names(it_count)[2] <- "Item_Count"
complete <- full_join(complete,it_count, by = "Item_Identifier")

unlist(sapply(split(complete,complete$Item_Type),count))
complete$Item_Type_new <- substr(complete$Item_Identifier,1,2)
complete$Item_Type_new <- gsub("FD", "Food",complete$Item_Type_new)
complete$Item_Type_new <- gsub("DR", "Drinks",complete$Item_Type_new)
complete$Item_Type_new <- gsub("NC", "Non consumable",complete$Item_Type_new)
complete$Item_Type_new <- factor(complete$Item_Type_new)

train <- complete[1:nrow(train),]
test <- complete[(nrow(train)+1):nrow(complete), ]

samp1 <- select(.data = train,Item_Type_new,Outlet_Size, Outlet_Location_Type, Outlet_Type)
ohe1 <- data.frame(model.matrix(~.-1,samp1))
mod_train1 <- select(.data = train,Item_Count, Outlet_Count, Item_Weight, Item_Fat_Content, Item_Visibility, Item_MRP, Outlet_Estd_YearPassed, Item_Outlet_Sales)
newdata1 <- cbind(ohe1, mod_train1)
fit1 <- lm(Item_Outlet_Sales~., data=newdata1)
summary(fit1)
plot(fit1)

samp2 <- select(.data = train, Item_Type_new,Outlet_Size, Outlet_Type)
ohe2 <- data.frame(model.matrix(~.-1,samp2))
mod_train2 <- select(.data = train, Outlet_Count,Item_Weight, Item_MRP, Outlet_Estd_YearPassed, Item_Outlet_Sales)
newdata2 <- cbind(ohe2, mod_train2)
fit2 <- lm(Item_Outlet_Sales~., data=newdata2)
summary(fit2)
plot(fit2)

samp2 <- select(.data = train, Item_Type_new,Outlet_Type)
ohe2 <- data.frame(model.matrix(~.-1,samp2))
mod_train2 <- select(.data = train,Item_Count, Outlet_Count,Item_Visibility, Item_MRP, Outlet_Estd_YearPassed, Item_Outlet_Sales)
newdata2 <- cbind(ohe2, mod_train2)
fit2_log <- lm(log(Item_Outlet_Sales)~., data=newdata2)
summary(fit2_log)
plot(fit2_log)

samp3 <- select(.data = train,Item_Type_new,Outlet_Type)
ohe3 <- data.frame(model.matrix(~.-1,samp3))
mod_train3 <- select(.data = train, Item_Count, Outlet_Count,Item_MRP,Item_Visibility, Outlet_Estd_YearPassed, Item_Outlet_Sales)
newdata3 <- cbind(ohe3, mod_train3)
fit3 <- lm(sqrt(log(Item_Outlet_Sales))~.+log(train$Item_MRP), data=newdata3)
summary(fit3)
plot(fit3)

par(mfrow=c(2,2))
plot(fit3)
rmse(train$Item_Outlet_Sales, exp((fit3$fitted.values)^2))

r <- cooks.distance(fit3)
m <-match(r[r>abs(4/nrow(train))], r)
p <- !(row.names(train) %in% m)

out <- train[m,]
write.csv(out, file = "outlier.csv")

length(m)/nrow(train)
samp5 <- select(.data = train[p,],Item_Type_new, Outlet_Type)
ohe5 <- data.frame(model.matrix(~.-1,samp5))
mod_train5 <- select(.data = train[p,],Item_Count, Outlet_Count,Item_Visibility, Outlet_Estd_YearPassed, Item_Outlet_Sales)
newdata5 <- cbind(ohe5, mod_train5)
fit5 <- lm(sqrt(log(Item_Outlet_Sales))~.+log(train$Item_MRP[p]), data=newdata5)
summary(fit5)
plot(fit5)

rmse(train$Item_Outlet_Sales[p], exp((fit5$fitted.values)^2))

samp6 <- select(.data = train[m,], Item_Type_new,Outlet_Type)
ohe6 <- data.frame(model.matrix(~.-1,samp6))
mod_train6 <- select(.data = train[m,],Item_Count, Outlet_Count,Item_Visibility,Outlet_Estd_YearPassed, Item_Outlet_Sales)
newdata6 <- cbind(ohe6, mod_train6)
fit6 <- lm(log(Item_Outlet_Sales)~.+log(train$Item_MRP[m]), data=newdata6)
summary(fit6)
plot(fit6)

library(e1071)
library(rpart)

x <- train[ ,c(-13,-14,-15,-16)]

tree_fit <- rpart(Item_Outlet_Sales ~ ., data = x)

tree <- rpart(Item_Outlet_Sales ~ ., data = x,control = rpart.control(cp = 0.0001))
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree, cp = bestcp)
v <- predict(tree.pruned, x)
rmse(train$Item_Outlet_Sales,v)

library(randomForest)
library(caret)

new_train <- train[ ,1:16]
new_test <- test[ ,1:16]


folds <- createFolds(y=new_train$Item_Outlet_Sales,k = 15, list = T, returnTrain = T)
final <- NA

for(i in 1:15){
  training <- new_train[folds[[i]], ]
  testing <- new_train[-folds[[i]], ]
  rf_fit <- randomForest(Item_Outlet_Sales~.,data=training, method="parRF")
  prediction <- predict(rf_fit, testing)
  final[!row.names(new_train) %in% folds[[i]]] <- prediction
  final
}
varImpPlot(rf_fit)
rmse(train$Item_Outlet_Sales,final)

final_predict <- predict(rf_fit, new_test)
submission <- data.frame(Item_Identifier = test$Item_Identifier, Outlet_Identifier = test$Outlet_Identifier,Item_Outlet_Sales =final_predict)
write.csv(submission, file = "final_submission.csv")

