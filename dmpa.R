library(dplyr)
library(corrplot)
library(Metrics)

library(car)
library(caret)
library(ggplot2)


complete<-read.csv("pca.csv")
dim(complete)
names(complete)

head(complete)
summary(complete)
miss_val <- sapply(complete, function(x) sum(is.na(x)))
sapply(complete, class)

complete$to.work <- gsub("Less than 30 minutes","1",complete[[3]])
complete$to.work <- gsub("Between 30-45 minutes","2",complete[[3]])
complete$to.work <- gsub("Between 45-60 minutes","3",complete[[3]])
complete$to.work <- gsub("Between 60-75 minutes","4",complete[[3]])
complete$to.work <- gsub("Between 75-90 minutes","5",complete[[3]])
complete$to.work <- gsub("More than 90 minutes","6",complete[[3]])
complete$to.work <- as.numeric(complete$to.work)

summary(complete)
sum_towork <- sum(complete[[3]][!is.na(complete[[3]])])
mean_sum_towork <- sum_towork/(nrow(complete)-miss_val[3])
mean_sum_towork<-as.integer(mean_sum_towork)
complete$to.work[is.na(complete$to.work)] <- mean_sum_towork

complete$from.work <- gsub("Less than 30 minutes","1",complete[[4]])
complete$from.work <- gsub("Between 30-45 minutes","2",complete[[4]])
complete$from.work <- gsub("Between 45-60 minutes","3",complete[[4]])
complete$from.work <- gsub("Between 60-75 minutes","4",complete[[4]])
complete$from.work <- gsub("Between 75-90 minutes","5",complete[[4]])
complete$from.work <- gsub("More than 90 minutes","6",complete[[4]])
complete$from.work <- as.numeric(complete$from.work)

sum_fromwork <- sum(complete[[3]][!is.na(complete[[3]])])
mean_sum_fromwork <- sum_fromwork/(nrow(complete)-miss_val[3])
mean_sum_fromwork<-as.integer(mean_sum_fromwork)
complete$from.work[is.na(complete$from.work)] <- mean_sum_fromwork

x <- sample(levels(complete[[5]]),sum(is.na(complete[[5]])), replace = TRUE)
complete[[5]][is.na(complete[[5]])]<- x

g <- ggplot(complete, aes(x=mode_of_transport, y=monthly_commute_cost))
g <- g + geom_boxplot(aes(group=complete$mode_of_transport))
g <- g + theme(axis.text.x = element_text(angle = 70,vjust = 0.5, color = "red"))
g <- g + ggtitle("Transport vs Cost")
g

plot(complete$monthly_commute_cost,complete$mode_of_transport)


complete$monthly_commute_cost<-factor(complete$monthly_commute_cost, labels = c(500,1500,2500,3500,4500,5500))

write.csv(complete, file = "pca2.csv")
summary(complete)
complete$mode_of_transport<-factor(complete$mode_of_transport, labels = c(1,2,3,4,5,6,7,8,9,10,11,12))

//
for (i in levels(complete$mode_of_transport))
{
  sum<-0
  avg<-0
  count<-0
  for (j in 1:298)
    {if(complete[j,5]==i)
      {
      sum[i]<-sum[i] + complete[j,6]
      count<-count+1
      }
    else {sum[i]<-sum[i]+0}
    }
  avg[i]<-sum[i]/count
  print(i,avg[i])
}
//

