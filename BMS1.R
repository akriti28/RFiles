library(ggplot2)
#library(ggbiplot)
library(gridExtra)

# corrplot is needed for correlation plots
library(corrplot)

# we'll use plyr and dplyr to manipulate the data
library(plyr)
library(dplyr)

# we'll use caret to dummify factors
# and to order the predictors by their 
# importance using Random Feature Elimination (RFE)
library(caret)

# imputaion of missing values
library(mice)
library(VIM)

# parallel computing
library(doParallel)

train <- read.csv("Train.csv")
test <- read.csv("Test.csv")

#check dimesions ( number of row & columns) in data sets
dim(train)
dim(test)

#check the variables and their types in train
str(train)

# brief summary of train
summary(train)
test$Item_Outlet_Sales <-  0
combi <- rbind(train, test)

levels(combi$Item_Fat_Content)

# "LF", "low fat" and "Low Fat" are certainly the same
# as are "reg" and "regular"
# let's replace them accordingly
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,
                                  c("LF" = "Low Fat", "low fat" = "Low Fat", "reg" = "Regular"))


# count fat levels for each Item type
fat <- as.data.frame( setNames(
  aggregate(
    combi$Item_Fat_Content, 
    by=list(Category=combi$Item_Type,
            Category=combi$Item_Fat_Content), 
    FUN= length),
  c("Item_Type", "Item_Fat_Content", "number")
))
fat

levels(combi$Item_Fat_Content) <- c(levels(combi$Item_Fat_Content), "None")

combi[ which(combi$Item_Type == "Health and Hygiene") ,]$Item_Fat_Content <- "None"
combi[ which(combi$Item_Type == "Household") ,]$Item_Fat_Content <- "None"
combi[ which(combi$Item_Type == "Others") ,]$Item_Fat_Content <- "None"

combi$Item_Fat_Content <- factor(combi$Item_Fat_Content)

str(combi)

# count fat levels for each Item type
fat <- as.data.frame( setNames(
  aggregate(
    combi$Item_Fat_Content, 
    by=list(Category=combi$Item_Type,
            Category=combi$Item_Fat_Content), 
    FUN= length),
  c("Item_Type", "Item_Fat_Content", "number")
))

fat

levels(combi$Outlet_Size)[1] <- "Other"

#any missing values?
table(is.na(combi))

#where exactly are those values missing?
colSums(is.na(combi))

# plenty of weight values are missing
# what can we learn about the weights?

# boxplot of weights vs Item type
ggplot(combi, aes(Item_Type, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Item Type")

ggplot(combi, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Outlet identifier")

weightsByItem <- as.data.frame( ddply(na.omit(combi), 
                                     ~Item_Identifier, 
                                     summarise, 
                                     mean=mean(Item_Weight), 
                                     sd=sd(Item_Weight)))

# we can now use these values to fill in the missing weight values:
combi$Item_Weight <- ifelse(is.na(combi$Item_Weight), 
                            weightsByItem$mean[
                              match(combi$Item_Identifier, weightsByItem$Item_Identifier)], combi$Item_Weight)

#any values still missing?
table(is.na(combi))

ggplot(combi, aes(Item_Type, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Item Type")

ggplot(combi, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Outlet identifier")

combi$Year <- as.factor(2013 - combi$Outlet_Establishment_Year)

# Now, we can drop Outlet_Establishment_Year, as we've replaced it
# by the new column Year
combi <- select(combi, -c(Outlet_Establishment_Year))

# let's have a look at the Item_MRP
ggplot(combi, aes(x=Item_MRP)) + 
  geom_density(color = "blue", adjust=1/5) +
  geom_vline(xintercept = 69, color="red")+
  geom_vline(xintercept = 136, color="red")+
  geom_vline(xintercept = 203, color="red") + 
  ggtitle("Density of Item MRP")

combi$MRP_Level <- as.factor(
  ifelse(combi$Item_MRP < 69, "Low",
         ifelse(combi$Item_MRP < 136, "Medium",
                ifelse(combi$Item_MRP < 203, "High", "Very_High")))
)

combi <- select( combi, c(Item_Identifier,
                          Item_Weight,
                          Item_Fat_Content,
                          Item_Visibility,
                          Item_Type,
                          Item_MRP,
                          Outlet_Identifier,
                          Outlet_Size,
                          Outlet_Location_Type,
                          Outlet_Type,
                          Year,
                          MRP_Level,
                          Item_Outlet_Sales
))

str(combi)

aggregate(combi$Outlet_Identifier, by=list(Category=combi$Outlet_Identifier), FUN=length)

ggplot(combi[1:nrow(train),], aes(Outlet_Identifier, Item_Outlet_Sales)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet identifier") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet identifier")

# boxplot of  Sales vs. Outlet Type
ggplot(combi[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet Type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet Type")

# Sales in the one type 2 supermarket appear a bit low,
# as one would expect them to be higher than in
# the type 1 supermarkets.
# Maybe it's because it's still fairly new, having
# been founded 4 years ago.

# boxplot of  Sales vs. Outlet Type
ggplot(combi[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet type")

otherShops <- as.data.frame( setNames(
  aggregate(
    combi$Outlet_Size, 
    by=list(Category=combi$Outlet_Identifier, 
            Category=combi$Outlet_Type,
            Category=combi$Outlet_Location_Type,
            Category=combi$Outlet_Size), 
    FUN= length),
  c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "number")
))
otherShops

combi[ which(combi$Outlet_Identifier == "OUT010") ,]$Outlet_Size <- "Small"
# "OUT017" and "OUT045" could be small or medium
combi[ which(combi$Outlet_Identifier == "OUT017") ,]$Outlet_Size <- "Small"
combi[ which(combi$Outlet_Identifier == "OUT045") ,]$Outlet_Size <- "Small"

# count the number of others per Outlet_Identifier and Outlet_Type
otherShops <- as.data.frame( setNames(
  aggregate(
    combi$Outlet_Size, 
    by=list(Category=combi$Outlet_Identifier, 
            Category=combi$Outlet_Type,
            Category=combi$Outlet_Location_Type,
            Category=combi$Outlet_Size), 
    FUN= length),
  c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "number")
))
otherShops

combi$Outlet_Size <- factor(combi$Outlet_Size)

str(combi)

# boxplot of  Sales vs. Outlet location
ggplot(combi[1:nrow(train),], aes(x = Outlet_Location_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet location") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet location")

# boxplot of  Sales vs. Outlet type
ggplot(combi[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet type")

# boxplot of  Sales vs. Item type
ggplot(combi[1:nrow(train),], aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type")

# boxplot of  Sales vs. Item type
ggplot(combi[1:nrow(train),], aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Type)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type")

ggplot(combi, aes(Item_Type, Item_Visibility, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Item Type")

# boxplot of Visibility vs. Outlet Identifier
ggplot(combi, aes(Outlet_Identifier, Item_Visibility)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Outlet identifier")

# We use the MICE package to impute those missing values

# to compare the visibility distributions before
# and after imputing the missing values
# we create a copy of the non-vanishing entries
combiNonZeroVis <- subset(combi, Item_Visibility > 0)

# replace 0 by NA so that mice can work its magic
outletIdentifiers <- levels(combi$Outlet_Identifier)
itemTypes <- levels(combi$Item_Type)
for (outName in outletIdentifiers) {
  for (itemName in itemTypes) {
    combi[ which(combi$Outlet_Identifier == outName &
                   combi$Item_Type == itemName),]$Item_Visibility <-
      ifelse(
        combi[ which(combi$Outlet_Identifier == outName &
                       combi$Item_Type == itemName), ]$Item_Visibility == 0 ,
        NA ,
        combi[ which(combi$Outlet_Identifier == outName &
                       combi$Item_Type == itemName),]$Item_Visibility
      )
  }
}

#any missing values now?
table(is.na(combi))
colSums(is.na(combi))

#pattern of missing values
md.pattern(combi)

newCombi <- mice(combi,m=1,maxit=1,meth='pmm',seed=0)
# summary of imputations
summary(newCombi)

# the imputed values
#newCombi$imp$Item_Visibility

# comparison of the distribution of existing
# and imputed visibilities
densityplot(newCombi)
stripplot(newCombi, pch = 20, cex = 1.2)

# let's replace the NA (formerly zero) values
# by the imputed ones
combi <- complete(newCombi,1)

# total visibility per shop
# should be 100
shopSum <- as.data.frame(setNames(
  aggregate(combi$Item_Visibility, by=list(Category=combi$Outlet_Identifier), FUN=sum),
  c("Outlet_Identifier", "TotVis")))

shopSum

# let's normalize all visibilities such that
# the total per shop comes out at 100

for (outName in outletIdentifiers) {
  combi[ which(combi$Outlet_Identifier == outName),]$Item_Visibility <-
    combi[ which(combi$Outlet_Identifier == outName),]$Item_Visibility *
    100/shopSum[ which(shopSum$Outlet_Identifier == outName),]$TotVis
}

shopSum <- as.data.frame(setNames(
  aggregate(combi$Item_Visibility, by=list(Category=combi$Outlet_Identifier), FUN=sum),
  c("Outlet_Identifier", "TotVis")))

shopSum

# densities of visibilities before and
# after imputation
ggplot() + 
  geom_density(aes(x=Item_Visibility), colour="red", data=combiNonZeroVis) + 
  geom_density(aes(x=Item_Visibility), colour="blue", data=combi)

# histograms of visibilities before and
# after imputation
ggplot(combiNonZeroVis[combiNonZeroVis$Outlet_Type %in% "Grocery Store", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) + 
  xlim(0.0,0.35) +
  xlab("Item visibility") + 
  ggtitle("Grocery Stores")
ggplot(combi[combi$Outlet_Type %in% "Grocery Store", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.35) +
  xlab("Item visibility") + 
  ggtitle("Grocery Stores")

ggplot(combiNonZeroVis[combiNonZeroVis$Outlet_Type %in% "Supermarket Type1", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.3) +
  xlab("Item visibility") + 
  ggtitle("Type 1")
ggplot(combi[combi$Outlet_Type %in% "Supermarket Type1", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.3) +
  xlab("Item visibility") + 
  ggtitle("Type 1")

ggplot(combiNonZeroVis[combiNonZeroVis$Outlet_Type %in% "Supermarket Type2", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.25) +
  xlab("Item visibility") + 
  ggtitle("Type 2")
ggplot(combi[combi$Outlet_Type %in% "Supermarket Type2", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.25) +
  xlab("Item visibility") + 
  ggtitle("Type 2")

ggplot(combiNonZeroVis[combiNonZeroVis$Outlet_Type %in% "Supermarket Type3", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.25) +
  xlab("Item visibility") + 
  ggtitle("Type 3")
ggplot(combi[combi$Outlet_Type %in% "Supermarket Type3", ], aes(Item_Visibility)) +
  geom_histogram(colour = "blue", fill = "blue", bins = 20) +
  theme(axis.text.x = element_text(vjust = 0.5, color = "black")) +  
  xlim(0.0,0.25) +
  xlab("Item visibility") + 
  ggtitle("Type 3")


# boxplot of Visibility vs. Outlet Identifier
ggplot(combiNonZeroVis, aes(Outlet_Identifier, Item_Visibility)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Outlet identifier")
ggplot(combi, aes(Outlet_Identifier, Item_Visibility)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Visibility") + 
  ggtitle("Item visibility vs Outlet identifier")

str(combi)

###############################################
#
# let's have a look at the item identifiers now,
# there are way too many of them.
#
# keeping only the first two letters gives us three groups:
# food, drink and non-food
#
###############################################

combi$Item_Class <- strtrim(combi$Item_Identifier, 2)
combi$Item_Class <- factor(combi$Item_Class)

levels(combi$Item_Class)

# keeping the first three letters of the Item identifier
# gives a somewhat higher granularity
combi$Item_Identifier <- strtrim(combi$Item_Identifier, 3)
combi$Item_Identifier <- factor(combi$Item_Identifier)


# let's have a look at the numerical variables now

# correlation between numerical variables
corMatrix <- cor(combi[1:nrow(train),][sapply(combi[1:nrow(train),], is.numeric)])
corMatrix

# a brief overview of the correlation matrix
corrplot::corrplot(corMatrix, method="number", type="upper")
corrplot::corrplot(corMatrix, method="number", type="upper", order="hclust")

#
# Item_Outlet_Sales has a strong positive correlation with Item_MRP
# and a somewhat weaker negative one with Item_Visibility
# Time for a quick principal component analysis
#

subData <- as.data.frame(cbind(
  combi[1:nrow(train),]$Item_Visibility, 
  combi[1:nrow(train),]$Item_MRP, 
  combi[1:nrow(train),]$Item_Outlet_Sales))

names(subData) <- c("Item_Visibility",
                    "Item_MRP",
                    "Item_Outlet_Sales")

sub.groupby <- combi[1:nrow(train),]$Outlet_Type

str(subData)

subData.pca <- prcomp(subData,
                      center = TRUE,
                      scale. = TRUE) 

summary(subData.pca)

theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle,aes(x,y)) + geom_path()

loadings <- data.frame(subData.pca$rotation, 
                       .names = row.names(subData.pca$rotation))
p + geom_text(data=loadings, 
              mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")


ggplot(combi[1:nrow(train),], aes(Item_Visibility, Item_Outlet_Sales)) +
  geom_point(size = 2.5, aes(colour = factor(Outlet_Type))) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Visibility") + 
  ylab("Item Outlet Sales") +
  ggtitle("Item Sales vs Item Visibility")

# Scatter plot of Item_Outlet_Sales vs Item_Visibility
# coloured according to the Outlet size
ggplot(combi[1:nrow(train),], aes(Item_Visibility, Item_Outlet_Sales)) +
  geom_point(size = 2.5, aes(colour = factor(Outlet_Size))) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Visibility") + 
  ylab("Item Outlet Sales") +
  ggtitle("Item Sales vs Item Visibility")

# Scatter plot of Item_Outlet_Sales vs Item_Visibility
# coloured according to the Outlet identifier
ggplot(combi[1:nrow(train),], aes(Item_Visibility, Item_Outlet_Sales)) +
  geom_point(size = 2.5, aes(colour = factor(Outlet_Identifier))) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Visibility") + 
  ylab("Item Outlet Sales") +
  ggtitle("Item Sales vs Item Visibility")

# boxplot of  Sales vs. Item type
ggplot(combi[1:nrow(train),], aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Type)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type")

max(combi$Item_MRP)
min(combi$Item_MRP)

# plenty of outliers here
# can we reduce them by dividing Sales by the MRP?

# boxplot of  Sales vs. Item type
ggplot(combi[1:nrow(train),], aes(x = Item_Type, y = Item_Outlet_Sales/Item_MRP, fill = Outlet_Type)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type")


# bar graph of Item_Outlet_Sales vs Item_Type
ggplot(train, aes(Item_Type, Item_Outlet_Sales/Item_MRP, fill = Outlet_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Outlet Sales") +
  ggtitle("Item Sales vs Item type")

# dividing sales by MRP does reduce the number of outliers
# and also emphasizes the differences between the different
# types of shop, so we'll just do that in the following

combi$Item_Outlet_Sales <- combi$Item_Outlet_Sales/combi$Item_MRP



# Proportion of Supermarkets vs. Grocery stores in the data
prop.table(table(combi$Outlet_Type))

analyze.shop <- function(shopID = character ) {
  cat("RESULTS FOR SHOP ", shopID, "\n", "\n", 
      file = "ContingencyTables.txt", append = TRUE)
  shopData <- as.data.frame(combi[1:nrow(train),][combi[1:nrow(train),]$Outlet_Identifier %in% shopID,])
  
  shopData$Outlet_Identifier <- factor(shopData$Outlet_Identifier)
  shopData$Outlet_Size <- factor(shopData$Outlet_Size)
  shopData$Outlet_Location_Type <- factor(shopData$Outlet_Location_Type)
  shopData$Outlet_Type <- factor(shopData$Outlet_Type)
  
  cat("Variance of outlet establishment year: ", var(shopData$Year), "\n", 
      file = "ContingencyTables.txt", append = TRUE)
  # since the variance of Outlet_Establishment_Year is zero, we
  # can also remove that column
  shopData <- select(shopData, -c(Outlet_Identifier,Outlet_Size, 
                                  Outlet_Location_Type, 
                                  Outlet_Type, 
                                  Year))
  
  # histograms of weight, visibility, MRP, and sales
  p1 <- ggplot(shopData, aes(Item_Weight)) +
    geom_histogram(colour = "blue", fill = "blue", bins = 20) +
    theme(axis.text.x = element_text(vjust = 0.5, color = "black")) + 
    xlab("Item weights")
  p2 <- ggplot(shopData, aes(Item_Visibility)) +
    geom_histogram(colour = "blue", fill = "blue", bins = 20) +
    theme(axis.text.x = element_text(vjust = 0.5, color = "black")) + 
    xlab("Item visibility")
  p3 <- ggplot(shopData, aes(Item_MRP)) +
    geom_histogram(colour = "blue", fill = "blue", bins = 20) +
    theme(axis.text.x = element_text(vjust = 0.5, color = "black")) + 
    xlab("Item MRP")
  p4 <- ggplot(shopData, aes(Item_Outlet_Sales)) +
    geom_histogram(colour = "blue", fill = "blue", bins = 20) +
    theme(axis.text.x = element_text(vjust = 0.5, color = "black")) + 
    xlab("Item outlet sales")
  
  grid.arrange(p1, p2, p3, p4, ncol=2, top = shopID)
  
  pairs(~Item_Weight+Item_Visibility+Item_MRP+Item_Outlet_Sales,data=shopData,
        main=shopID)
  
  # boxplot of  Sales vs. Item type
  p6 <- ggplot(shopData, aes(x = Item_Type, y = Item_Outlet_Sales)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
    xlab("Item type") + 
    ylab("Sales") + 
    ggtitle("Sales vs Item type")
  
  grid.arrange(p6, ncol=1, top = shopID)
  
  cat("Contingency tables for shop ", shopID, "\n", file = "ContingencyTables.txt", append = TRUE)
  capture.output(
    table(shopData$Item_Identifier, shopData$Item_Fat_Content),
    file = "ContingencyTables.txt", append = TRUE)
  capture.output(
    table(shopData$Item_Type, shopData$Item_Identifier),
    file = "ContingencyTables.txt", append = TRUE)
  capture.output(
    table(shopData$Item_Type, shopData$Item_Fat_Content),
    file = "ContingencyTables.txt", append = TRUE)
  cat("\n", "\n", file = "ContingencyTables.txt", append = TRUE)
}

cat("Brief analyses by shop", "\n", file = "ContingencyTables.txt")

for (i in levels(combi$Outlet_Identifier)) {
  analyze.shop(i)
}

# let's resurrect the train and test data sets
new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]

# and drop the faked Item_Outlet_Sales column in new_test
new_test <- dplyr::select(new_test, -c(Item_Outlet_Sales))

str(new_train)
str(new_test)

# let's save them, so that we don't have to redo the cleaning
# over and over again
write.csv(new_train, file="new_train.csv", row.names=FALSE, quote = FALSE)
write.csv(new_test, file="new_test.csv", row.names=FALSE, quote = FALSE)

# check variable importance with 
# random feature elimination (RFE)
# from caret

# scale Sales to be in interval [0,1]
maxSales <- max(new_train$Item_Outlet_Sales)
new_train$Item_Outlet_Sales <- new_train$Item_Outlet_Sales/maxSales

set.seed(0)

# one-hot encoding of the factor variables
# leave out the intercept column

new_train <- as.data.frame(model.matrix( ~ . + 0, data = new_train))
new_test <- as.data.frame(model.matrix( ~ . + 0, data = new_test))

str(new_train)

# define a vector of Item_Outlet_Sales
# and a dataframe of predictors
sales <- new_train$Item_Outlet_Sales
predictors <- subset(new_train, select=-c(Item_Outlet_Sales))


#########################################
#
# check relative importance of predictors
# with caret rfe
#
#########################################

# do it in parallel
cl <- makeCluster(detectCores()); registerDoParallel(cl)

subsetSizes <- c(1:20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 121)
N <- 5 # number of resamples
seeds <- vector(mode = "list", length = N+1)
for(i in 1:N) seeds[[i]] <- sample.int(1000, length(subsetSizes) + 1)
seeds[[N+1]] <- sample.int(1000, 1)
control <- rfeControl(functions=rfFuncs,
                      method="cv",
                      seeds = seeds,
                      number = N,
                      repeats = 3,
                      verbose=TRUE,
                      allowParallel=TRUE
)
# Start the clock!
ptm <- proc.time()
# run the RFE algorithm
results2 <- rfe(x = predictors,
                y = sales,
                sizes = subsetSizes,
                preProc=c("center", "scale"),
                rfeControl=control)
# Stop the clock
proc.time() - ptm

# stop the parallel processing and register sequential front-end
stopCluster(cl); registerDoSEQ();

# summarize the results
print(results2)
# list all features in descending order of importance
listOfPreds <- pickVars(results2$variables, 120)
listOfPreds
# plot the results
plot(results2, type=c("g", "o") )

# build a data frame containing the predictors 
# ordered by their importance

ordered.preds <- predictors[,listOfPreds[1]]
for (i in 2:length(listOfPreds)) {
  ordered.preds <- cbind(ordered.preds, predictors[,listOfPreds[i]])
}
colnames(ordered.preds) <- listOfPreds
ordered.preds <- as.data.frame(ordered.preds)

ordered.test <- new_test[,listOfPreds[1]]
for (i in 2:length(listOfPreds)) {
  ordered.test <- cbind(ordered.test, new_test[,listOfPreds[i]])
}
colnames(ordered.test) <- listOfPreds
ordered.test <- as.data.frame(ordered.test)

#remove the scaling to [0,1] in sales

sales <- sales*maxSales
write.csv(sales, file="sales.csv", row.names=FALSE, quote = FALSE)

