library(readr)
age_income_data <- read_csv("age_income_data.csv")

market <- age_income_data

#Summraize the data
str(market)
summary(market)

par(mfrow= c(1,2))
boxplot(market$age ~ market$bin, main="Explore age")
boxplot(market$income ~ market$bin, main="Explore Income")

cor.test(market$age, market$income)

#K-means
three <- kmeans(market[ ,2:3],3)

#visualize
plot(market$age, market$income, col=three$cluster)

#Now scale the data in order to properly cluster it
market$age_scale <- as.numeric(scale(market$age))
market$inc_scale <- as.numeric(scale(market$income))

#Re-do the K-means
three <- kmeans(market[ ,4:5],3)
plot(market$age, market$income, col=scaled_three$cluster)

par(mfrow= c(1,1))

#H-cluster
hc_model <- hclust(dist(market[,4:5]),method="ward.D2")

dend <- as.dendrogram(hc_model)
library("dendextend")
dend_six_color <- color_branches(dend, k = 6)

plot(dend_six_color,leaflab = "none", horiz=TRUE,main="Age and Income Hierarchy")
abline(v=37.5,lty='dashed',col='blue')

#Evaluate the Model
two <- kmeans(market[ ,4:5],2)
four <- kmeans(market[ ,4:5],4)
five <- kmeans(market[ ,4:5],5)
six <- kmeans(market[ ,4:5],6)
seven <- kmeans(market[ ,4:5],7)
eight <- kmeans(market[ ,4:5],8)
nine <- kmeans(market[ ,4:5],9)
ten <- kmeans(market[ ,4:5],10)

optimize <- data.frame(clusters = c(2:10), wss = rep(0, 9)) #Create a new dataframe to store different wss(withinness) from the above models
optimize[1, 2] <- as.numeric(two$tot.withinss) #store the total within-cluster sum of square to their respective row
optimize[2, 2] <- as.numeric(three$tot.withinss)
optimize[3, 2] <- as.numeric(four$tot.withinss)
optimize[4, 2] <- as.numeric(five$tot.withinss)
optimize[5, 2] <- as.numeric(six$tot.withinss)
optimize[6, 2] <- as.numeric(seven$tot.withinss)
optimize[7, 2] <- as.numeric(eight$tot.withinss)
optimize[8, 2] <- as.numeric(nine$tot.withinss)
optimize[9, 2] <- as.numeric(ten$tot.withinss)

plot(optimize$wss ~ optimize$clusters, type='b')

#Save the k-means cluster results to original dataset
market$clus5 <- five$cluster
market$clu6 <- six$cluster

#Save the heirarchical results
dend_five <- cutree(dend, k = 5)
market$dend5 <- dend_five

dend_six <- cutree(dend, k = 6)
market$dend6 <- dend_six
