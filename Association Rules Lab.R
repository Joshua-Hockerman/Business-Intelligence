# Use the arules library
library(arules)

groceries <- read.transactions("groceries.csv", sep = ",")

inspect(groceries[1:3])

image(groceries[1:10])

image(sample(groceries,100))

#Support
itemFrequency(groceries[,1:3])

itemFrequencyPlot(groceries, support = 0.1)

itemFrequencyPlot(groceries, topN = 15)

#Train association rule model
groceryrules <- apriori(groceries,list(support=0.006,
                                       confidence=0.25,
                                       minlen=2))
summary(groceryrules)

inspect(groceryrules[1:10])

inspect(sort(groceryrules, by = "lift")[1:5])

#Subsetting rules
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

#Save association rules
write(groceryrules,"groceryrules.csv",sep=",",quote=T,row.names=F)
