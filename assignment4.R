# Joshua Hockerman
# BI Assignment 4
# Use the arules library
library(arules)

groceries <- read.transactions("groceries.csv", sep = ",")

# Q1: Use the same dataset groceries, create a sparse matrix, display the last five
# transactions.
inspect(groceries[1:5])

# Q2: Visualize the sparse matrix (with sample of 150 transactions). Which item (ID) 
# is the most frequently purchased item according to the plot?
image(sample(groceries, 150))
items <- itemFrequency(groceries[,],type="absolute")
head(sort(items, decreasing=TRUE), n=10)
# According to the plot and visualization of the data, "whole milk" is the most
# purchased item in the list, followed by "other vegetables".

# Q3: Show the support of the last five items in the grocery data.
support(groceries[1:5],groceries)

# Q4: Visualize Support values of the products with support threshold of 0.2 and 0.3. 
# What is the key difference between two plots?
itemFrequencyPlot(groceries, support=0.2)
itemFrequencyPlot(groceries, support=0.3)
# The key difference is that Whole Milk is the only item with a support over
# 0.2, and there are no items over a support of 0.3.

# Q5: Train an association rule model, experiment with different values of thresholds 
# of support, confidence that is different from the lab example, and select your 
# own thresholds (as long as you can result in a manageable amount of rules
# [larger than 0, smaller than 400]).
groceryrules1 <- apriori(groceries, parameter = list(support = 0.007, 
                                                    confidence = 0.3, 
                                                    minlen = 2))

groceryrules2 <- apriori(groceries, parameter = list(support = 0.006, 
                                                     confidence = 0.35, 
                                                     minlen = 2))

groceryrules3 <- apriori(groceries, parameter = list(support = 0.005, 
                                                     confidence = 0.4, 
                                                     minlen = 2))

groceryrules4 <- apriori(groceries, parameter = list(support = 0.004, 
                                                     confidence = 0.45, 
                                                     minlen = 2))

# Q6: Sort your rules by confidence first. If you are an expert Market Basket 
# analyst. What promotion strategy or advice you can give based on the rules 
# you generated? Explain why?
inspect(sort(groceryrules1, by = "confidence")[1:5])
inspect(sort(groceryrules2, by = "confidence")[1:5])
inspect(sort(groceryrules3, by = "confidence")[1:5])
inspect(sort(groceryrules4, by = "confidence")[1:5])
# Whole milk is the closest associated with the top five groups made by each ruleset.
# So I would recommend that the grocery develop marketing and pricing strategies
# around whole milk and the items that are most closely associated with it.
# This can include discounts for buying the other products with the purchase of milk,
# "meal deal" type discounts for a whole selection of products associated with milk,
# or placing products associated with milk together to increase purchase chance.

# Q7: Sort your rules by lift. What promotion strategy or advice you can give 
# based on the rules you generated? Explain why?
inspect(sort(groceryrules1, by = "lift")[1:5])
inspect(sort(groceryrules2, by = "lift")[1:5])
inspect(sort(groceryrules3, by = "lift")[1:5])
inspect(sort(groceryrules4, by = "lift")[1:5])
# Root vegetables are closely associated with several groups such as herbs, 
# whole milk, tropical fruit, beef broth, and whipped/sour cream. I would recommend
# grouping these products strategically throughout the store to increase purchases
# of these and associated products by customers.
