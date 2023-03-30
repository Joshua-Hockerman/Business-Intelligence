##-----------------------[BASIC DATA VISUALIZATION]------------------------------------

#Load data (assume that you have the dataset in your working directory)
df<-read.table(file="HousePrices.csv", 
               sep=",", header=TRUE, stringsAsFactors=FALSE)

##-----------------------Create Histogram for Numeric Data ------------------------------------
# adjust the size of each bin via breaks
hist(df$price, breaks=20) 

#More customized histogram. To learn more about these additional arguments, just type ?hist
hist(df$price, col="green", xlim=c(50000, 800000),
     xlab="Price of house", ylab="Count", 
     main="Distribution of house price")

##-----------------------Create Bar chart for Categorical Data ------------------------------------
#Get the frequency of the categorical data
ditribution <- table(df$heat)
ditribution
#Create the barplot
barplot(ditribution)

##-----------------------Create a Simple Pie chart for Categorical Data ------------------------------------
ditribution <- table(df$fuel)
pie(ditribution)

##-----------------------Create Simple Box Plot ------------------------------------
class(df) 
boxplot(price~rooms,data=df, main="House Prices of Different Rooms",
        xlab="Number of rooms", ylab="House Prices ($)")


##-----------------------[ADVANCED DATA VISUALIZATION]------------------------------------
#install.packages("ggplot2") #Run this once to install packages, then comment it out. 
#install.packages("mosaicData") #Run this once to install packages, then comment it out. 

#load data, here you can import your own data instead
data(CPS85, package = "mosaicData")

# load library 
library(ggplot2)
library(dplyr)
library(mosaicData)
head(CPS85,5)

#Remove outliers from the data
plotdata <- filter(CPS85, wage < 40) #This is how we filter out the outlier

#----------------Scatter plot with specific colors-------------------------
# make points blue, larger, and semi-transparent
ggplot(data = plotdata,
       mapping = aes(x = exper, y = wage)) +
  geom_point(color = "cornflowerblue",  #Here we specify options of geom_
             alpha = .7,
             size = 3)

# add a line of best fit.
ggplot(data = plotdata,
       mapping = aes(x = exper, y = wage)) +
  geom_point(color = "cornflowerblue",alpha = .7,size = 3) +
  geom_smooth(method = "lm") #Here we added another layer of smooth line of best fit and specified linear regression method

# Grouping: Indicate different groups of dots in scatter plot. Here we indicate sex using color
ggplot(data = plotdata, mapping = aes(x = exper, y = wage, color = sex)) + #Here we specified using sex variable to differentiate colors
  geom_point(alpha = .7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5)


#Specify the scales of x and y axis 
# modify the x and y axes and specify the colors to be used
ggplot(data = plotdata,
       mapping = aes(x = exper, y = wage,color = sex)) +
  geom_point(alpha = .7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +
  scale_x_continuous(breaks = seq(0, 60, 10)) + #Here we adjust the scales of axes
  scale_y_continuous(breaks = seq(0, 30, 5), label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) # Here we use this function to specify different colors of the trend lines

#Specify the scales of x and y axis 
# modify the x and y axes and specify the colors to be used
ggplot(data = plotdata,
       mapping = aes(x = age, y = wage,color = married)) +
  geom_point(alpha = .7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +
  scale_x_continuous(breaks = seq(0, 60, 10)) + #Here we adjust the scales of axes
  scale_y_continuous(breaks = seq(0, 30, 5), label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) 

#Facets: Provide different graphs for each level of a certain variable
ggplot(data = plotdata,
       mapping = aes(x = exper, y = wage,color = sex)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5),label = scales::dollar) +
  scale_color_manual(values = c("indianred3","cornflowerblue")) +
  facet_wrap(~sector) #This is the facet_ function, how we produce a graph for each sector


# add informative labels
ggplot(data = plotdata,mapping = aes(x = exper, y = wage,color = sex)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5),label = scales::dollar) +
  scale_color_manual(values = c("indianred3","cornflowerblue")) +
  facet_wrap(~sector) +
  labs(title = "Relationship between wages and experience",
       subtitle = "Current Population Survey",
       caption = "source: http://mosaic-web.org/",
       x = " Years of Experience",
       y = "Hourly Wage",
       color = "Gender")



#-----------------------Create Bar Charts ------------------------------------
library(ggplot2)
data(Marriage, package = "mosaicData")

ggplot(Marriage, aes(x = race)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "Race", 
       y = "Frequency", 
       title = "Participants by race")

#-----------------------Create Pie Charts ------------------------------------

# First calculate the porportion of each pie slice and save it as plot data
plotdata <- Marriage %>%
  count(race) %>%
  arrange(desc(race)) %>%
  mutate(prop = round(n * 100 / sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5  *prop)

# create a basic ggplot2 pie chart
ggplot(plotdata, 
       aes(x = "",y = prop, fill = race)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void()

#-----------------------Create Tree Maps ------------------------------------
#install.packages("treemapify")
library(treemapify)
data(Marriage, package = "mosaicData")

#First calculate tree map data -  the count of a certain variable
plotdata <- Marriage %>%
  count(officialTitle)

# create a treemap of marriage officials
ggplot(plotdata, aes(fill = officialTitle, area = n)) +
  geom_treemap() + 
  labs(title = "Marriages by officiate")

#Tree maps with labels
ggplot(plotdata, 
       aes(fill = officialTitle, area = n, label = officialTitle)) + 
  geom_treemap() + 
  geom_treemap_text(colour = "white",place = "centre") +
  labs(title = "Marriages by officiate")

#-----------------------Create Correlation Plot ------------------------------------
library(ggplot2)
library(ggcorrplot)

#Load data
data(SaratogaHouses, package="mosaicData")
# select numeric variables
df <- dplyr::select_if(SaratogaHouses, is.numeric)
# calulate the correlations
r <- cor(df, use="complete.obs")
round(r,2)

# Plot the correlation 
ggcorrplot(r) 

#Customize correlation plot
ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)
