house_prices<-read.csv("C:\\Users\\joshh\\OneDrive\\Documents\\R Programs\\Business Intelligence\\HousePrices.csv")

df<-data.frame(house_prices)

hist(df$price, col="green", 
     xlab="Price of the House", 
     ylab="Count",
     main="Distribution of House Price")

hist(df$living_area, col="red",
     xlab="Living Area (square feet)",
     ylab="Frequency",
     main="Distribution of Living Area of Houses")

scatter.smooth(df$price, df$living_area,
               xlab="House Price",
               ylab="Living Area (square feet)",
               main="House Price by Living Area")

hist(df$lot_size, col="cyan",
         xlab="Lot Size (acres)",
         ylab="Frequency",
         main="Distribution of Lot Sizes")

ac_distribution <- table(df$air_cond)
barplot(ac_distribution)

fuel_distribution <- table(df$fuel)
fuel_distribution
pie(fuel_distribution)

construction_distribution <- table(df$construction)
pie(construction_distribution)

boxplot(price ~ rooms, data=df, main="House Prices ($)")

plot(price ~ living_area, data=df,
     pch=ifelse(df$air_cond=="No",0,1),
     col=ifelse(df$air_cond=="No","red","blue"))

plot(price ~ living_area, data=df,
     pch=ifelse(df$fireplaces==1,0,1),
     col=ifelse(df$fireplaces==1,"red","black"))
