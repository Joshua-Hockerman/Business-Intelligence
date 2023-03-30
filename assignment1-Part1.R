demographics <- list(
  name=c("Alex","Bob","Claire","Denise"), 
  female=c(FALSE,FALSE,TRUE,TRUE),
  age=c(20,25,30,35))

demographics
demographics$name[2]

df <- data.frame(
  name=c("Alex","Bob","Claire","Denise"),
  Female=c(FALSE,FALSE,TRUE,TRUE),
  age=c(20,25,30,35),
  row.names = c("row_1","row_2","row_3","row_4"))

df
mean(df$age)
df$age[which(df$name == "Claire")]
