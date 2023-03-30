course <- c("MIS1234","MIS2345","MIS3456")
numOfStudents <- c(20,30,40)
dataAnalyticsMinor <- c(T,T,F)

dfTest <- data.frame(
  course, 
  numOfStudents, 
  dataAnalyticsMinor, 
  stringsAsFactors = FALSE)

dfTest

ncol(dfTest)
nrow(dfTest)
colnames(dfTest)
rownames(dfTest)<-c("row1","row2","row3")
dfTest

dfTest$course
dfTest[2,]
dfTest[2,2]
dfTest[c(1,3),]
dfTest[c(1,2),c(2,3)]
dfTest[dataAnalyticsMinor==TRUE,]
