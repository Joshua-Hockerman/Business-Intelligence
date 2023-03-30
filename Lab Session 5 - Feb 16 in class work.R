x<-3

if (x==1){
  print(paste("x equals 1"))
} else {
  print(paste("x does not equal 1, but instead equals", x, sep=" "))
}

grade<-"A"

determine_grade <- function(grade){
  if(grade=="A"){
    print(4)
  } else if(grade=="B"){
    print(3)
  } else if(grade=="C"){
    print(2)
  } else if(grade=="D"){
    print(1)
  } else if(grade=="F"){
    print(0)
  } else{
    "Valid grade not given"
  }
}

determine_grade(grade)

grade_list <- c("A", "A", "C", "B", "B")

for (grade in grade_list){
  determine_grade(grade)
}
