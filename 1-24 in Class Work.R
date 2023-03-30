x <- c(2, 3, 5, 7, 11)
x
is.vector(x)
length(x)

n <- seq(from=1, to=50)
n

m <- seq(10,100,10)
m

names(x) <- c("Value1", "Value2", "Value3", "Value4", "Value5")
x

#The most efficient way to create the following vectors
vone <- seq(10,20)
vone

vtwo <- seq(-30,30,5)
vtwo

vthree <- c(apple=4,banana=5,cherry=3)
vthree

vec1 <- 1:4
vec1
vec2 <- c(1,0,0,0)
vec2-vec1

lone <- list(name="Bob", grades= c(100,90,85,95))
lone
lone[2]
lone[[2]]
class(lone[2])
class(lone[[2]])

lone$name
lone$grades

#Example test
student <- list(name="Anna", is_female=TRUE, age=22, enrollment=c("MIS4710","MIS4720","MIS4730"), grade_point=c(4,3,4))
student
gpa <- mean(student$grade_point)
gpa
