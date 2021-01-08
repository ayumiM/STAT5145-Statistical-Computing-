# Assignment 3

#question 1
# (a)

# random simulation 
set.seed(1000)

# set the door 
door <- c(1,2,3)

# count success 
count <- 0

# if player does not chage his choice 
# player does not change his choice so pick car door and player's choice door, then if they match count++
MontyHall <- function(n){
  for (i in 1:n) {
  car <- sample(door, 1)
  choice <- sample(door, 1)
  if(car == choice){
    count <- count + 1
  }
  }
  return(count)
}

# player changes his choice.
# if car door and player's first choice match, host's door can be randomly selected from 3 door subtract car door 
# if car door and player's first choice do not match, host's door cannot be car door nor the first choice door, so used setdiff.
# Then, player's second choice needs not to be the host's door nor first choice. 
MontyHallChange <- function(n){
  for (i in 1:n) {
    car <- sample(door, 1)
    choice <- sample(door, 1)
    if(car == choice){
      host <- sample(setdiff(door, car), 1)
    }else{
      host <- setdiff(door, c(choice, car))
    }
    secondchoice <- setdiff(door, c(host, choice))
    if(secondchoice == car){
      count <- count + 1
    }
  }
  return(count)
}

# the number of trial 
N <- 100000
p <- MontyHall(N)
prob <- p / N
prob

p1<- MontyHallChange(N)
prob1<- p1/N
prob1

# (b)
# the number of trial 
N <- 100000

# random simulation 
set.seed(12220)

# door is not 3 so pass the number of door as an argument 
MontyHallChange2 <- function(n){
  door <- c(1:n)
  # the number of win
  count <- 0
  for (i in 1:N) {
    car <- sample(door, 1)
    choice <- sample(door, 1)
    if(car == choice){
      host <- sample(setdiff(door, car), 1)
    }else{
      host <- sample(setdiff(door, c(choice, car)), 1)
    }
    secondchoice <- sample(setdiff(door, c(host, choice)),1)
    if(secondchoice == car){
      count <- count + 1
    }
  }
  return(count)
}

# try the number of door is 2
Try2 <- MontyHallChange2(2)
#95% interval 
Upper2 <- Try2/N + 1.96*sqrt(((Try2/N)*((N-Try2)/N))/N)
Lower2 <- Try2/N - 1.96*sqrt(((Try2/N)*((N-Try2)/N))/N)
c(Lower2, Upper2)

# the number of door is 10
Try10 <- MontyHallChange2(10)
#95% interval 
Upper10 <- Try10/N + 1.96*sqrt(((Try10/N)*((N-Try10)/N))/N)
Lower10 <- Try10/N - 1.96*sqrt(((Try10/N)*((N-Try10)/N))/N)
c(Lower10, Upper10)

# the number of door is 100
Try100 <- MontyHallChange2(100)
#95% interval 
Upper100 <- Try100/N + 1.96*sqrt(((Try100/N)*((N-Try100)/N))/N)
Lower100 <- Try100/N - 1.96*sqrt(((Try100/N)*((N-Try100)/N))/N)
c(Lower100, Upper100)

# the number of door is 1000
Try1000 <- MontyHallChange2(1000)
#95% interval 
Upper1000 <- Try1000/N + 1.96*sqrt(((Try1000/N)*((N-Try1000)/N))/N)
Lower1000 <- Try1000/N - 1.96*sqrt(((Try1000/N)*((N-Try1000)/N))/N)
c(Lower1000, Upper1000)

# (c)
# the number of trial 
N <- 1000

# random simulation 
set.seed(12220)

# door is not 3 so pass the number of door as an argument 
MontyHallChange3 <- function(n){
  door <- c(1:n)
  # the number of win
  count <- 0
  for (i in 1:N) {
    car <- sample(door, 1)
    choice <- sample(door, 1)
    if(car == choice){
      host <- sample(setdiff(door, car), 1)
    }else{
      host <- sample(setdiff(door, c(choice, car)), 1)
    }
    secondchoice <- sample(setdiff(door, c(host, choice)),1)
    if(secondchoice == car){
      count <- count + 1
    }
  }
  return(count)
}

# from the lecture notes 
gamma.density <- function(x, alpha, beta) {
  x^(alpha-1)*exp(-x/beta)/gamma(alpha)/beta^alpha
  }#Evaluate the gamma density at 1:5 for alpha=beta=1, alpha=beta=2, alpha=bet

mapply(gamma.density, alpha=1:3, beta=1:3, MoreArgs=list(x=1:5))
#Evaluate the gamma density at 1:5 for all combinations of alpha and beta
parm <- expand.grid(alpha=1:3, beta=1:3)
out <- mapply(gamma.density, parm[,1], parm[,2], MoreArgs=list(x=1:5))
colnames(out) = paste("alpha", parm[,1], "beta", parm[,2], sep=".")
out

# question 2
library(graphics)
library(tidyverse)
view(ChickWeight)

# (a)
plot(ChickWeight$Time, log(ChickWeight$weight))
# cannot tell which chick's or which diet they got. 

# (b)
plot(ChickWeight$Time, log(ChickWeight$weight), col =ChickWeight$Diet)

# (c)
range(log(ChickWeight$weight))
range(ChickWeight$Time)

# (d)
plot.new()
with(ChickWeight, {
  plot(1, type = 'n', xlab = "Weight", ylab = "Age", xlim = range(Time), ylim = range(log(weight)))
})

# (e)
lines(ChickWeight$Time, log(ChickWeight$weight),  col = ChickWeight$Diet)

# (f)
library(lattice)
plot.new()
with(ChickWeight, {
  plot(1, type = 'n', xlab = "", ylab = "", xlim = range(Time), ylim = range(log(weight)))
})
xyplot(
  log(weight)~Time,
  groups = Chick,
  col = rep(c(1,1,2:4), each = 10),
  data = ChickWeight,
  type = "1")

#question 3
install.packages("TeachingDemos")
library(TeachingDemos)
x <- seq(-2, 6, len=25)
y <- seq(-2, 6, len=25)
z <- outer(x, y, FUN=function(x,y) -x*y*exp(-x^2-y^2))

contour(x,y,z, main="Contour Plot")
filled.contour(x,y,z, main="Filled Contour Plot")
filled.contour(x,y,z, color.palette = heat.colors)
filled.contour(x,y,z,color.palette=colorRampPalette(c("red","white","blue")))
persp(x,y,z, shade=.75, col="green3", srt = 45) # 3-D Surface Plotrotate.persp(x,y,z
rotate.persp(x,y,z)

# question 4
a <- c(-1,2,6,7,9)
b <- seq(-pi, pi, 0.1)
par(mfrow=c(1,2))
shape1 = c(5,9,16)
shape2 = c(2,10,13)
plot(a, type = 'o', lty = 2, pch=shape1, col="red", main = "Figure 1",xlab = "x", ylab = 'y',axes=FALSE)
abline(v = 4, h = 4, lty = 3, lwd = 3 , col = 'purple')
axis(1)
axis(2, at=seq(0,10,by=.5), labels=format(seq(0,10,by=.5), nsmall=3))
axis(3)
axis(4, at=1:10, las=2)
text(3,3, "This is a plot text", font=2, cex=1.5 )
mtext("This is Margin Text for Figure 1", side=3, line=.5)
legend(1,8, legend = "Line 1", col = 'red', lty = 3, cex = 0.8)
plot(b, sin(b), main="Figure 2", pch=shape2, col="blue")
abline(h = 0, lty = 1, lwd = 3, col = "yellow")
abline(v = -1, lty = 4, lwd = 3, col = "green")
axis(4, at= -3:3, las = 2)
text(0, 0.5, expression(y==alpha[1]*x+alpha[2]*x^2), cex=1.5)
mtext("This is Margin Text for Figure 2", side=3, line=.5)
legend(-3,0.8, legend = "sin(x)", col = "blue", pch = 6, cex = 1)

