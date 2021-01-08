# take home exam 

# question 1 (a)
# create function 
Adjacent<- function(x){
  #convert string to vector to run for loop 
  y<-strsplit(x, "")[[1]]
  # create a vector to store adjacent 
  vec=c()
  for(i in 2:length(y)){
    vec[i-1]=paste(y[i-1],y[i])
  }
  # remove space between each characters 
  newChar<- gsub(" ", "", vec)
  # table 
  return(table(newChar))
}
# define string
x<-'CAGACAAAAC'
# call function
Adjacent(x)

# (b)
# generate 10000 characters randomly from A, C, G, T and store it in a vector
m<- sample(c('A','C','G','T'), 10000, replace=TRUE)
# make the m vector a string 
n<-paste(m,collapse='')
#call function from (a)
Adjacent(n)


# (c)
set.seed(1000)

# empty vector to store each character string
entry <- c()
# k can be any numbers  
# drawing function 
Draw <- function(k) {
  for (i in 1:10000) {
# generate k characters randomly 
    dummy <- sample(c('A','C','G','T'), k, replace = TRUE)
# make it strings and store it in vector 
    entry[i] <- paste(dummy,collapse='')
    # if CG or AAA then remove it from vector and redo it 
    if(entry[i]=='CG' | entry[i]=='AAA') {
      entry <- entry[-i]
      i = i-1
    }
  }
  return(entry)
}

# call function 
# k = 2
char2<-Draw(2)
table(char2)

# k= 3
char3 <- Draw(3)
table(char3)

# ------------------------------------------------------------------------------------------#
# question 2 
# (a)
view(airquality)
#plot ozon  
plot(airquality$Day, airquality$Ozone, pch = 15, col="orange", main = "Ozon per day")


# different types
plot(airquality$Day, airquality$Ozone, type = "h")
plot(airquality$Day, airquality$Ozone, type = "l")
str(airquality)

# plot with lines 
plot(airquality$Ozone, pch = 15, main="Ozon")
# add lines 
abline(h=120,lwd=2, col = "yellow")
abline(h=140,lwd=2, col = "orange")
abline(h=160,lwd=2, col = "red")

# get severe ozon date, index and ozon number 
for (i in 1:nrow(airquality)) {
  if(airquality[i,]$Ozone>160 & !(is.na(airquality[i,]$Ozone))) {
    index <- i
    ozon <-airquality[i,]$Ozone
    d<- paste(airquality[i,]$Month, airquality[i,]$Day, sep = "/")
  }
}

# add text 
text(78, 168,"Ozon severe date: " , font = 3)
text(index-10, ozon,  d , font = 2, col = "red")


# (b)
plot(airquality$Ozone, airquality$Solar.R, xlab = "Solar.R", ylab = "Ozon",
     col= "blue", main = "Ozon vs Solar.R", pch = 8)

# (c)
par(mar=c(1,1,1,1))
plot.new()
# wind 
coplot(Ozone~Solar.R | Wind, data = na.omit(airquality), col= "orange", pch = 12)
#temp 
coplot(Ozone~Solar.R | Temp, data = na.omit(airquality), col="purple", pch=15)
#both
coplot(Ozone~Solar.R | Wind*Temp, data = na.omit(airquality), col = c("orange", "red"))

# xyplot
library(lattice)
# create wind and temperature levels for xyplot. 
wind <- cut(airquality$Wind, breaks=c(0, 5, 10, 15, 20, 25))
temp <- cut(airquality$Temp, breaks=c(55, 65, 75, 85, 95, 100))
#create plot 
# wind
xyplot(Ozone~Solar.R | wind, data = airquality, main="Ozon vs Solar.R by wind", col="green", pch=3)
# temp
xyplot(Ozone~Solar.R | temp, data = airquality, main="Ozon vs Solar.R by temperature", col="purple", pch=6)
# both levels  
xyplot(Ozone~Solar.R | wind*temp, data = airquality, main="Ozon vs Solar.R by wind and temperature", 
col=c("green","purple"), pch=c(3,6))

# ------------------------------------------------------------------------------------------#
# question 3
# (a)

#function 
Wvalue<- function(x,y) {
  # calculate differences x-y
  # Z is a vector to store the absolute value differences
  # signZ is a vector to store the signed differences 
  z <- abs(x- y)
  signZ <- x-y
  
  # remove element 0 if there's any 
  for (i in 1:length(z)) {
    if(z[i]==0 && !(is.na(z[i]))) {
      z <- z[-i]
      signZ<-signZ[-i]
    }
  }
  
  # get rank 
  rankZ = rank(z)
  
  # define W sign positive and W sign negative 
  Wpositive = 0
  Wnegative = 0
  # get sum of each sign by using loop and if statement 
  for (i in 1:length(signZ)) {
    if(signZ[i] > 0) {
      Wpositive <- rankZ[i] + Wpositive
    }
    else if(signZ[i]<0) {
      Wnegative <- rankZ[i] + Wnegative
    }
  }
  # make a list of Wpositive and Wnegative 
  result <- list(Positive= Wpositive, Negative= Wnegative)
  return(result)
}

x <- c(3.43,3.45,2.5,3.86,1.52,2,1.30)
y <- c(0.86,4.05,3.18,3.84,2.52,2,2.56)
# call the function 
result <- Wvalue(x,y)

# (b)
# create a new variable d 
# first I did the way the word file in canvas mentioned, but I got the same number for 20000 times.
# So I changed how randomly generate numbers. 
# randomely generate numbers range -1.26 to 2.57(range of x-y)
d <- x-y
set.seed(1000)

dhat<- runif(length(d), -1.26, 2.57)
# less negative numbers than positive numbers 


# function to calculate w value 
# edited part (a) function 
Wvalueb<- function(x) {
  # Z is a vector to store the absolute value differences
  # signZ is a vector to store the signed differences 
  z <- abs(x)
  signZ <- x
  
  # remove element 0 if there's any 
  for (i in 1:length(z)) {
    if(z[i]==0 && !(is.na(z[i]))) {
      z <- z[-i]
      signZ<-signZ[-i]
    }
  }
  
  # get rank 
  rankZ = rank(z)
  
  Wpositive = 0
  Wnegative = 0
  # get sum of each sign by using loop and if statement 
  for (i in 1:length(signZ)) {
    if(signZ[i] > 0 && !(is.na(z[i]))) {
      Wpositive <- rankZ[i] + Wpositive
    }
    else if(signZ[i]<0 && !(is.na(z[i]))) {
      Wnegative <- rankZ[i] + Wnegative
    }
  }
  # get test statistics W. Randomly geterated value dhat has less negative, so there's high possibility of getting smaller Wnegative
  # than Wpositive, so get Wnegative  
  result <- Wnegative 
  return(result)
}


# function to calculate p-value 
Pvalue <- function(){
N <- 20000
# vector to stpre W value from the function 
P <-c()
# generate dhat randomly and call the function 20000 times
for(i in 1:N) {
  # generate random numbers range from -1.26 to 2.57
  # d is from part(a) x-y 
  dhat<- runif(length(d), -1.26, 2.57)
  # call Wvalue funtion 
  P[i]<- Wvalueb(dhat)
}

# get 95% interval 
Lower<-quantile(P, 0.025)
Upper<- quantile(P, 0.975)
c(Lower,Upper)
# count the number of variable that is out of range of the interval 
count <-0
# get out of range counts 
for(i in 1:length(P)){
  if(P[i]<Lower | P[i]>Upper){
    count <- count + 1
  }
}
# P-value is count/20000
Pvalue <- count/N
return(Pvalue)
}

set.seed(1000)
# call the p-value function
Pvalue()


# (c)
# result from (a)
# use smaller number 
targetvalue<- min(unlist(result))
N <- length(x)
#normal approximation Z = (W-n*(n+1)/4)/sqrt((n(n+1)(2n+1))/24)
normal <- (targetvalue - (N*(N+1))/4)/sqrt((N*(N+1)*(2*N+1))/24)
pnorm(normal)

targetvalue<- max(unlist(result))
N <- length(x)
#normal approximation Z = (W-n*(n+1)/4)/sqrt((n(n+1)(2n+1))/24)
normal <- (targetvalue - (N*(N+1))/4)/sqrt((N*(N+1)*(2*N+1))/24)
pnorm(normal)

# check actual p value wilcoxon signed rank test function 
library(MASS)
wilcox.test(x,y,paired=TRUE)

# ------------------------------------------------------------------------------------------#
# question 5

# when we use triangular grid, there's an area inside the triangle where the needle will not 
# cross the triangle lines if the needle center falls into the area. 
# I have attached the pdf files that I found online and used for reference. 


# needle's length
# needle's length should be shorter than triangle's height 
needle <- 1
# equilateral triangle height
height <- 2

# assume the triangle is equilateral triangle. So side s = h*(2/sqrt(3))
s = height*(2/sqrt(3))

# r is the distance from the centroid to the midpoint of a side 
r = (sqrt(3)/6)*s

# R, curcumscribing circle of radius 
R <- (sqrt(3)/3)*s

set.seed(200)

# function 
Buffon <- function(n) {
  # the number of crossing trianble lines
  cross <- 0
  # repeat n times to get probability and estimated pi 
  for (i in 1:n){
    # theta is range of 0 to pi/3. Equilateral triangle  
    theta <- runif(1, 0, pi/3)
    
    # if the center position is in the certain position (the inner triangle) of the triangle, 
    # the needle does not cross the line 
    # get the inner triangle area 
    a1 <- (needle/2)*sin(theta)
    a2 <- (needle/2)*sin((pi/3)-theta)
    a3 <- (needle/2)*sin(((2*pi)/3)-theta)
    
    e <- a1/tan(pi/3)
    f2 <- a2/sin(pi/3)
    f3 <- a3/sin(pi/3)
    t <- s - (2*e + f2 + f3)
    innerarea <- t * theta^2*(sqrt(3)/4)
    
    # equilateral area
    area <- (s*height)/2

    # needle center position 0 to the outer triangle area 
    center <- runif(1, 0, area)
    
    # if center position is bigger than the inner triangle area then cross + 1
    if(center > innerarea) {
      cross <- cross + 1
    }
  }
  
  # probability of line crossing 
  probability <- cross/n
  
  
  estimatepi <- (3*(needle/height)*(8-sqrt(3)*(needle/height)))/(2*(3-2*probability))
  return(estimatepi)
}

# call the function 
# n = 20
Buffon(20)
# n = 50
Buffon(50)
# n = 100
Buffon(100)
# n = 10000
Buffon(10000)

# histgram 
pi20 <- sapply(rep(20,20), Buffon)
hist(pi20, xlab="estimated pi", main = "estimated pi n=20", col = "blue")
# n=50
pi50 <- sapply(rep(50,50), Buffon)
hist(pi50, xlab="estimated pi", main = "estimated pi n=50", col = "red")
box()
# n=100
pi100 <- sapply(rep(100,100), Buffon)
hist(pi100, xlab="estimated pi", main = "estimated pi n=100", col = "green")
box()
# n=10000
# it takes time to run 
pi10000 <- sapply(rep(10000,10000), Buffon)
hist(pi10000, xlab="estimated pi", main = "estimated pi n=10000", col = rainbow(7))
box()

# from the histogram, we can say it is normally distributed. 

# plot 
plot(pi100, ylab="estimated pi", main="estimated pi n=100", col=rainbow(7), pch=10)


# ------------------------------------------------------------------------------------------#
# The above simulation is not close to actual pi, so I did another simulation 

# another simulation 
# r is the distance from the centroid to the midpoint of a side 
r = (sqrt(3)/6)*s

Buffon2 <- function(n) {
  # the number of crossing lines
  cross <- 0
  # repeat n times to get probability and estimated pi 
  for (i in 1:n){
    # theta is range of 0 to pi/3. Equilateral triangle
    theta <- runif(1, 0, pi/3)
    
    # center of the needle 
    # 0 to r. Not sure if it is correct or not. 
    x <- runif(1, 0, r)
    
    # if the center position is in the certain position(shaded area) of the triangle, 
    # the needle does not cross the line 
    # calculate the point of the shaded area. If x is less than any of these point, 
    # then the needle would cross the line 
    a1 <- (needle/2)*sin(theta)
    a2 <- (needle/2)*sin((pi/3)-theta)
    a3 <- (needle/2)*sin(((2*pi)/3)-theta)
 

    # x is smaller any of a1, a2, or a3 then cross + 1
    if(x < abs(a1) | x < abs(a2) | x < abs(a3)) {
      cross <- cross + 1
    }
  }
  
  # probability of line crossing 
  probability <- cross/n
  
  estimatepi <- (3*(needle/height)*(8-sqrt(3)*(needle/height)))/(2*(3-2*probability))
  return(estimatepi)
}
# call the function 
Buffon2(20)
Buffon2(50)
Buffon2(100)
Buffon2(10000)

# it is closer to 3.14, but if I change the length of needle and height, the results would be very different. 

# plots 
secondpi20 <- sapply(rep(20,20), Buffon2)
hist(secondpi20, xlab = "estimated pi", main="Estimated pi n=20")

# n=50
secondpi50 <- sapply(rep(50,50), Buffon2)
hist(secondpi50, xlab = "estimated pi", main="Estimated pi n=50")

# n=100
secondpi100 <- sapply(rep(100,100), Buffon2)
hist(secondpi100, xlab = "estimated pi", main="Estimated pi n=100")

# n= 10000
secondpi10000 <- sapply(rep(10000,10000), Buffon2)
hist(secondpi10000, xlab = "estimated pi", main="Estimated pi n=10000")

# plot 
# plot 
plot(secondpi100, ylab="estimated pi", main="estimated pi n=100", col="red", pch=16)
