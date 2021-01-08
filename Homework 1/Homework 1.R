#Stat5145 Homework 1

#question 1
#(a)
a=seq(1,21, by=2)
a

#(b)
b=seq(50,11, by=-3)
b

#(c)
c=2^(seq(0,10,1))
c

#(d)
d = matrix(c(1:16), nrow = 4, ncol = 4, byrow = TRUE)
colnames(d) = c("C1", "C2", "C3", "C4")
rownames(d) = c("R1", "R2", "R3", "R4")
d


#question 2
#(a)
x = sample(1:50, 110, replace = TRUE)
A = matrix(x, nrow = 10, ncol = 11, byrow = TRUE)
A

#(b)
apply(A, 1, max)

#(c)
apply(A, 2, sd)

#(d)
b = A[,ncol(A)]
b = matrix(b, nrow = 10, ncol = 1, byrow = TRUE)
A = A[,-ncol(A)]
b

#(e)
#check determinant is not 0
det(A)
#solve
solve(A,b)

#(f)
SumVector <- c(apply(A, 1, sum))
#other way to acheive
SumVector2 <- rowSums(A)
SumVector
SumVector2
# SumVector2 <- A%*% rep(1, ncol(A))

#(g)
AcolSum = apply(A,2,sum)
B = matrix(AcolSum, nrow = 1, ncol = 10)
B

#(h)
D = diag(1/(1:10))
D


#question 3
#(a)
state.name[grep(" ", state.name)]
length(state.name[grep(" ", state.name)])
# there are 10 of them

#(b)
#the longest state name
state.name[which.max(nchar(state.name))]
#how long is it
max(nchar(state.name))

#(c)
#create a vector of single word US state
WithSpace <- state.name[grep(" ", state.name)]
WithoutSpace<- state.name[!state.name %in% WithSpace]
#Find the longest name 
WithoutSpace[which.max(nchar(WithoutSpace))]
#how long it is
max(nchar(WithoutSpace))

#(d)
gsub("a", "Z", state.name, ignore.case = TRUE)


#question 4
#(a)
MrsSmith <- seq.Date(as.Date("1999/3/3"), as.Date("2002/3/3"), by="6 months")
MrsSmith

#(b)
dates1 <- c("9/10/1999", "2/10/2000", "9/16/2000", "2/23/2001", "9/9/2001", "3/28/2002")
date2 <- as.Date(dates1, format = "%m/%d/%Y")
date2

#(c)
# remove the first element 
MrsSmith <- MrsSmith[-1]
MrsSmith - date2
# No. The second visit was supposed to be 2000/3/3, but she showed up 2000/2/10, 22 days later. Also, she was supposed to show up on 2002/3/3 for
# her sixth visit, but she showed up on 2002/3/28, 25 days earlier. 
