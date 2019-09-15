class <- rnorm(100,86,5.0)
hist(class, col='coral',xlab='Marks',ylab='Students number', main='Class 1 Awesome University')
print(summary(class))
count <- 0
for (i in 1:length(class)){
  if (class[i]>91 || class[i]<81){count <- count+1}
}
# This gives the same result as the following, since Boolean true or false can be sumed as 1 and 0
print(sum( class<91 & class>81 ))

print(sum(class>76 & class<96))
# problem: scores higher than 100 may occur
# get good grades problem
# met1.Random selection
# met2.Select A for each answer: 0.25^10
record1 <- vector()
record2 <- vector()
score_pass <- 10
for (i in 1:10000){
answer <- sample.int(4,20,replace=TRUE) # generate normal-distrubuted answer
# answer <- sample(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)) # generate "anvanced" answer
met1 <- sample.int(4,20,replace=TRUE)
met2 <- rep(1,time=20)
mark1 <- sum(as.integer(met1==answer))
mark2 <- sum(as.integer(met2==answer))
record1 <- c(record1, mark1>=score_pass)
record2 <- c(record2, mark2>=score_pass)
}
print(sum(record1)/10000)
print(sum(record2)/10000)
# probability can be calculated as [(1+3+...+3^10)*C(20,10)]/4**20 = 0.0149
# All possible answer that can pass the exam over all possible random answers.

# Scenario 2: 
answer <- sample(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)) # generate "anvanced" answer
score_pass <- 10
if (5<=score_pass){
print("For method2: select A for each answer, you will get a fail")
} else {
  print("For method2: select A for each answer, you will get a pass")
}
record <- vector()
for (i in 1:10000){
  answer <- sample(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)) # generate "anvanced" answer
  met <- sample.int(4,20,replace=TRUE)
  mark <- sum(as.integer(met==answer))
  record <- c(record, mark>=score_pass)
}
print(sum(record)/10000)

