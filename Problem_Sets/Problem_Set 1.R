class <- rnorm(100,86,5.0)
hist(class, col='coral',xlab='Marks',ylab='Students number', main='Class 1 Awesome University')
print(summary(class))
count <- 0
for (i in 1:length(class)){
  if (class[i]>91 || class[i]<81){count <- count+1}
}
count <- 0
for (i in 1:length(class)){
  if (class[i]>96 || class[i]<76){count <- count+1}
}
# problem: scores higher than 100 may occur
# get good grades problem
# 1.Random selection
# 2.Select A for each answer: 0.25^10
record <- vector()
for (i in 1:10000){
answer <- sample.int(4,20,replace=TRUE) # generate normal-distrubuted answer
# answer <- sample(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)) # generate "anvanced" answer
met1 <- sample.int(4,20,replace=TRUE)
met2 <- rep(1,time=20)
mark1 <- sum(as.integer(met1==answer))
mark2 <- sum(as.integer(met2==answer))
record <- c(record,mark1>=mark2)
}
summary(record)

