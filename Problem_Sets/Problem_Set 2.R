population <- rnorm(1e6,100,5)
popmean <- round(mean(population),1)
popmean
popsd <- round(sd(population),1)
popsd
spmeanv <- vector()
spsdv <- vector()
# My prediciton of bigger sample size: mean will be 100, sd will be 5
for (i in 1:1000){
  sp <- sample(population, 100)
  spmean <- round(mean(sp),1) # round (x, digits=0) digits: number of decimal number
  spmeanv <- c(spmeanv,spmean)
  spsd <- round(sd(sp),1)
  spsdv <- c(spsdv, spsd)
}
hist(spmeanv, col="orange", xlab= "mean of sample")
hist(spsdv, col="coral", xlab="sd of sample")
round(mean(spmeanv),1)
round(mean(spsdv),1)

# Breakfast and student performance

# Sample bias: 
#1. Only breakfast included -> What if other food eaten influence the result
#2. Only children aged 9 to 11 included; -> Small effect.
#3. Academic performance recorded over one months later. -> Relationship does not mean causality.
# Improvement: Wider range of sampled children; Include all food they ate before certain meal

# Unrelated question randomised reponse;
if (sample(c(0,1),1,replace = TRUE) == 1){
  # Head: answer "Were you born between 1 Jaunary and 30 June?"
  # Equal possibility to say yes or no because people are honest.
  if (sample(c(0,1),1)==1, replece = TRUE){
    answer <- "yes"
  } else {
    answer <- "no"
  } else {
    # Tail: answer "Have you ever shoplifted?"
    # More likely to say no than yes.
  }
}
# 300 participants, 112 "yes", 188 "no";
# Suppose a% people lies when asked shoplifting question
# If ask public directlly, shoplifting people percentage should be (112 + 188 * a%) / 300
# If the above method is used, suppose 2x people answered the "equal" question, than the 
# percentage should be ( (112-x) + (188 - x) * a% ) / (300 - 2x)
