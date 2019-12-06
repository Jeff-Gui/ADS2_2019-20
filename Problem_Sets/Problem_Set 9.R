data = read.table("/Users/jefft/Library/Mobile Documents/com~apple~CloudDocs/ADS/Problem sets/barley.txt")
# equivalent:
# data = scan("/Users/jefft/Library/Mobile Documents/com~apple~CloudDocs/ADS/Problem sets/barley.txt")
names(data) = "weight"
summary(data$weight)
t.test(data$weight, mu = 50, alternative = "two.sided") # p = 4.9e-7, seems that brewery has not added enough barley
# Assumption 1: data is continuous and randomly-selected: we don't know
# Assumption 2: sampling distribution is normally distributed: we don't know, only one sample
# Assumption 3: mean and standard error are independent: T
sampling_errors<-vector()
sampling_means<-vector()
for (replicate in 1:100){
  barley_sample<-sample(data, size = length(data), replace = TRUE) 
  standard_error<-sd(barley_sample)/sqrt(length(barley_sample)) 
  sampling_errors<-c(sampling_errors, standard_error) 
  sampling_means<-c(sampling_means, mean(barley_sample))
}
plot(sampling_means, sampling_errors, xlab = "Sample mean", ylab = "Standard error") 
lmfit<-lm(sampling_errors~sampling_means) # relationship can be tested by linear regression
abline(lmfit, col = 'red')
summary(lmfit) # and can be quantified by coefficient

poss = c()
for (i in 2:50){
  count = 0
  for (j in 1:100){
    p = t.test(sample(data$weight, i), mu = 50, alternative = "two.sided")$p.value
    if (p <= 0.05){
      count = count + 1
    }
  }
  poss = c(poss, count / 100)
}
plot(2:50, poss, xlab = "sample size", ylab = "p<=0.05 probabiliry", main = "100 times t-test for different sample size", pch = 20, col = "coral")
abline(h=0.95)
# can have 95% confidence if n >= 16