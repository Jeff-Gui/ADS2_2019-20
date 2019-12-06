# 1. one sample sample size estimation
#   experiment type: 2 sample, unpaired, two sided t-test
#   significance level 0.05
#   critical value 1.96
qnorm(0.975)
#   effect size 100
#   variance 385
round((qnorm(0.975) * 385 / 100)**2) # 56
#   n total
round(56 / 0.95) + 1 # 60
#   Conclusion: 9 women need to be enrolled

# 2. alpha: 0.05, beta: 0.2, critical value 1.96 & 0.84, effect size 0.25, variation 0.42
qnorm(0.975)
qnorm(0.8)
# n total
ceiling(2*(((qnorm(0.975)+qnorm(0.8)) / (0.25/0.42))**2)) # 45
ceiling(power.t.test(delta = 0.25, sd = 0.42, sig.level = 0.05, type = "two.sample", alternative = "two.sided", power = 0.8)$n) # 46

# 3. relationships
# 3.1 
power.t.test(sig.level = 0.05, n = 20, sd = 0.5, delta = 0.4, type = "two.sample", alternative = "two.sided")$power #0.69
# 3.2 increase sig.level to 0.1
power.t.test(sig.level = 0.1, n = 20, sd = 0.5, delta = 0.4, type = "two.sample", alternative = "two.sided")$power #0.80
# 3.3 decrease n to 10
power.t.test(sig.level = 0.05, n = 10, sd = 0.5, delta = 0.4, type = "two.sample", alternative = "two.sided")$power #0.39
# 3.4 decrease effect size to 10
power.t.test(sig.level = 0.05, n = 20, sd = 0.5, delta = 0.8, type = "two.sample", alternative = "two.sided")$power #0.99
# 3.5 optimal challenge (power~sg.level~sample.size~effect.size)
# Statistical power vs significance level, sample size, and effect size
# 3.5.1 sig.level
power = c()
for (sig.level in seq(0.01, 0.2, by=0.002)){
  p = 0
  for (j in 1:5000){
    A = rnorm(20, 0, 0.5)
    B = rnorm(20, 0.4, 0.5)
    if (t.test(A,B)$p.value < sig.level){
      p = p + 1
    }
  }
  power = c(power, p/5000)
}
plot(seq(0.01, 0.2, by=0.002), power, type = 'b', xlab = 'sig.level', ylab = 'power', main = 'delta: 0.4, sd: 0.5, n: 20') #linear

# 3.5.2 sample size
power = c()
for (n in seq(10, 30, by=0.2)){
  p = 0
  for (j in 1:5000){
    A = rnorm(n, 0, 0.5)
    B = rnorm(n, 0.4, 0.5)
    if (t.test(A,B)$p.value < 0.05){
      p = p + 1
    }
  }
  power = c(power, p/5000)
}
plot(seq(10,30,by=0.2), power, type = 'b', xlab = 'n', ylab = 'power', main = 'delta: 0.4, sd: 0.5, sig.level: 0.05') #linear

# 3.5.3 effect size
power = c()
for (delta in seq(0.1, 2, by=0.02)){
  p = 0
  for (j in 1:5000){
    A = rnorm(20, 0, 0.5)
    B = rnorm(20, delta, 0.5)
    if (t.test(A,B)$p.value < 0.05){
      p = p + 1
    }
  }
  power = c(power, p/5000)
}
plot(seq(0.1,2,by=0.02), power, type = 'b', xlab = 'effect size', ylab = 'power', main = 'sd: 0.5, sig.level: 0.05, n: 20') # exponetial

# 4 sample size vs p_value
# 4.1
A = rnorm(5,10,5)
B = rnorm(5,11,5)
# since sample size is small, use t.test better
t.test(A,B) # p>0.05
# 4.2
A = rnorm(500,10,5)
B = rnorm(500,11,5)
t.test(A,B) # p<0.05
# 4.3 optimal challenge
p = c()
for (n in 5:104){
  A = rnorm(n, 10, 5)
  B = rnorm(n, 11, 5)
  p = c(p, t.test(A, B)$p.value)
}
plot(5:104, p, xlab = 'n', ylab = 'p value', main = 'delta:1, sd: 5')



