# Rolling Dice
dice_number <- 5
roll_time <- 1000
tr <- c()
for (j in 1:roll_time){
  sr <- c()
  result <- c()
  for (i in 1:dice_number){
    result <- c(result, sample(1:6,1))
  }
  tr <- c(tr,sum(result))
}
hist(tr,col = "green", ylab = "Outcome", main = paste((dice_number),"dice"))
# 68 - 95 - 99.7 rule:
sum(mean(tr)-sd(tr) < tr & tr < mean(tr)+sd(tr))/roll_time * 100 # 68
sum(mean(tr)-2*sd(tr) < tr & tr < mean(tr)+2*sd(tr))/roll_time * 100 # 95
sum(mean(tr)-3*sd(tr) < tr & tr < mean(tr)+3*sd(tr))/roll_time * 100 # 99.7
# 正态性检验 - nortest 包
library(nortest)
ad.test(tr)
# Why result is so not normal???

# Bean machine
beans <- 1000
layers <- 50 # large enough in CLT means large enough layers, *not beans*
bean_result <- c()
for (i in 1:beans){
  bean_result <- c(bean_result, sum(sample(c(0,1),layers,replace=TRUE, p=c(0.5,0.5)))) 
}
hist(bean_result, col="coral", main = "Bean machine", axes = T,breaks=0:layers)


