library(truncnorm)
country_raws = c()
class_means = rnorm(10000, 50, 5)
class_sizes = c(sample(5:40, 9999, replace = T), 26)
classes = c()
for (i in 1:9999){
  classes = c(classes, replicate(class_sizes[i],i))
  raw_scores = rtruncnorm(class_sizes[i], 0, 100, class_means[i], 10)
  country_raws = c(country_raws, raw_scores)
}
unlucky_class = rnorm(26, 40, 8)
country_raws = c(country_raws, unlucky_class)
classes = c(classes, replicate(26,10000))

data = data.frame('country_raws'=country_raws, 'classes'=classes)
data = data[order(data$country_raws),]
data = cbind(data, 'percentile'=(1:length(country_raws)) / length(country_raws) * 100)
data = data[order(data$classes),]

normative_means = c()
j = 1
for (i in 1:10000){
  current_mean = mean(data$percentile[j : j+class_sizes[i]-1])
  normative_means = c(normative_means, replicate(class_sizes[i], current_mean))
  if (i>9000){
    print(j)
    print(current_mean)
  }
  j = j + class_sizes[i]
}
data = cbind(data, 'normative_means' = normative_means)

normative_means = unique(normative_means)
hist(normative_means)

hist(country_raws) # a normal distribution with mean slightly purturbs 50
unlucky_class_normative = data$percentile[which(data$classes==10000)]
hist(unlucky_class_normative) # an exponetial distribution with mean around 26

sum(normative_means[10000] > normative_means[1:9999]) / 9999 # 0.015

# How does the average of Leonie’s team’s normative scores now compare to that of Sheldon’s team (as in question 3)???

L = c(64,63,62,59)
S = c(70,63,61,56)
mean(L) > mean(S) # False
for (i in 1:4){
  L[i] = sum(L[i]>data$percentile) / length(data$percentile) * 100
  S[i] = sum(S[i]>data$percentile) / length(data$percentile) * 100
}
mean(L) > mean(S) # True
