# 1. one sample t test
# Hypothesis: 
#   Null: No difference between SOX17 expression level between two cell lines
#   Alternative: SOX17 expression is significantly different between two cell lines
# Distribution: t distribution: small sample number, unknown sample deviation
# Critical value
cv = qt(0.975, 3-1)
# Test value
tv = (mean(c(15,30,50)) - 8.9) / (sd(c(15,30,50)) / sqrt(3))
tv > cv
# Decision: Fail to reject null hypothesis.
# There is no significant difference between newly generated cell line and average SOX17 expression. Bigger sample size required

# 2. paired two sample t test
# Hypothesis:
#   Null: No repression of AML1 expression after treatment for each patient.
#   Alternative: Significant repression of AML1 expression after treatment for each patient.
# Distribution: t distribution: small sample size, unknown sample deviation
# Critical value
cv = qt(0.95, 5-1) # one-tailed
# Test value
tv = ( mean(c(102-74, 340-56, 234-70, 332-104, 129-11)) ) / ( sd(c(102-74, 340-56, 324-70, 332-104, 129-11)) / sqrt(4) )
tv > cv
# Decision: Reject null hypothesis
# There is significant difference between AML1 expression level before and after treatment for each patient
t.test(c(102,340,234,332,129),c(74,56,70,104,11), alternative = "greater", paired = T)

# 3. unpaired two sample t test
geneexp=read.csv("/Users/jefft/Library/Mobile Documents/com~apple~CloudDocs/ADS/Problem sets/Problem_Set_10.csv", na.strings = "")
head(geneexp)
tail(geneexp)
dim(geneexp)
anyNA(geneexp)
geneexp$gname = as.character(geneexp$gname)
genes = c()
for (i in 1:dim(geneexp)[1]){
  p = 100
  v1 = as.vector(as.matrix(geneexp[i,2:5]))
  v2 = as.vector(as.matrix(geneexp[i,6:9]))
  if (sum(v1==0)<4 | sum(v2==0)<4){ 
    p = t.test(v1, v2)$p.value
  }
  if (p < 0.05){
    genes = c(genes, geneexp[i,1])
  }
}

genes
