setwd("/Users/jefft/Library/Mobile Documents/com~apple~CloudDocs/ADS/Lecture/")
data = read.csv("lecture5_Rdata_diamonds_samples100_mdf.csv")
head(data)
# 6.1 Missing data
  # 6.1.1 Screen-Diagnosis
head(is.na(data)) # no NA found
tail(data)
tail(is.na(data)) # some NAs emerges
apply(is.na(data),2,which) # find row, col of NA; ???

  # 6.1.2 Treat
dim(data)
data.noNA = data[complete.cases(data),] # complete.cases(vectors, matrices or data.frames), indicate cases without missing values.
    # complete.cases(c(1,NA,3))
dim(data.noNA)
print(data[!complete.cases(data),]) # opposite output of certain function: !-
  
  # 6.1.3 Documentation: 92 remained rows after removing NAs, original 103 observations (rows)

# 6.2 Duplicated data
  # 6.2.1 Screen-diagnosis
duplicated(data.noNA)
frw.idx=which(duplicated(data.noNA)) #duplciated() will only give you the duplicated rows, but not the original rows, so we need the next line to get the originals
rvs.idx=which(duplicated(data.noNA,fromLast = TRUE)) 
  
  # 6.2.2 Treat: use either slicing by duplicated() or simply unique()
dim(data.noNA)
data.noNA.noDup = data.noNA[!duplicated(data.noNA),]
data.noNA[!duplicated(data.noNA),] == unique(data.noNA)
identical(data.noNA.noDup,unique(data.noNA)) # Only content identical
dim(data.noNA.noDup) # After removing duplicated rows, dimension rows decrease from 92 to 89
  # 6.2.3 Documentation: ...

# 6.3 Screen
  # 6.3.1 Screen of volume ~ carat (bigger dimonds should have larger carat)
data.noNA.noDup=data.frame(data.noNA.noDup,volume=data.noNA.noDup$x*data.noNA.noDup$y *data.noNA.noDup$z) # coustruct a new line
# *Alt: cbind()* data.noNA.noDup1=cbind(data.noNA.noDup,volume=data.noNA.noDup$x*data.noNA.noDup$y *data.noNA.noDup$z) # coustruct a new line
head(data.noNA.noDup)
plot(x=data.noNA.noDup$carat,y=data.noNA.noDup$volume, pch=20,col="darkgoldenrod4", las=1,xlab="carat",ylab="volume",
     main="diamond carat ~ volume")
text(data.noNA.noDup$carat, data.noNA.noDup$volume, labels=data.noNA.noDup$ID,col="dimgray",
     cex= 0.7, pos=4)

  # 6.3.2 Diagnosis
print(data.noNA.noDup[which(data.noNA.noDup$ID=="7"),])
      # is there any duplications (volume recorded by mistake?)
fwrd.dup.idx=which(duplicated(data.noNA.noDup[,9:11]))
rvse.dup.idx=which(duplicated(data.noNA.noDup[,9:11],fromLast = TRUE)) 
data.noNA.noDup[c(fwrd.dup.idx,rvse.dup.idx),]
print(data.noNA.noDup[which(data.noNA.noDup$ID=="28"),]) # z=0, definitely wrong data

  # 6.3.3 Treat
data.noNA.noDup.noStrg = data.noNA.noDup[-which(data.noNA.noDup$ID==7 | data.noNA.noDup$ID==28),]
data.noNA.noDup.noStrg2 = data.noNA.noDup[!which(data.noNA.noDup$ID==7 | data.noNA.noDup$ID==28),]
identical(data.noNA.noDup.noStrg,data.noNA.noDup.noStrg2) # FALSE
  # * which() return row number, duplicated() return booleans, therefore, use "-" to get opposite vector; use "!" to get opposite boolean.
dim(data.noNA.noDup)
dim(data.noNA.noDup.noStrg)
  
  # 6.3.4 Documentation: 2 rows removed

#===========Problem Set=======================================
pdata = data.noNA.noDup.noStrg
# 2. Correct typos
summary(pdata) # cut: one "Idea" found, should be "Ideal"
pdata[which(pdata$cut=="Idea"),which(names(pdata)=="cut")] = "Ideal"
# However, levels(pdata$cut still have redudant level "Idea")
levels(pdata$cut)[which(levels(pdata$cut)=="Idea")] = "Ideal"
summary(pdata)
# Therefore, directly modify factor is easier
  # Documentation: cut column typo found and corrected

# 3. Outliers
plot(x=pdata$carat,y=pdata$price, pch=20,col="darkgoldenrod4", las=1,xlab="carat",ylab="price",
     main="diamond carat ~ price")
text(pdata$carat, pdata$price, labels=pdata$ID,col="dimgray",
     cex= 0.7, pos=4) # outliers found: ID: 29,27,96
#   Diganose
pdata[which(pdata$ID=="29"),] # Large carat, low price; likely price duplication
frd = which(duplicated(pdata[,8]))
rrd = which(duplicated(pdata[,8],fromLast = TRUE)) # only price duplication in the whole data
pdata[c(frd,rrd),] # Not adjacent, but still likely to be a mistake: delete this row

pdata[which(pdata$ID=="27"),] # Low carat, very high price; likely carat duplication
which(pdata[,2]==1.57) # no duplication, color of 29 is 2rd best, but clarity is poor: suspicious

pdata[which(pdata$ID=="96"),] # Low carat, high price
pdata[which(pdata[,2]==1),] # Four continuous rows with the same carat;
# But ID.96 has the best color and clarity. High price seems to be reasonalbe.

#   Treat
pdata.olpcd = pdata[-which(pdata$ID=="29"),]
dim(pdata.olpcd)
pdata.olpcd = cbind(pdata.olpcd,"suspicious"=rep(FALSE,times=dim(pdata.olpcd)[1]))
pdata.olpcd[which(pdata$ID=="27"),"suspicious"] = TRUE # use col name to index
summary(pdata.olpcd)

# 4.Bounce: category-separeted scatters
library(ggplot2)
ggplot(pdata.olpcd, aes(x=carat,y=price)) +
  geom_point(aes(color=clarity)) + # 根据组分颜色
  geom_text(aes(label = price),hjust = 0.1, nudge_x = 0.05 ,size=1) + # point上数据，调整位置
  scale_y_continuous(trans="log2") + # 调整y轴坐标
  labs(title="Price VS Carat in diamonds") +
  geom_smooth(method = "lm") + #线性回归
  facet_grid(.~clarity) #分组


#===========Simple Data types=================================
typeof(data$clarity)
typeof(data$ID)
class(data$ID)
class(data$cut) #factor
levels(data$cut)

a <- as.matrix(data) # convert to homogenous matrix of character
head(a)
typeof(a[1,])

var1 <- c(2,3,4,5)
typeof(var1)
var2 <- c(2L,3L,4L,5L)
typeof(var2)
var.char <- c("a","b")
var.fac = factor(c('mid',"high","mid","low"),levels=c("mid","high","low"))
