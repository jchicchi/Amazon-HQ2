rm(list=ls()) #clean global environment
library(readxl)
Atlanta <- read_excel("~/Desktop/BusinessAnalytics/Amazon HQ2/Atlanta top 50.xlsx")
###data cleanup step
Atlanta <- Atlanta[,-c(2,3,4,6)]
Atlanta$`Employee this Site` <- gsub(',','',Atlanta$`Employee this Site`)
Atlanta$`Employee this Site`<-as.numeric(Atlanta$`Employee this Site`)
Atlanta$`Employee All Sites` <- gsub(',','',Atlanta$`Employee All Sites`)
Atlanta$`Employee All Sites`<-as.numeric(Atlanta$`Employee All Sites`)
Atlanta$`Employee All Sites`<- ifelse(Atlanta$`Employee All Sites`==0, Atlanta$`Employee this Site`, Atlanta$`Employee All Sites`)
Boston <- read_excel("~/Desktop/BusinessAnalytics/Amazon HQ2/Boston top 48.xlsx")
###data cleanup step
Boston <- Boston[,-c(2,3,4,6)]
Boston$`Employee this Site` <- gsub(',','',Boston$`Employee this Site`)
Boston$`Employee this Site`<-as.numeric(Boston$`Employee this Site`)
Boston$`Employee All Sites` <- gsub(',','',Boston$`Employee All Sites`)
Boston$`Employee All Sites`<-as.numeric(Boston$`Employee All Sites`)
Boston$`Employee All Sites`<- ifelse(Boston$`Employee All Sites`==0, Boston$`Employee this Site`, Boston$`Employee All Sites`)
Seattle <- read_excel("~/Desktop/BusinessAnalytics/Amazon HQ2/Seattle top 50.xlsx")
###data cleanup step
Seattle <- Seattle[,-c(2)]
Seattle$`Employee this Site` <- gsub(',','',Seattle$`Employee this Site`)
Seattle$`Employee this Site`<-as.numeric(Seattle$`Employee this Site`)
Seattle$`Employee All Sites` <- gsub(',','',Seattle$`Employee All Sites`)
Seattle$`Employee All Sites`<-as.numeric(Seattle$`Employee All Sites`)
Seattle$`Employee All Sites`<- ifelse(Seattle$`Employee All Sites`==0, Seattle$`Employee this Site`, Seattle$`Employee All Sites`)
city <- Seattle
dubious <- city[city$`Employee All Sites`< city$`Employee this Site`, ]
dubious

Atlanta$ratio <- Atlanta$`Employee this Site`/(Atlanta$`Employee All Sites`)
str(Atlanta)
employees.site.Atlanta <- sum(Atlanta$`Employee this Site`)
employees.totals.Atlanta <- sum(Atlanta$`Employee All Sites`)

Boston$ratio <- Boston$`Employee this Site`/(Boston$`Employee All Sites`)
str(Boston)
employees.site.Boston <- sum(Boston$`Employee this Site`)
employees.totals.Boston <- sum(Boston$`Employee All Sites`)
#View(Atlanta_new_top_100)
#library(readxl)

Seattle$ratio <- Seattle$`Employee this Site`/(Seattle$`Employee All Sites`)
str(Seattle)
employees.site.Seattle <- sum(Seattle$`Employee this Site`)
employees.totals.Seattle <- sum(Seattle$`Employee All Sites`)
employees.ratio.Seattle <- employees.site.Seattle/employees.totals.Seattle
employees.ratio.Atlanta <- employees.site.Atlanta/employees.totals.Atlanta
SE.ratio.Seattle <- sqrt(employees.ratio.Seattle*(1-employees.ratio.Seattle)/nrow(Seattle))
SE.ratio.Atlanta <- sqrt(employees.ratio.Atlanta*(1-employees.ratio.Atlanta)/nrow(Atlanta))
#variance of sum is sum of variances
#variance of difference is sum of variances
SE.ratio.diff <- sqrt(SE.ratio.Seattle^2 + SE.ratio.Atlanta^2)
ratio.diff <- abs(employees.ratio.Seattle - employees.ratio.Atlanta)
z.diff <- ratio.diff/SE.ratio.diff
p.diff <- pnorm(z.diff,lower.tail=FALSE)
result.ratio <- t.test(Seattle$ratio,Atlanta$ratio)
print((result.ratio))
result.ratio <- t.test(Seattle$ratio,Boston$ratio)
print((result.ratio))
allcities <- rbind(Atlanta,Boston,Seattle)
allcities$PhysicalCity<- as.factor(allcities$`Physical City`)
str(allcities)
mod.ratio <- lm(ratio ~ PhysicalCity, data=allcities)
print(summary(mod.ratio))
boxplot(ratio ~ PhysicalCity, data=allcities)
aov.ratio <- aov(mod.ratio)
print(summary(aov.ratio))
################################################
#kmeans clustering
plot(allcities$`Employee this Site`,allcities$`Employee All Sites`,log='xy')
x=allcities$`Employee this Site`
y=allcities$`Employee All Sites`
df <- data.frame(x=log(x), y=log(y))
df$x=ifelse(df$x>6.5& df$y<11,df$x+5,df$x)
n=3
df <- df[-c(37,55),]
kobj <- kmeans(df,centers=n)
df$cluster <- kobj$cluster
cols <- rainbow(n)
plot(y ~ x, col=cols[cluster],data=df)
points(kobj$centers,col=cols,pch=8)


#hierarchical clusters
#dm <- dist(x=df[-c(37,55),c(1,2)])
dm <- dist(df)
hc <- hclust(dm)
plot(hc,col=cols[kobj$cluster])
require(dendextend)
dend <- as.dendrogram(hc)
dend2 <- color_labels(dend,col=cols[kobj$cluster])
plot(dend2)

##randomForest-need to tweak a few things and then update code
library(randomForest)

