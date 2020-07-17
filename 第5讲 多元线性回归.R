multi.data = housing_data[,-1]
#可视化数据
plot(multi.data)
cor(multi.data)

#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(multi.data, histogram=TRUE, pch=19)


#建立多元线性回归模型
model = lm(log(price)~sqft.living+bedrooms+bathrooms,data = multi.data)
model
#回归诊断
par(mfrow=c(2,2))
plot(model)
hist(log(price))


#解释多元线性回归模型
model
exp(12.32446)

#解释beta
par(mfrow=c(1,1))
bathrooms <- 1:10
bedrooms <- rep(mean(multi.data$bedrooms), 10) 
sqft.living <- rep(mean(multi.data$sqft.living), 10)
dat.new <- data.frame(bedrooms, bathrooms, sqft.living)
plot(bathrooms, exp(predict(model, newdata=dat.new)), type="l", lwd=3, ylab="price", xlab="bedrooms", cex.lab=2)

#研究X变量是否都显著
summary(model)
anova(model)


#overall F test:
MSR = (882.53+10.40+4.38)/3
MSE = 1042.99/6976
F = MSR/MSE
1-pf(F,df1=3,df2=6976)
