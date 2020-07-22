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


#预测
model
x = data.frame(sqft.living=1000, bedrooms=3, bathrooms=3)
exp(predict(model,x,interval="confidence"))


#1）pearson 相关系数 处理多重共线性
cor(multi.data)


#2）VIF 方差膨胀系数(variance inflation factor)
partial.regrs =lm(sqft.living ~ bedrooms+bathrooms,data=multi.data )
summary(partial.regrs)
1/(1-0.6281)
#install.packages("car")
library(car)
model
vif(model)

#3)  偏残差图
par(mfrow=c(1,3))
#判断X变量和Y的线性关系
plot(multi.data$bedrooms,log(multi.data$price))
plot(multi.data$bathrooms,log(multi.data$price))
plot(multi.data$sqft.living,log(multi.data$price))
#判断在bedrooms变量和bathrooms存在的情况下，sqft.living和log(price)是否呈线性关系
par(mfrow=c(1,1))
#sqft.living为X轴，不含sqft.living变量拟合出回归模型的残差为Y轴，做散点图
partial.model = lm(log(price)~bathrooms+bedrooms,data=multi.data)
plot(multi.data$sqft.living,partial.model$residuals)

#偏残差图的包
#install.packages("car")
library(car)
model = lm(log(price)~sqft.living+bedrooms,data = multi.data)
crPlots(model)

