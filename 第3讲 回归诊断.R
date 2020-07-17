#数据预处理
data<-student_data[,-1]
#删除列中有缺失值的数据
lm_data = na.omit(data)


#散点图可视化身高体重线性关系
attach(lm_data)
plot(height,weight ,main="scatter plot")
abline(model,col="red")


#最小二乘法拟合简单线性回归
model =lm(weight~height,data=lm_data)


#residual plot（残差图）
par(mfrow = c(1,2))
plot(model$fitted.values,model$residuals)
abline(h=0,col="red")
plot(model,which=1)

#Scale-Location Graph 位置尺度图
plot(model$fitted.values,sqrt(abs(scale(model$residuals))))
plot(model,which=3)




#QQ图
hist(weight)
hist(log(weight))

qqnorm(weight)
qqline(weight)
plot(model,which=2)


#异常点处理
plot(model,which=4)
plot(model,which=5)
