#数据预处理
data<-student_data[,-1]
#删除列中有缺失值的数据
lm_data = na.omit(data)

#散点图可视化身高体重线性关系
attach(lm_data)
plot(height,weight ,main="scatter plot")

abline(h = mean(weight),col ="red")
abline(v = 170,col ="red")

abline(model,col="red")

#最小二乘法拟合简单线性回归
model =lm(weight~height,data=lm_data)
model$residuals
summary(model)

#根据summary表找出SSE和SST
anova(model)

#假设检验：身高和体重是否呈线性关系
#1） 区间估计法
t = -qt(0.025,df=model$df.residual)
c(0.903-t*0.134,0.903+t*0.134)

#2）p值法
(1-pt(6.740,df=model$df.residual))*2






#3）F值法
1-pf(45.426,df1 = 1,df2 = 260)

#估计：beta
#1）点估计
model$coefficients
#2）区间估计
summary(model)
t =-qt(0.025,df=260)
c(0.903-t*0.134,0.903+t*0.134)
confint(model)

#估计：y, 身高为165中国人的体重
#1）点估计
model$coefficients
-86.3177802+0.903*165
#2）区间估计
x = data.frame(height = c(165,180))
predict(model, newdata =x , interval = "predict")


#在散点图中绘制出回归模型的置信区间和预测区间
plot(height,weight ,main="scatter plot")
abline(model,col="red")

x = data.frame(height=seq(150,200,by = 1))
conf_interval = predict(model,newdata = x,interval = "predict")
conf_interval
lines(x$height, conf_interval[,2], col="blue", lty=2)
lines(x$height, conf_interval[,3], col="blue", lty=2)
