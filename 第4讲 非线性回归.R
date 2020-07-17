nonlinear.student.data <-  read.csv("~/Desktop/回归分析/第4讲 非线性回归/student_data.csv", row.names=1)
nonlinear.student.data<- na.omit(nonlinear.student.data)
head(nonlinear.student.data)

#散点图可视化身高体重线性关系
attach(nonlinear.student.data)
plot(height,weight ,main="scatter plot")




#1）多项式回归模型建模
plot(height,weight ,main="scatter plot")
poly.model = lm(weight~height+I(height^2)+I(height^3))
#第二种方法：lm(weight~poly(height,100,raw=TRUE))
poly.model

#可视化多项式回归
x <- seq(150,200,by=0.1)
lines(x,predict(poly.model,newdata = data.frame(height = x)),col = "red")

#置信区间
x <- seq(150,200,by=0.1)
conf_interval = predict(poly.model,data.frame(height = x),interval = "confidence")
lines(x, conf_interval[,2], col="blue", lty=2)
lines(x, conf_interval[,3], col="blue", lty=2)

#预测区间
predict_interval = predict(poly.model,data.frame(height = x),interval = "predict")
lines(x, predict_interval[,2], col="blue", lty=2)
lines(x, predict_interval[,3], col="blue", lty=2)

#如何判断polynomial degree
anova(poly.model)



#2）把身高看成分类型变量回归
cut(height,3)
step.regression = lm(weight~cut(height,3))
step.regression
#可视化分类型变量回归
plot(height,weight ,main="scatter plot")
lines(sort(height),sort(step.regression$fitted.values),col = "red",lty=2,lwd=2)



#3）多分类变量回归
plot(height,weight ,main="scatter plot")
multi.factor.lm = lm(weight~cut(height,3)+gender)
multi.factor.lm
#可视化多分类变量回归




#4）分类型数值型混合回归
mix.model = lm(weight~height+gender)
mix.model
#可视化混合回归
plot(height,weight ,main="scatter plot")
x <- seq(150,200,by=0.1)
lines(x,predict(mix.model,newdata = data.frame(height = x,gender="Male")),col = "red",lty=2,lwd=2)
lines(x,predict(mix.model,newdata = data.frame(height = x,gender="Female")),col = "Blue",lty=2,lwd=2)
legend(180,140,legend = c("Male","Female"), col=c("red", "blue"),lty=c(2,2))


#5）分类型数值型混合回归（含交互作用）
mix.model = lm(weight~height+gender+height:gender)
mix.model
#可视化混合回归
plot(height,weight ,main="scatter plot")
x <- seq(150,200,by=0.1)
lines(x,predict(mix.model,newdata = data.frame(height = x,gender="Male")),col = "red",lty=2,lwd=2)
lines(x,predict(mix.model,newdata = data.frame(height = x,gender="Female")),col = "Blue",lty=2,lwd=2)
legend(180,140,legend = c("Male","Female"), col=c("red", "blue"),lty=c(2,2))

