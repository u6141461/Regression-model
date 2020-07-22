savings

#建立回归模型
mod = lm(SavingsRate~Pop15+Pop75+DPIgrowth,data=savings)
anova(mod)



#1）高杠杆点 high leverage point
hatv <- hatvalues(mod)
plot(hatv, type="h", ylab="Leverages")
#标出界限
p <- 4
n <- nrow(savings)
abline(h=(2*p/n), lwd=2, col="red") 
#标出高杠杆点
countries <- row.names(savings)
text(c(1:50)[hatv>(2*p/n)], hatv[hatv>(2*p/n)],savings$Country[hatv>(2*p/n)] , col="dodgerblue", cex=1.5)




#2）异常点 outlier
par(mfrow=c(1,2))
plot(mod,which=1)
plot(mod$fitted.values,rstandard(mod))
abline(h = 0, col="red")
#标出 outlier 
countries <- row.names(savings)
text(mod$fitted.values[abs(rstandard(mod))>2], rstandard(mod)[abs(rstandard(mod))>2],savings$Country[abs(rstandard(mod))>2] , col="dodgerblue", cex=1)
#根据标准QQ图找出异常点
par(mfrow=c(1,1))
plot(mod,which=2)



#3）cook distance找强影响点（high influential point）
cooks.distance(model)
plot(cooks.distance(model), type="h", ylab="cook.distance")
#标出影响点
countries <- row.names(savings)
text(c(1:50)[which.max(cooks.distance(model))],cooks.distance(model)[which.max(cooks.distance(model))], savings$Country[which.max(cooks.distance(model))] , col="dodgerblue", cex=1)
#用R语言内置函数画cook distance
plot(model,which=4)




#4）斜率的改变率 找强影响点（high influential point）
par(mfrow=c(2,2))
#intercept change percentage
intercept.change = dfbeta(mod)[,1]/coef(mod)[1]*100
plot(intercept.change, type = "h", ylab="intercept", pch=16, cex.lab=1.5)
abline(h=0)
text( c(1:50)[which.max(abs(intercept.change))],intercept.change[which.max(abs(intercept.change))],savings$Country[which.max(abs(intercept.change))], col="dodgerblue", cex=1)

#Pop15 change percentage
Pop15.change = dfbeta(mod)[,2]/coef(mod)[2]*100
plot(Pop15.change, type = "h", ylab="Pop15", pch=16, cex.lab=1.5)
abline(h=0)
text( c(1:50)[which.max(abs(Pop15.change))],Pop15.change[which.max(abs(Pop15.change))],savings$Country[which.max(abs(Pop15.change))], col="dodgerblue", cex=1)

#Pop75 change percentage
Pop75.change = dfbeta(mod)[,3]/coef(mod)[3]*100
plot(Pop75.change, type = "h", ylab="Pop75", pch=16, cex.lab=1.5)
abline(h=0)
text( c(1:50)[which.max(abs(Pop75.change))],Pop75.change[which.max(abs(Pop75.change))],savings$Country[which.max(abs(Pop75.change))], col="dodgerblue", cex=1)


#DPIgrowth change percentage
DPIgrowth.change = dfbeta(mod)[,4]/coef(mod)[4]*100
plot(DPIgrowth.change, type = "h", ylab="DPIgrowth", pch=16, cex.lab=1.5)
abline(h=0)
text( c(1:50)[which.max(abs(DPIgrowth.change))],DPIgrowth.change[which.max(abs(DPIgrowth.change))],savings$Country[which.max(abs(DPIgrowth.change))], col="dodgerblue", cex=1)


