#数据从这个ISLR包中获取
install.packages("ISLR")
library("ISLR")
View(Credit)
attach(Credit)

#建立回归模型
model = lm(Balance~.,data = Credit)

#1）逐步回归
#install.packages("leaps")
library(leaps)
regfit.full=regsubsets(Balance~.,data=Credit,method = "exhaustive",nvmax = 12)
reg.summary = summary(regfit.full)
summary(regfit.full)

#2）adjusted R^2
par(mfrow=c(1,2))
plot(reg.summary$rsq,xlab="Number of Variables",type="b",ylab="RSq")
plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adjusted RSq",type="b",ylim=c(0.953,0.954))
par(mfrow=c(1,1))
plot(regfit.full,scale="adjr2")
coef(regfit.full,id =7)

#3） BIC and Mellow'CP
plot(regfit.full,scale="bic")
coef(regfit.full,id =4)
which.min(reg.summary$cp)
coef(regfit.full,id =6)


#4）Validation selection
cv_set=sample(1:nrow(Credit),nrow(Credit)*3/4)
regfit.best=regsubsets(Balance~.,data=Credit[cv_set,],nvmax=12)
test.mat=model.matrix(Balance~.,data=Credit[-cv_set,])
val.errors=rep(NA,11)

for(i in 1:12){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Credit$Balance[-cv_set]-pred)^2)
}

which.min(val.errors)
coef(regfit.full,id=4)



#5）10-fold Validation selection
k=10
folds=sample(1:k,nrow(Credit),replace=TRUE)
out.cv = matrix(0, nrow = k, ncol = 12)
colnames(out.cv)=1:12

predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  coefi = coef(object, id)
  test.mat = model.matrix(form, newdata)
  pred = test.mat[ , names(coefi)]%*%coefi
  return(pred)
}

for (j in 1:k) {
  best.fit=regsubsets(Balance~.,data=Credit[folds!=j,],nvmax=12)
  for (i in 1:12) {
    pred = predict.regsubsets(best.fit, Credit[folds == j, ], i)
    MSE = mean((Credit$Balance[folds == j] - pred)^2)
    out.cv[j, i] = MSE
  }
}
out.cv
which.min(apply(out.cv, 2, mean))
coef(regfit.full,id=4)



# 6）Ridge and Lasso
gridLambda=10^seq(5,-2,length=100)
install.packages("glmnet")
library(glmnet)
train.validation.x=model.matrix(Balance~.,data=Credit)
train.validation.y=Credit$Balance
lasso.mod=cv.glmnet(train.validation.x,train.validation.y,alpha=0,lambda=gridLambda)
lasso.mod$cvm
#给定最佳的lambda值，算coefficient
bestlam=lasso.mod$lambda.min
predict(lasso.mod,s=gridLambda[1],type="coefficients")
mean(Balance)


# 7）降维 PCR
library(pls)
pcr.fit <- pcr(Balance~.,data=Credit,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
predict(pcr.fit,newdata = Credit[1:5,],ncomp=11)



# 8）降维 PLS
library(pls)
set.seed(1)
pls.fit <- plsr(Balance~.,data=Credit,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
predict(pls.fit,newdata = Credit[1:10,],ncomp=4)

