
oring <- read.csv("oring.csv", header = TRUE)
attach(oring)
head(oring)

#建立简单线性回归
y<-ifelse(FAILURE=="Yes",1,0)
plot(TEMP,y,ylab="FAILURE")
lm1 <- lm(y ~ TEMP)
abline(lm1)


#建立逻辑回归
m1 <- glm(FAILURE ~ TEMP, family = binomial(link = "logit"))
plot(TEMP,y,ylab="probability",xlim=c(45,85))
invlogit <- function(x){exp(x)/(1 + exp(x))}
curve(invlogit(cbind(1,x)%*%coef(m1)),add=TRUE,lty=1)
m1

#ln(p/1-p) = 10.8753-0.1713*temp
# p = exp(10.8753-0.1713*temp) / 1+exp(10.8753-0.1713*temp)
# odds = p/1-p = exp(10.8753-0.1713*(temp+1))=exp(-0.1713)*exp(10.8753-0.1713*(temp))

m1$linear.predictors

m1$deviance
#-2* loglikelihood
predict(m1,newdata = data.frame(TEMP=60),type="response",se.fit = T)



##画出95%CI
templims=range(TEMP)
templims
temp.grid=seq(from=templims[1]-10,to=templims[2]+5,by=0.01)

preds=predict(m1,newdata=list(TEMP=temp.grid),se=T,type="response")
lines(temp.grid,preds$fit+2*preds$se.fit,lty=2)

preds=predict(m1,newdata=list(TEMP=temp.grid),se=T,type="link")
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

lines(temp.grid,se.bands[,1],lty=2,col="red")
lines(temp.grid,se.bands[,2],lty=2,col="red")


#介绍其他的linkfunction
plot(TEMP,y,ylab="probability",xlim=c(45,85))
templims=range(TEMP)
templims
temp.grid=seq(from=templims[1]-10,to=templims[2]+5,by=0.01)

#logit
m1 <- glm(FAILURE ~ TEMP, family = binomial(link = "logit"))
m1
lines(temp.grid,predict(m1,newdata=list(TEMP=temp.grid),type="response"))

#probit
m2 <- glm(FAILURE ~ TEMP, family = binomial(link = "probit"))
m2
lines(temp.grid,predict(m2,newdata=list(TEMP=temp.grid),type="response"),lty=2,col="red")

#cloglog
m3 <- glm(FAILURE ~ TEMP, family = binomial(link = "cloglog"))
m3
lines(temp.grid,predict(m3,newdata=list(TEMP=temp.grid),type="response"),lty=3,col="blue")


legend(75, 0.8, legend=c("logit", "probit","cloglog"),
       col=c("black","red", "blue"), lty=c(1:3), cex=0.8)

