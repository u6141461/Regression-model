#读取数据，但建议采用第二种方法（import Dataset)
student_data <-read.csv("student_data.csv")
data<-student_data[,-1]

tail(data,10)
summary(data)
View(data)

#删除列中有缺失值的数据
data = na.omit(data)
summary(data)

#对身高进行直方图可视化 histogram
hist(data$height,col="red")
 
#用散点图可视化身高和体重的关系
plot(height,weight,data = data,main="scatter plot")

#对身高进行箱线图可视化
boxplot(height)

#对男女生身高分别进行箱线图可视化
boxplot(height~gender)

