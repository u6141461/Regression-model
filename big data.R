set.seed(6141461)
#function to create n observations with p-dimensional variable X ∼ Unif([0, 1]^p)
uni.d.matrix<- function(n,p){
  replicate(p, runif(n,0,1))
}

#function to calculate the pair of distance
library(gtools)
distance.list<- function(n,p){
  X= uni.d.matrix(n,p)
  index.pair = permutations(n=100,r=2,v=1:100,repeats.allowed=F)
  pair.distance = c()
  for(i in 1:nrow(index.pair)){
    pair.distance[i] = dist(X[index.pair[i,],])
  }
  pair.distance
}


#main
p.equal.2 = distance.list(100,2)
p.equal.10 = distance.list(100,10)
p.equal.100 = distance.list(100,100)
p.equal.1000 = distance.list(100,1000)
df <- data.frame(
  p.equal.2,
  p.equal.10,
  p.equal.100,
  p.equal.1000
)
summary(df)

#ggplot2 to visualize
# Libraries
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

data <- df %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

data[1:9900,2] =df[,1]
data[9901:(9900*2),2] =df[,2]
data[(9900*2+1):(9900*3),2] =df[,3]
data[(9900*3+1):(9900*4),2] =df[,4]

picture <- ggplot(data=data, aes(x=value, group=text, fill=text)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
picture
