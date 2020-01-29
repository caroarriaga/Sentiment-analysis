library(dplyr)
library(rafalib)

df<- read.csv("prePostRes.csv", sep = ",")
names(df)[1]<-"Pre"

hist(df$Pre)
hist(df$Post)

pre<- na.omit(df$Pre) %>% unlist()
post<-na.omit(df$Post) %>% unlist()

mypar(2,2)
qqnorm(pre, main = "Pre, n=47")
qqline(pre)
qqnorm(post, main = "Post, n=34")
qqline(post)

#sample of population (total students = 29)
n<- 20

#distribution of pre
pre.dist<- replicate(10000, mean(sample(pre, n, replace = TRUE)))
pre.mean<-mean(pre.dist)
pre.sd<- sd(pre.dist)

#distribution of post
post.dist<- replicate(10000, mean(sample(post, n, replace = TRUE)))
post.mean<-mean(post.dist)
post.sd<- sd(post.dist)

qqnorm(pre.dist, main = "Pre, mean(n=20), rep =10000")
qqline(pre.dist)
qqnorm(post.dist, main = "Post, mean(n=20), rep =10000")
qqline(post.dist)

set.seed(134)
control<-sample(pre.dist, 100)
teat<- sample(post.dist,100)
tres<-t.test(teat, control, var.equal = FALSE)

teat.mean<-mean(teat)
teat.sd<- sd(teat)

control.mean<-mean(control)
control.sd<-sd(control)

N<-100
teat.qt<-abs(qt(.05, N-1))
teat.se<-teat.qt*teat.sd/sqrt(N)

control.qt<-abs(qt(.05, N-1))
control.se<-control.qt*control.sd/sqrt(N)

paste(c(control.mean, "+/-", control.se,"[",control.mean-control.se,",",control.mean+control.se,"]"), collapse = " ")
paste(c(teat.mean, "+/-", teat.se,"[",teat.mean-teat.se,",",teat.mean+teat.se,"]"), collapse = " ")

t.test(post, pre)

mypar(1,2)
boxplot(df$Pre, df$Post, 
        names = c("Pre","Post"),
        xlab="A", ylab="Scores",
        ylim = c(0,80))

boxplot(control, teat, 
        names = c("Pre","Post"),
        xlab="B", ylab="Scores",
        ylim = c(0,80))
