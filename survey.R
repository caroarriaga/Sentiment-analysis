library(dplyr)
library(rafalib)

dat<- read.csv("Survey-Final-project-Groups.csv")

#distribution of each factorr without NA
project <-na.omit(dat$Project %>% unlist())
theme <-na.omit(dat$Theme %>% unlist())
content<-na.omit(dat$Content %>% unlist())

#sample of population (total students = 29)
n<- 10

#distribution of project
p.dist<- replicate(300, mean(sample(project, n, replace = TRUE)))
p.mean<-mean(p.dist)
p.sd<- sd(p.dist)

#distribution of theme
t.dist<- replicate(300, mean(sample(theme, n, replace = TRUE)))
t.mean<-mean(t.dist)
t.sd<- sd(t.dist)

#distribution of content
c.dist<- replicate(1000, mean(sample(content, n, replace = TRUE)))
c.mean<-mean(c.dist)
c.sd<- sd(c.dist)

#qt requires right tail value and degrees of freedom - CI 95%
#project
p.qt<-abs(qt(.05, n-1))
p.se<-p.qt*p.sd/sqrt(n) 
#theme
t.qt<-abs(qt(.05, n-1))
t.se<-t.qt*t.sd/sqrt(n) 
#content
c.qt<-abs(qt(.05, n-1))
c.se<-c.qt*c.sd/sqrt(n) 

mypar(3,2)
#project
qqnorm(p.dist)
qqline(p.dist)
hist(p.dist)

#theme
qqnorm(t.dist)
qqline(t.dist)
hist(t.dist)

#Content
qqnorm(c.dist)
qqline(c.dist)
hist(c.dist)

paste(c(p.mean, "+/-", p.se,"[",p.mean-p.se,",",p.mean+p.se,"]"), collapse = " ")
paste(c(t.mean, "+/-", t.se,"[",t.mean-t.se,",",t.mean+t.se,"]"), collapse = " ")
paste(c(c.mean, "+/-", c.se,"[",c.mean-c.se,",",c.mean+c.se,"]"), collapse = " ")

req.dat<- dat$Requirements.list %>% unlist()
pr.req<-sum(req.dat == "Yes")/length(req.dat)
pr.req

dec.dat<- dat$Decision.Matrices %>% unlist()
pr.dec<-sum(dec.dat == "Yes")/length(dec.dat)
pr.dec

map.dat <- dat$Thought.maps %>% unlist()
pr.map <- sum(map.dat == "Yes")/length(map.dat)
pr.map

#two lists to be converted into a data frame
tool<- c('Requirement list', 'Decision matrix', 'Thought map')
usage<- c(.93, .93, .72)

tool_use<-do.call(rbind.data.frame, Map('c', tool, usage))
tool_use<-as.data.frame.matrix(tool_use) 
barplot(tool_use$usage, tool_use$tool,
        ylab = "Percentage")
