cd<- c(388,418,413,407,464,412,408,393,414,421,413,391,423,396,410,403,406,412,417)
mean(cd)
sd(cd)
length(cd)
mean<-0.27
mu<-0.10
sd<-0.32
n<-106
sqrt(106)
0.32/10.29
0.17/0.031
df<-105
ts<- 5.4696
p.value<- 2*pt(abs(ts),df,lower.tail=F)
p.value
t.test(mu=0.10,alternative = "two.sided",paired = FALSE, var.equal = FALSE)

fev <- c(rnorm(106, mean = 0.27, sd = 0.32))
fev
t.test(fev,mu=0.10,alternative = "two.sided")
p.value<- 2*pt(abs(ts),df,lower.tail=F)
p.value

df= n-1
t.test
t.value <-(mean-mu) / (sd / sqrt(n) )
t.value                       
p.value = dt(t.value, df=length(n-1))
p.value
ts<- -2
p.value<- pt(-abs(ts),df=99,lower.tail = F)
p.value
15.34^2
18.23^2
(7*235.3)+(20*332.3)
8293.1/27
132.86-127.44
1/8+1/21
sqrt(307.15*0.172)
5.42/7.26
t<-0.74655
p.value<-2*pt(abs(t),df=27,lower.tail=F)
p.value
p.value=2*(1-pnorm(abs(z)))
z<-14.25
p.value

before<-c(90,78,20,72,40,55,60,54,63)
after<-c(93,77,23,80,55,60,62,57,70)
#a-
test<-t.test(before,after,var.equal = T)
test
var.before<-var(before)
var.after<-var(after)
var.pooled<-(8*var.before+8*var.after)/(9+9-2)
var.pooled
sd.pooled<-sqrt(var.pooled)
sd.pooled
ts<-(mean(before)-mean(after))/sd.pooled*sqrt(1/9+1/9)
ts<- -0.52367
p.value<-2*pt(ts,df=16)
p.value

##compare
test$stat
test$p.value
#b-
test.paired<-t.test(before,after,paired = T)
test.paired
diff= before-after
diff

##compare
test.paired$stat
test.paired$p.value
#c-
wilcox.test(before,after,paired = T,exact=T)
wilcox.test(before,after,paired = T,exact=F)
?test.paired







##hw 7- 
low<-data.frame(lowbwt$sex,lowbwt$apgar5)
low
boxplot(lowbwt$apgar5~ lowbwt$sex,main="boxplot of 5 minute apgar score in relation to gender",names=c("female", "male"),xlab="GENDER",ylab="APGAR SCORE",col=c("magenta","sky blue"))
male<- low[which(lowbwt$sex==1),]
female<- low[which(lowbwt$sex==0),]
wilcox.test(male$lowbwt.apgar5,female$lowbwt.apgar5,paired=F)
lowbwt


table(lowbwt$tox)
prop.table(table(lowbwt$tox))




before<-c(10, 12, 8, 6, 20, 18, 13, 7, 9, 11)
after<-c(8,  8,  3, 4, 7, 9,  3,  8,  5,  9)
t.test(before,after,paired = T)
diff<-c(-2,-4,-5,-2,-13,-9,-10,-1,-4,-2)
mean(diff)
wilcox.test(before,after, paired=T)
ks.test(before,after)

dollars<-c(10, 12, 8, 6, 20, 18, 13, 7, 9, 11)
length(dollars)
mean(dollars)
wilcox.test(dollars,mu=8,alternative = "two.sided")



before<-rnorm(n=10,mean=15,sd=2)
after<-rnorm(n=10,mean=11.5,sd=2)
t.test(before,after,paired=T,alternative="two.sided")
before
after
diff<-c(before-after)
diff
mean(diff)
sd(diff)
n=10
se<- sd(diff)/sqrt(n)
se
t<- mean(diff)/se
t
t.test(before,after,paired=T)


n1<- 9
xbar1<- 18.9
s1<-5.9
sd1<-sqrt(s1)
n2<-13
xbar2<-11.9
s2<-6.3
sd2<-sqrt(s2)
a<-rnorm(n1,xbar1,sd1)
b<-rnorm(n2,xbar2,sd2)
t.test(a,b,paired=F,alternative="two.sided")



z<-1.113
p<-2*(1-pnorm(abs(z)))
p

d<-rnorm(n=100,sd=15,mean = 118)
hist(d)
t<- -2.309
df=99
p<- 1-pt(abs(t),df)
p
1-0.9888
help(t.test)
t.test(d,mu=120,n=100,sd=15,alternative = "less")
after<- c(5,1,5,7,10,9,7,11,8)
before<- c(3,0,6,7,4,3,2,1,4)
t.test(before,after, paired=T,alternative = "two.sided")           
diff<- c(before-after)       
diff
sd(diff)
3.5/3
-3.666/1.1666



log(82.143)
sqrt(1/23+1/7+1/20+1/500)
1.96 * 0.4881
4.408 - 0.9566
23*507 
7*43
11661/301
?cor.test
sqrt(5/1-(0.996)^2)
0.996*2.001
cars
plot(cars, col='blue', pch=20, cex=2, main="Relationship between Speed and Stopping Distance for 50 Cars",
     xlab="Speed in mph", ylab="Stopping Distance in feet")
lm(cars$speed~cars$dist)


?lm()
cars
plot(cars, col='blue', pch=20, cex=2, main="Relationship between Speed and Stopping Distance for 50 Cars",
     xlab="Speed in mph", ylab="Stopping Distance in feet")
set.seed(122)
speed.c = scale(cars$speed, center=TRUE, scale=FALSE)
mod1 = lm(formula = dist ~ speed.c, data = cars)
summary(mod1)



before<-c(85,70,40,65,80,75,55,20)
after<-c(75,50,50,40,20,65,40,25)
wilcox.test(before,after,paired=T)
wilcox.test(before,after,paired=F)
?wilcox.test
hist(before)

h<- sqrt(86-before)
hist(h)

pnorm(120,mean=115,sd=15)
low<- rnorm(100,mean=115,sd=15)
low
hist(low)
#left skewed
ts=-2
p.value<- pt(abs(ts),df=99,lower.tail = F)
p.value
ts= 5.666
p.value<- 2*pt(abs(ts),df=105,lower.tail=F)
p.value
0.04*0.06/10000


before<-c(90,78,20,72,40,55,60,54,63)
after<-c(93,77,23,80,55,60,62,57,70)
diff<- c(before-after)
diff
sd(diff)
4.6/3
5/1.5333
exp(1.217)
exp(2.851)
