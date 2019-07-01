#1a
cig<-read.table(file="CIG.txt",header=T,sep="",as.is=T)
#1b
plot(cig$nicotine,cig$tar,main = "Relationship between nicotine levels and tar in canadian cigarettes",xlab="NICOTINE LEVELS(in mm)",ylab="TAR",col="dark blue",pch="O")
#1c
summary(cig$nicotine)
length(cig$nicotine)
mean(cig$nicotine)
sd(cig$nicotine)
hist(cig$nicotine)
error<-qt(0.975,df=length(cig$nicotine)-1)*sd(cig$nicotine)/sqrt(length(cig$nicotine))
error
left<-mean(cig$nicotine)-error
right<-mean(cig$nicotine)+error
left
right
## the 95% confidence interval of the mean concentration of nicotine is between 0.85 and 1.12. 
t.test(cig$nicotine)
#1d
hist(cig$tar)
mean(cig$tar)
length(cig$tar)
sd(cig$tar)
error1<-qt(0.975,df=length(cig$tar)-1)*sd(cig$tar)/sqrt(length(cig$tar))
error1
lft<-mean(cig$tar)-error1
ryt<-mean(cig$tar)+error1
lft
ryt
t.test(cig$tar)
#1e
#	What percentage of brands had nicotine concentrations above 1.0mm?
cig$nicotine
nic<- data.frame(cig[which(cig$nicotine>1.0),2])
nic
nic_pct<-nic/sum(nic)
nic_pct
#1f
range(cig$nicotine)
#1g
#g)Find the mean level of nicotine for brands with tar levels from 10 to 12 inclusive.
nic1<-cig$nicotine[which(cig$tar<=12 & cig$tar>=10)]
nic1
mean(nic1)
#1h
#h)	Find the mean level of tar for brands with nicotine levels from 0.95 to 1.35 inclusive?
tar1<-cig$tar[which(cig$nicotine>=0.95 & cig$nicotine<=1.35)]
tar1
mean(tar1)
#1i
set.seed(1)
nic.samp<-sample(cig$nicotine,10)
mean(nic.samp)
mean(cig$nicotine)
## similar mean for both the data sets.
#1j
##Do any brands have nicotine levels above 1.5mm?
nic2<-data.frame(cig$nicotine>1.5)
nic2
nic2<-cig[which(cig$nicotine>1.5),2]
any(cig$nicotine>1.5)

##no nicotine levels are above 1.5 mm.

#2

##error1<-qt(0.975,df=length(cig$tar)-1)*sd(cig$tar)/sqrt(length(cig$tar))
dr<-c(1,3,5,3,8,2,7,2,1,3)
sd(dr)
mean(dr)
length(dr)
error2<-qt(0.975,df=length(dr)-1)*sd(dr)/sqrt(length(dr))
error2
#lft<-mean(cig$tar)-error1
#ryt<-mean(cig$tar)+error1
lt<-mean(dr)-error2
lt
ryt<-mean(dr)+error2
ryt
salary<- c(500,10000,15000,20000,22000,25000,28000)
mean(salary)
median(salary)
range(salary)
max-min
max(salary)-min(salary)
nico<-cig[which(cig$nicotine>1),]
nico
nrow(nico)
pct<-nrow(nico)/nrow(cig)*100
pct
21/35*100




#HW4

alc<- c(1, 7, 5, 3, 8, 2, 7, 5, 1, 3 )
#1
mean(alc)
t.test(alc)

n<-9
x<-2
s<-1.414
red<-c(1,2,0,2,1,4,4,3,1)
mean(red)
m<-3
ts<-(x-m)/(s/sqrt(9))
ts

p.value<-2*pt(abs(ts),df=8,lower.tail=F)
p.value
pt

drinks<-c(1, 7, 5, 3, 8, 2, 7, 5, 1, 3 )
s<-2.415 
df<-9
sd(drinks)
length(drinks)
mean(drinks)
error<-qt(0.975,df=length(drinks)-1)*sd(drinks)/sqrt(length(drinks))
lft<-mean(drinks)+error
ryt<-mean(drinks)-error
lft
ryt
