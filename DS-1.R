library(HSAUR2)

dim(household)
head(household)
hs <- data.frame(household)
View(hs)
summary(hs)
boxplot(hs$housing, hs$food, hs$goods, hs$service, horizontal = TRUE)

library(ggplot2)
boxplot(hs)


?boxplot

#a)
x=household$goods
mean(x)
var(x)
sd(x)
median(x)
min(x)
max(x)
quantile(x,c(0.25,0.5,0.75)) # kwartyle
quantile(x,seq(0.2,0.8,by=0.2)) # kwintyle
quantile(x,seq(0.1,0.9,by=0.1)) # decyle
max(x)-min(x) 
IQR(x)

library(fBasics)
kurtosis(x) 
skewness(x) 
mean(x, trim=0.1)
library(psych)
winsor.means(x,trim=0.1)

# b)
boxplot(x)
box=boxplot(x, horizontal = T)
box$stats
box$out

?boxplot
?boxplot.stats

# c)
h=hist(x)
h=hist(x,prob=T)


h$mids
h$counts
h$breaks
h$density

# d)
x0i=h$mids # srodki przedzialow klasowych
ni=h$counts # licznosci klas
# Srednia dla szeregu rozdzielczego:
 (sred=sum(x0i*ni)/sum(h$counts))

mean(household$goods)

?household

#Wariancja dla szeregu rozdzielczego:
 (war=sum(ni*(x0i-sred)^2)/(sum(h$counts)-1))

#odchylenie standardowe dla szeregu rozdzielczego:
(odch=sqrt(war))

sd(household$goods)

# zadanie 2
library(alr3)
View(banknote)
head(banknote)

#a)
par(mfcol=c(1,1))
boxplot(banknote$Diag)
hist(banknote$Diag,prob=T)

#b)
box=boxplot(banknote$Diagonal~banknote$Y, horizontal=T)

?boxplot

box$stats

box$out
box$group
?group
box$out[box$group==1] # outliery przy Y=0
box$out[box$group==2] # outliery przy Y=1
boxplot(banknote$Diagonal[banknote$Y==0])$out # dla Y=0
boxplot(banknote$Diagonal[banknote$Y==1])$out # dla Y=1

boxplot(banknote$Diagonal[banknote$Y==0], banknote$Diagonal[banknote$Y==1], horizontal=T)$out

par(mfcol=c(2,1))
hp=hist(banknote$Diagonal[banknote$Y==0])

hf=hist(banknote$Diagonal[banknote$Y==1])

#c)
quantile(banknote$Diag[banknote$Y==1], 0.9)
quantile(banknote$Diag[banknote$Y==0], 0.1)



banknot <- data.frame(banknote)
b <- banknot %>% group_by(Y) %>% summarise(dec1=quantile(Diagonal, 0.1))
b

library(HSAUR2)

dim(Forbes2000)
head(Forbes2000)
View(Forbes2000)

mean(Forbes2000$profits)
mean(Forbes2000$profits, na.rm=T)
mean(na.omit(Forbes2000$profits))

tapply(Forbes2000$profits,Forbes2000$country,mean, na.omit=T)

forbes2000 <- data.frame(Forbes2000)
pokrajach <- forbes2000 %>% group_by(country) %>% summarise(avg_prof=mean(profits, na.rm=TRUE))
View(pokrajach)

summary(Forbes2000$profits)
