# ---- 23102016 ----

install.packages("HSAUR2")

library(HSAUR2)


dim(household)
head(household)
head(household,50)
tail(household)
?household

summary(household)

??household

x=household$goods
summary(x)
mean(x)
var(x)
var(x)^0.5
sd(x)
min(x)
max(x)
quantile(x,c(0.25,0.5,0.75)) # kwartyle
quantile(x,seq(0, 1, by=0.2)) #kwintyle
quantile(x,seq(0 , 1, by=0.1)) #decyle
max(x)-min(x)
range(x)
IQR(x)

library(fBasics)

kurtosis(x)
skewness(x)
mean(x, trim=0.1)
library(psych)
winsor.means(x, trim=0.1)
mean(x)

??winsor

# wykresy
#pudelkowy
boxplot(x)
boxplot(x,horizontal=T)
box=boxplot(x)
box
box$stats
summary(x)

?box
?box$stats
?boxplot

boxplot(household$goods)$stats


box$out
#histogram
h=hist(x)
h
h=hist(x,prob=T)
h$mids
h$counts
h$breaks
h$density
7.5e-04+1.5e-04+5.0e-05+0.0e+00+0.0e+00+2.5e-05+2.5e-05

?density

par(mfrow=c(2,2))

hist(x,prob=T, breaks="Sturges")
hist(x,prob=T, breaks=8)
hist(x,prob=T, breaks="Scott")
hist(x,prob=T, breaks="FD")
??breaks

# przeksztalcanie wykresow
h=hist(x,labels=T)
par(mfrow=c(1,1))
h=hist(x,prob=T,labels=T)

h$mids
h$counts
h$breaks
attributes(h)
fivenum(x)

#d)
x0i=h$mids #srodki przedzilow
ni=h$counts # licznosc klas
h

#srednia arytmetyczna dla szeregu rozdzielczego

sr=sum(h$mids*h$counts)/length(x)
sr
mean(x)
#wariancja
var=sum((h$counts*(h$mids-sr)^2))/(length(x)-1)
var
sd=sqrt(var)
sd
sd(x)

#bez outlierow
box2$out[box2$out[box2$group==1]]>box2$stats[5,1]

#boxy na wykresie - wiecej

box2=boxplot(x~household$gender)
box2
box2$out[box2$group==2]

par(mfrow=c(1,1))

# tablice cross
tapply(x,household$gender,mean)
tapply(x, y, FUN)
mean(x[household$gender=='female'])

# wykresy na filtrze 
tapply(x,household$gender,hist)
hist(x[household$gender=='female'],xlim=c(0,7000))
hist(x[household$gender=='male'],xlim=c(0,7000))

# 90% mczyzn kobiet

quantile(x[household$gender=='female'],0.9)
quantile(x[household$gender=='male'],0.1)

#forbes 2000

dim(Forbes2000)
head(Forbes2000)

mean(Forbes2000$profits)
summary(Forbes2000)

# mean(x,na.rm=T)
# mean(na.omit(x))

?na.omit

x=Forbes2000$pr
y=na.omit(x)
y
mean(y)
(y=10*x)

#przelam po krajach
tapply(x,Forbes2000$country,mean)
tapply(x,Forbes2000$category,mean)

table(Forbes2000$category)
?table

#losowanie z rozkladu normalnego

par(mfrow=c(2,2))

x1=rnorm(50,-4,2)
x2=rnorm(1000,-4,2)
x3=rnorm(10000,-4,2)
x4=rnorm(500000,-4,2)

hist(x1,prob=T)
curve(dnorm(x,-4,2),add=T,col=2)

hist(x2,prob=T)
curve(dnorm(x,-4,2),add=T,col=2)

hist(x3,prob=T)
curve(dnorm(x,-4,2),add=T,col=2)

hist(x4,prob=T)
curve(dnorm(x,-4,2),add=T,col=2)

#dystrybuanta

plot(ecdf(x1))
curve(pnorm(x,-4,2),add=T,col=3)

plot(ecdf(x2))
curve(pnorm(x,-4,2),add=T,col=3)

plot(ecdf(x3))
curve(pnorm(x,-4,2),add=T,col=3)

plot(ecdf(x4))
curve(pnorm(x,-4,2),add=T,col=3)

# zadanie drugie (banknoty)

par(mfrow=c(2,2))

library(alr3)

summary(banknote)

d=banknote$Diagonal
b=boxplot(d)
b
h=hist(d, prob=T)
h
tapply(d,banknote$Y,hist)
tapply(d,banknote$Y,boxplot)

# nie wiem jak wyznaczyć te outliery na filtrze - w zbiorze pokazuje zero

b$out
?banknote
print(banknote)



quantile(d[banknote$Y==1],0.9)
quantile(d[banknote$Y==0],0.1)

# ---- 06112016 ----

(0.85*0.9)/0.8

#Narysowa? g?sto?ci i dystrybuanty nast?puj?cych rozk?ad?w normalnych: 0,1, -2,1, 1,1,0,2, 0, 1/2. Prze?ledzi? wp?yw parametr?w rozk?adu na wygl?d g?sto?ci.

#gestosci
curve(dnorm(x,0,1)) # ups, niewiele widac - zmieniamy zakres x i y
curve(dnorm(x,0,1),xlim=c(-6,5),ylim=c(0,0.8),main="gestosci normalne")
curve(dnorm(x,-2,1),add=T,col=2)
curve(dnorm(x,1,1),add=T,col=3)
curve(dnorm(x,0,2),add=T,col=4)
curve(dnorm(x,0,1/2),add=T,col=5)
curve(dnorm(x,0,1/3),add=T,col=66)

?curve
??col # zobacz w ksiazce biecka

# dystrybuanty:
# zmieniamy zakres x (y i tak bedzie w przedziale (0,1))
curve(pnorm(x,0,1),xlim=c(-6,5), main="dystrybunaty r normalnego")
curve(pnorm(x,-2,1),add=T,col=2)
curve(pnorm(x,1,1),add=T,col=3)
curve(pnorm(x,0,2),add=T,col=4)
curve(pnorm(x,0,1/2),add=T,col=5)

# W DOMU FUNKCJE --> legend - dodawanie legendy do wykresu, pdf("rys 1.pdf") - eksport do pliku


#ZADANIE 2
#Niech  b?dzie zmienn? losow? o rozk?adzie normalnym 0,1.
#a) Obliczy? nast?puj?ce prawdopodobie?stwa:
# -1 <  < 1
#-2 <  < 2
#-3 <  < 3
#b) Wygenerowa? 10 000 obserwacji ,?,  z rozk?adu normalnego 0,1 Wyznaczy? cz?sto?ci nast?puj?cych zdarze?:
# = :  ??? -1,1
# = :  ??? -2,2
# = :  ??? -3,3
#Por?wna? otrzymane cz?sto?ci z prawdopodobie?stwami z punktu a).

# a)
# pnorm(1)-pnorm(-1) # to samo co pnorm(1,0,1)-pnorm(-1,0,1)
2*(pnorm(1))-1 
pnorm(2)-pnorm(-2)
pnorm(3)-pnorm(-3)

# b)
nor=rnorm(10000) #b to samo co rnorm(10000,0,1)
n1=nor[nor>-1 & nor<1]
n2=nor[nor>-2 & nor<2]
n3=nor[nor>-3 & nor<3]
length(n1)/10000
length(n2)/10000
length(n3)/10000

#ZADANIE 3
#Niech X bedzie zmienna losowa o rozk?adzie Cauchy?ego C(0; 1).
#a) Obliczyc nastepujace prawdopodobienstwa:
#P(????1 < X < 1);
#P(????2 < X < 2);
#P(????3 < X < 3):
#Por?wnac otrzymane prawdopodobienstwa z tymi uzyskanymi dla zmiennej o rozk?adzie standardowym normalnym (patrz poprzednie zadanie).
#b) Wygenerowac 10000 obserwacji X1; : : : ;X10000 z rozk?adu Cauchy?ego C(0; 1). Wyznaczyc czestosci nastepujacych zdarzen:
#A = fXi : Xi 2 (????1; 1)g;
#B = fXi : Xi 2 (????2; 2)g;
#C = fXi : Xi 2 (????3; 3)g:
#Por?wnac otrzymane czestosci z obliczonymi wczesniej prawdopodobienstwami

# a)
curve(dnorm(x,0,1),xlim=c(-6,6),ylim=c(0,0.8),main="gestosci normalne")
curve(dcauchy(x,0,1),add=T,col=2)

pcauchy(1,0,1)-pcauchy(-1,0,1)
# lub
2*(pcauchy((-1,0,1))))-1
pcauchy(2,0,1)-pcauchy(-2,0,1)
pcauchy(3,0,1)-pcauchy(-3,0,1)
# b)
cau=rcauchy(10000,0,1)
c1=cau[cau>-1 & cau<1]
c2=cau[cau>-2 & cau<2]
c3=cau[cau>-3 & cau<3]
length(c1)/10000
length(c2)/10000
length(c3)/10000
# c) porownac r normalny i cauchego
curve(dnorm(x),xlim=c(-6,6))
curve(dcauchy(x,0,1),col=3,add=T)

#ZADANIE 4
#Wyznaczyc
#a) kwantyl rzedu 0.95 rozk?adu standardowego normalnego,
#b) kwantyl rzedu 0.99 rozk?adu normalnego N(????4; 2);
#c) kwantyl rzedu 0.9 rozk?adu t-Studenta o 20 stopniach swobody,
#d) kwantyl rzedu 0.95 rozk?adu chi-kwadrat o 5 stopniach swobody
#e) kwantyl rzedu 0.9 rozk?adu F-Snedecora o 3 stopniach swobody licznika i 12 stopniach swobody mianownika.

# a)
qnorm(0.95)
# b)
qnorm(0.99, -4, 2)
# c)
qt(0.9,20)

?t

# d)
qt(0.95, 5)
# d)
qchisq(0.95,5)
# )
qchisq(0.975,20)
# f)
qf(0.9,3,12)
# )
qf(0.95,7,20)

#zad4

#a

1 - pnorm(130,120,20)

#b


#ZADANIE 5
#Przyjmuje sie, ze d?ugosc skoku na Wielkiej Krokwi ma rozk?ad normalny N(120; 20):
#a) Obliczyc prawdopodobienstwo, ze skok bedzie mia? d?ugosc przekraczajaca 130 m.
#b) Wyznaczyc wartosc, ponizej kt?rej plasuje sie 85% skok?w.
#c) Wyznaczyc wartosc, powyzej kt?rej skacze 75% zawodnik?w.


#odtad nie wiem co to
# a)
curve(dnorm(x, 0.0018, 0.01), xlim=c(-0.1, 1.1))
curve(dlnorm(x, 0.0018, 0.01),add=T) # te dwie g?sto?ci na jednym rysunku
dobrze nie wygl?daj?
# Narysujmy wi?c razem tylko g?sto?ci rozk?ad?w normalnego i Cauchy'ego:
curve(dnorm(x, 0.0018, 0.01), xlim=c(-0.1,0.1))
curve(dcauchy(x, 0.0018, 0.01),add=T, col=3)
abline(v=0.0018, col=6) # dorysowujemy linie o r-niu x=0.0018, nie ma tu add=T !!!
# oddzielnie rysyjemy g?sto?? dla rozk?adu lognormalnego:
curve(dlnorm(x, 0.0018, 0.01), xlim=c(0.8,1.2))
# b)
# i
# dla N
pnorm(0.007,0.0018, 0.01)
#dla LN
plnorm(0.007,0.0018, 0.01)
# dla C
pcauchy(0.007,0.0018, 0.01)
# ii
# dla N
1-pnorm(0.0002,0.0018, 0.01)
#dla LN
1-plnorm(0.0002,0.0018, 0.01)
# dla C
1-pcauchy(0.0002,0.0018, 0.01)
# iii
# dla N
pnorm(0.004,0.0018, 0.01)-pnorm(-0.005,0.0018, 0.01)
#dla LN
plnorm(0.004,0.0018, 0.01)-plnorm(-0.005,0.0018, 0.01)
# dla C
pcauchy(0.004,0.0018, 0.01)-pcauchy(-0.005,0.0018, 0.01)
# c)
# dla N
qnorm(0.85,0.0018, 0.01)
#dla LN
qlnorm(0.85,0.0018, 0.01)
# dla C
qcauchy(0.85,0.0018, 0.01)
# d)
# dla N
qnorm(0.25,0.0018, 0.01)
#dla LN
qlnorm(0.25,0.0018, 0.01)
# dla C
qcauchy(0.25,0.0018, 0.01)

#dotad nie wiem


#ZADANIE 6
#Producent dach?wek szacuje, ze jego reklama dotrze do mieszkanca pewnego regionu z prawdopodobienstwem 0.18. Jezeli sposr?d mieszkanc?w tego regionu wylosujemy 14
#os?b, jakie jest prawdopodobienstwo, ze reklama dotar?a do
#a) co najmniej szesciu z nich?
#b) dok?adnie pieciu z nich?
#c) wiecej niz dw?ch, ale mniej niz osmiu?
# X - liczba mieszka?c?w, do kt?rych reklama dotar?a
# X ma rozk?ad bin(n=14, p=0.18)
# P(X>=6)=1-P(X<6)=1-P(X<=5)
1-pbinom(5,14,0.18)
# P(X=5)
dbinom(5,14,0.18)
#P(2<x<8)
pbinom(7,14,0.18)-pbinom(2,14,0.18)


#ZADANIE 7
#Czas pracy baterii w czytniku pewnej firmy ma rozk?ad wyk?adniczy ze srednia r?wna 100 [h].
#a) Obliczyc prawdopodobienstwo, ze bateria nie roz?aduje sie w ciagu: 50, 100, 150 godzin pracy.
#b) Na ile godzin czytania powinna wystarczyc taka bateria z prawdopodobienstwem 0.95?

# X-czas poprawnej pracy; EX=100=1/lambda, czyli lambda=0.01
# a) P(X>t)gdzie t=50, 100, 150
1-pexp(50,0.01)
1-pexp(100,0.01)
1-pexp(150,0.01)
# b) P(X>c)=0.95, czyli P(X<=c)=0.05, zatem c - kwantyl rz?du 0.05 rozkladu X
qexp(0.05,0.01)

#ZADANIE 8
#Liczba wizyt w ciagu minuty na stronie pewnej ksiegarni internetowej ma rozk?ad Poissona z parametrem 8. Jakie jest prawdopodobienstwo, ze strona ta bedzie mia?a ponad
#16 ods?on w ciagu minuty?

1 - ppois(16,8)

#z9
#a1 wykladniczy lambda=3 (Ex=1/3)

N=10000
n=1:N
lambda=1/3
X=rexp(N,lambda)
S=cumsum(X)
M=S/n
plot(n, M, type='l')
abline(h=1/lambda, col=3)

M[1:20]

M[1:100]

#a2
set.seed(125) #ustawienie gen liczb losowych

nmax=10000
n=1:nmax
X=rcauchy(nmax)
S=cumsum(X)
M=S/n
plot(n, M, type='l')
abline(h=0, col=3)

#zad10

N=100000
n=100
lam=0.25
S=replicate(N,sum(rexp(n,lam))) # zamiast petli powtarza 10000 generowanie 100 zsumowanych obserwacji z rozkladu wykladniczego
nS=(S-n/lam)/(sqrt(n)/lam) #ciag unormowanych sum
hist(nS,prob=T,br='Scott')
curve(dnorm(x,mean=0,sd=1), col=4, add=T)

#zad11

n=10000
X=runif(n)
Y=sqrt(X*(1-X))
mean(Y)

?runif

n=10000
X=runif(n, 1, 2)
Y=sqrt(log(X))
mean(Y)


#z8dod
#a

n=10000
X=runif(n, pi/2, pi)
Y=pi/2*cos(X)/X
sum(Y)/n

#b

n=10000
X=rexp(n)
Y=X^1.5
sum(Y)/n

gamma(5/2)

#z12

#a

ppois(6,8)-ppois(1,8)
pbinom(6,400,0.02)-pbinom(1,400,0.02)

#b

pnorm(6,8,sqrt(400*0.02*0.98))-pnorm(1,8,sqrt(400*0.02*0.98))
pnorm(6.5,8,sqrt(400*0.02*0.98))-pnorm(1.5,8,sqrt(400*0.02*0.98))

#z13
#tw linderberga levego i podstawic dobrze wartosci (co jest co)


## ---- 20112016 ----

library(lattice)


#zad4
T <- ( ( (mean(barley$yield)-32) ) /(sd(barley$yield))*sqrt(120))
T


t.test(barley$yield, mu=32, alt="greater", conf.level = .99)

t.test(barley$yield, mu=32, alt="greater", conf.level = .99)



#zad1
#a
prop.test(29,250,conf.level=0.98)
prop.test(29,250,conf.level=0.98)$conf.int

?prop.test

binom.test(29,250,conf.level=0.98)
binom.test(29,250,conf.level=0.98)$conf.int

#b
binom.test(29,250,0.1,alt="greater")
prop.test(29,250,0.1,alt="greater", corr=F) #corr poprawka naciaglosc

#zad2

prop.test(c(37,29),c(22800,20300), alt="greater")

#zad3
#a
binom.test(118, 150,p=0.8)

#b
binom.test(118, 150,alt="two.sided")

#zad5

library(HSAUR2)
data <- data.frame(Forbes2000$category=='Insurance')#zle

x=Forbes2000$profits[Forbes2000$category=='Insurance']
#summary(Forbes2000)
#x<- data$profits

mean(x)
t.test(x,conf.level=0.9)$conf.int


x2 <- na.omit(x)
library(TeachingDemos)
sigma.test(x2,conf.level=0.9)$conf.int

#zad6


library(lattice)

#a
x <- environmental$temperature

t.test(x,conf.level=0.99)$conf.int


#b

t.test(x,mu=80,alt="less")

#c
sqrt(sigma.test(x,conf.level=0.97)$conf.int)

#d
sigma.test(x,sigmasq=85,alt='greater') 
sigma.test(x,sigma=sqrt(85),alt='greater') 

#zad7

t.test(sleep$extra[sleep$group==1],sleep$extra[sleep$group==2], alt='less', paired=T)


#zad8

library(alr3)
#a
pr=banknote$Diagonal[banknote$Y==0]
fa=banknote$Diagonal[banknote$Y==1]

var.test(pr,fa)
#b
t.test(pr,fa,alt='gr')
t.test(pr,fa,alt='gr',var.equal=T)

#zad9

library(MASS)

a=Cushings$Te[Cushings$Type=='a']
b=Cushings$Te[Cushings$Type=='b']
a
b

#a
var.test(a,b)$p.val

#b
t.test(a,b,alt='tw')
t.test(a,b)$p.val
#c
t.test(a, conf.level = 0.99)$conf.int
t.test(b, conf.level = 0.99)$conf.int

#zad10

P=c(0.42,0.8,0.42,0.3,0.55,-0.13)
Z=c(-0.2,1.4,-0.3,0.25,1.6,0.4,0.3,1.1,0.2)

#a


#b
t.test(P,Z,var.equal=F)$p.val


#zad11
#b
pbinom(9,10,0.95)
#c
dbinom(0,10,0.95)

#zad12

dbinom(0,15,0.05)

#zad13
x=c(11.24,0.86,3.43,3.58,4.46,4.34,30.85,3.78,4.52,0.2)
#EMM i ENW
1/mean(x)

library(MASS)
x <-rexp(1000,1)

fitdistr(x,"exponential")

## ---- 03.12.2016 ----

#zad3

library(HSAUR2)
shapiro.test(household$housing)

hist(household$housing)

#zad4

shapiro.test(anscombe$x1)
shapiro.test(anscombe$x2)
shapiro.test(anscombe$x3)
shapiro.test(anscombe$x4)

View(anscombe)
?anscombe

#Y=a+bX+E

model1=lm(y1~x1, data=anscombe)
summary(model1, data=anscombe)$coef
summary(model1, data=anscombe)$r.squared
summary(model1, data=anscombe)

model2=lm(y2~x2, data=anscombe)
summary(model2, data=anscombe)



A
x=c(9.65,
4.43
,8.10
,3.36
,6.04
,7.12
,4.92
,6.38
,9.22
,5.36
,3.75
,6.71)

y=c(3.81
,1.40
,3.05
,1.57
,2.02
,3.13
,1.53
,2.44
,3.45
,2.05
,1.71
,2.32)

?scan

layout(1)
plot(x,y)
model=lm(y~x)
abline(model)
summary(model)
resid(model) #model$resid
plot(resid(model))

fitted(model) #model$fitted
resid(model)-(y-fitted(model))

plot(model$fitted, model$resid); abline(h=0)
shapiro.test(model$res)

nowe=data.frame(x=c(5,7)) 
predict(model,nowe)
predict(model,nowe,interval="conf") 
predict(model,nowe,interval="pred")
#predykcja przedzialowa - pacz ABSskrypt

#zad6

chi=c(19,17,29,34,21)

chisq.test(chi)


#zad7

x=c(107,66,27)
r=c(0.6,0.25,0.15)
chisq.test(x,p=r)

#zad8

x=c(12.8,
       10,
       11.3,
       9.8,
       9.7,
       12,
       8.4,
       10.1,
       9.5,
       11.6
       )

ks.test(x,pnorm,11.5,1.8)

#zad9

y=c(10
,
21
,
37
,
12
,
44
,
15
,
25
,
28
,
17)

ks.test(y,pexp,1/20)

#zad10

m=matrix(c(134,156,167,172),2,2)
m
chisq.test(m)
chisq.test(m,corr=F)
