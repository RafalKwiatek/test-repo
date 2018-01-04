# zadanie 1 
# a)
prop.test(29,250,conf.level=0.95)$conf.int
?prop.test
binom.test(29,250,conf.level=0.98)$conf.int
?binom.test

?sigma.test
# b)

prop.test(29,250,p=0.1, alt='greater')
binom.test(29,250,p=0.1, alt='greater')

# zadanie 2
prop.test(c(37,29),c(228,203),alt='greater')

prop.test(c(37,29),c(228,203),alt='greater', corr=F)

# zadanie 3
#a)
binom.test(118,150,p=0.8)

#b)
binom.test(118,150,conf.level=0.99)$conf.int
prop.test(118,150,conf.level=0.99)$conf.int

#zadanie 4
library(lattice)
t.test(barley$yield, mu=32, alt="greater")
?t.test
#zadanie 5
x=Forbes2000$profits[Forbes2000$category=='Insurance']

#a)
t.test(x,conf.level=0.9)$conf.int
mean(x, na.rm=TRUE)
mean(na.omit(x))
?mean
?t.test
t.test(x,conf.level=0.9)$conf.int

#b)
library(TeachingDemos)
sqrt(sigma.test(na.omit(x),conf.level=0.9)$conf.int)

#zadanie 6

library(lattice)
#a)
t.test(environmental$temperature, conf.level=0.99)$conf.int
mean(environmental$temperature)
dim(environmental)
head(environmental)
#b)
t.test(environmental$temperature, mu=80, alt='less')
#c)
library(TeachingDemos)
sqrt(sigma.test(environmental$temperature,conf.level=.97)$conf.int)

#d)
sigma.test(environmental$temperature,sigmasq=85,alt='greater')

#zadanie 7
t.test(sleep$extra[sleep$group==1],sleep$extra[sleep$group==2], alt='less',paired=T)

sleep %>% group_by(group) %>% summarise(mean(extra))

dim(sleep)
head(sleep)

#zadanie 8
#a)
var.test(banknote$Diagonal[banknote$Y==0], banknote$Diagonal[banknote$Y==1])
#b)
t.test(banknote$Diagonal[banknote$Y==0], banknote$Diagonal[banknote$Y==1],alt='gr')

t.test(banknote$Diagonal[banknote$Y==0], banknote$Diagonal[banknote$Y==1], alt='gr', var.equal=T)
#zadanie 9
#a)
var.test(Cushings$Te[Cushings$Type=='a'], Cushings$Te[Cushings$Type=='b'])
# b)
t.test(Cushings$Te[Cushings$Type=='a'], Cushings$Te[Cushings$Type=='b'])
# c)
t.test(Cushings$Te[Cushings$Type=='a'],conf.level=0.99)$conf.int
t.test(Cushings$Te[Cushings$Type=='b'],conf.level=0.99)$conf.int

#zadanie 10
P = c(0.42, 0.8, 0.42, 0.3, 0.55, -0.13)
Z = c(-0.2, 1.4, -0.3, 0.25, 1.6, 0.4, 0.3, 1.1, 0.2)
#a)
var.test(P, Z)
#b)
t.test(P, Z, var.equal=T)

# zadanie 10

#Liczba os?b, kt?re poda?y ,,dobry'' przedzia? ufno?ci jest zmienn?
# losow? o rozk?adzie dwumianowym $bin(10, 0.95)$. 
EX=np=9.5.
  
 #b) 
pbinom(9,10,0.95)
 #c)
dbinom(0,10,0.95)

#zadanie 12

# Niech Y oznacza liczb? odrzuconych hipotez zerowych. 
# Wiemy, ?e ?rednia waga jest zgodna z deklaracj?, wi?c 
# odrzucenie hipotezy zerowej oznacza pope?nienie b??du pierwszego rodzaju.
# Prawdopodobie?stwo wyst?pienia takiego b??du jest wynosi $\alpha=0.05$. 
# St?d $Y$ ma rozk?ad dwumianowy $bin(n=15, p=0.05)$. 
#a) EY=np=15*0.05
  
  #b)
dbinom(0,15,0.05)

#zadanie 13
x = c(11.24, 0.86, 3.43, 3.58, 4.46, 4.34, 30.85, 3.78, 4.52, 0.2)
# EMM i ENW
1/mean(x)

library(MASS)
fitdistr(x,"exponential")

#zadanie 14
x=c(-1.11 ,  2.90 , -1.10 , -3.80 , -1.66,  -3.07 , -3.25 , -3.01,
     -1.27,  -5.86 ,-14.99 ,-10.23,  -3.91 , 99.34,  -3.56,  -5.39)
#EMM
mean(x)/(mean(x)-1)
#ENW
length(x)/log(prod(x))

ml=function(a){(a+1)*log(prod(x))-length(x)*log(a)}
nlm(ml,p=0.1)$est

#zadanie 15
x = c(-1.11, 2.90, -1.10, -3.80, -1.66, -3.07, -3.25, -3.01, -1.27,
      +         -5.86, -14.99, -10.23, -3.91, 99.34, -3.56, -5.39)
 q1=quantile(x,0.25)
 q3=quantile(x,0.75)
 (a=(q1+q3)/2
  
#zadanie 16
set.seed(123)
 N=300
 n=50
 MM=replicate(N,2*mean(runif(n,0,4)))
 MNW=replicate(N,max(runif(n,0,4)))


 par(mfrow=c(2,1))
 plot(MM,pch=16,cex=0.6)
 abline(h=4,col=2,lwd=2)
 plot(MNW,,pch=16,cex=0.6, ylim= c(min(MNW)-0.05,max(MNW)+0.05) )
 abline(h=4,col=2,lwd=2)


par(mfrow=c(1,2))
 hist(MM,breaks=20)
 abline(v=4,col=2,lwd=2)
 hist(MNW,breaks=20,xlim= c(min(MNW)-0.05,max(MNW)+0.1) )
 abline(v=4,col=2,lwd=2)

#zadanie 17
set.seed(123)
 N= 500
 i=1:N
 X=rnorm(N,4,2)
 Srednie=cumsum(X)/i

  
 plot(Srednie,type='l')
 abline(h=4, col=2, lwd=2)


 Mediany=c()
 for (i in 1:N )   Mediany[i]=median(X[1:i])


 plot(Mediany, type='l')
 abline(h=4, col=2, lwd=2)

Wariancje=c()
 for (i in 1:N )   Wariancje[i]=var(X[1:i])

plot(Wariancje, type='l')
abline(h=4, col=3, lwd=2)

# zadanie 18
set.seed(101)
 N= 500
 i=1:N
 X=rcauchy(N,20,1)
 Srednie=cumsum(X)/i
 plot(Srednie,type='l')
 abline(h=20,col=3,lwd=2)

#zadanie 19
x=c(0.37,  0.14,  0.25,  0.16, 0.12 , 0.07, 0.75,  0.41,
    0.46 ,0.18,  0.26,0.51,  0.06,  0.38, 0.40,  0.62)
fitdistr(x,'gamma')

#EMM:
n=length(x)
S2=(n-1)/n*var(x)

# p:
mean(x)^2/S2
# a:
mean(x)/S2
