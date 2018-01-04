#############################
################### zadanie 1
#############################

par(mfcol=c(1,1))
curve(dnorm(x,0,1)) #  niewiele wida? - zmieniamy zakres x i y 
curve(dnorm(x,0,1),xlim=c(-6,6),ylim=c(0,1),main="g?sto?ci normalne")
curve(dnorm(x,-2,1),add=T,col=2)
curve(dnorm(x,1,1),add=T,col=3)
curve(dnorm(x,0,2),add=T,col=4)
curve(dnorm(x,0,1/2),add=T,col=5)

# dystrybuanty:
 #  zmieniamy zakres x (y i tak bedzie w przedziale (0,1)) 

curve(pnorm(x,0,1),xlim=c(-6,6),main='dystrybuanty')
curve(pnorm(x,-2,1),add=T,col=2)
curve(pnorm(x,1,1),add=T,col=3)
curve(pnorm(x,0,2),add=T,col=4)
curve(pnorm(x,0,1/2),add=T,col=5)


curve(dt(x,1),xlim=c(-4,4),ylim=c(0,0.4),col=5,ylab="")
curve(dt(x,5),add=T, col=2)
curve(dt(x,15),add=T, col=3)
curve(dt(x,40),add=T, col=6)
curve(dnorm(x),add=T)

#############################
###################    zadanie 2
#############################

# a)

pnorm(1)-pnorm(-1)  # to samo co pnorm(1,0,1)-pnorm(-1,0,1)
#mozna skorzysta? z symetrii rozk?adu:

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


#############################
###################    zadanie 3
#############################

pcauchy(1,0,1)-pcauchy(-1,0,1)

# a) 

pcauchy(1,0,1)-pcauchy(-1,0,1)  

#lub
2*(pcauchy(1,0,1))-1
pcauchy(2,0,1)-pcauchy(-2,0,1)  
pcauchy(3,0,1)-pcauchy(-3,0,1)  


# b) 
set.seed(123)

cau=rcauchy(10000,0,1)

c1=cau[cau>-1 & cau<1]
c2=cau[cau>-2 & cau<2]
c3=cau[cau>-3 & cau<3]

length(c1)/10000
length(c2)/10000
length(c3)/10000

# rzut oka na g?sto?ci obu rozk?ad?w:

curve(dnorm(x),xlim=c(-6,6))
curve(dcauchy(x,0,1),col=3,add=T)

curve(dnorm(x),xlim=c(-4,4),ylab="")
curve(dcauchy(x,0,1),col='green',add=T)
abline(v=1,col=4,lty=2)> 
abline(v=-1,col=4,lty=2)
abline(v=2,col=5,lty=2)
abline(v=-2,col=5,lty=2)
abline(v=-3,col=6,lty=2)
abline(v=3,col=6,lty=2)


#############################
###################    zadanie 4
#############################

qnorm(0.95)

# a)
qnorm(0.95)
pnorm(1.644854)

# b)
qnorm(0.99,-4,2)
?qnorm
# c)
qt(0.9,20)
?qt
# d)
qchisq(0.95,5)
?qchisq

curve(dchisq(x,5), xlim = c(0,50))
curve(dchisq(x,6), col=2, add=TRUE)
curve(dchisq(x,7), col=3, add=TRUE)
curve(dchisq(x,500), col=3, add=TRUE)
# f)
qf(0.9,3,12)
?qf
# h)
qf(0.95,7,20)

#############################
###################    zadanie 5
#############################

#a)
1-pnorm(130,120,20)
#b)
qnorm(0.85,120,20)
#c)
qnorm(0.25,120,20)
#############################
###################    zadanie 6
#############################

# X - liczba mieszka?c?w, do kt?rych reklama dotar?a
# X ma rozk?ad bin(n=14, p=0.18)
#a)
# P(X>=6)=1-P(X<6)=1-P(X<=5)
1-pbinom(5,14,0.18)
#b)
# P(X=5)

dbinom(5,14,0.18)
#c)
pbinom(7,14,0.18)-pbinom(2,14,0.18)
sum(dbinom(3:7,14,0.18))

#############################
###################    zadanie 7
#############################

#  X-czas poprawnej pracy; EX=100, czyli lambda=0.01

# a) P(X>t)gdzie t=50, 100, 150

1-pexp(50,0.01)
1-pexp(100,0.01)
1-pexp(150,0.01)

#pexp(0.0001,0.01)

curve(dexp(x, 0.01), xlim = c(0,200))

?dexp

# b)  P(X>c)=0.95, czyli P(X<=c)=0.05, zatem c - kwantyl rz?du 0.05 rozkladu X

qexp(0.05,0.01)

qexp

#############################
###################    zadanie 8
#############################

1-ppois(16,8)

#############################
###################    zadanie 9
#############################

# dla rozk?adu wyk?adniczego:

exp <- rexp(10000, 1/3)
Cauch <- rcauchy((10000))

X <- c(1,2,3,4,5)
S=cumsum(X)


par(mfcol=c(2,1))

N= 10000
n=1:N
lambda=1/3
X=rexp(N,lambda)
S=cumsum(X)
M=S/n
plot(n,M,type='l')
abline(h=1/lambda, col=3)


# to samo dla C(0,1)

N= 10000
n=1:N
X=rcauchy(N)
S=cumsum(X)
M=S/n
plot(n,M,type='l')
abline(h=0, col=3)

#############################
###################    zadanie 10
#############################

N=10000
n=100
lam=0.25
?replicate
S=replicate(N,sum(rexp(n,lam)))
nS=(S-n/lam)/(sqrt(n)/lam)  # ci?g unormowanych sum
hist(nS,prob=T,br="Scott")
curve(dnorm(x,mean=0,sd=1), col=4,add=T)

#############################
###################    zadanie 11
#############################

## a)

n=10000
X=runif(n)
Y=sqrt(X*(1-X))
sum(Y)/n 

?runif

## b)

n=10000
X=runif(n,1,2)
Y=sqrt(log(X))
sum(Y)/n


#############################
###################    zadanie 12
#############################
p=0.02; n=400
# wynik dok?adny:
pbinom(6,n,p)-pbinom(1,n,p)
# przybli?enie rozkladem Poissona, lambda=np
ppois(6,n*p)-ppois(1,n*p)
# przyblizenie normalne:
pnorm(6,n*p,sqrt(n*p*(1-p)))-pnorm(1,n*p,sqrt(n*p*(1-p)))
#lepsze, z poprawk? na ci?g?o??
pnorm(6.5,n*p,sqrt(n*p*(1-p)))-pnorm(1.5,n*p,sqrt(n*p*(1-p)))
#############################
###################    zadanie 13
#############################
###############################################################################################################
###################################################################################
##################################################################################


1-pnorm((10000-120*80)/(sqrt(120)*50))

############################################## zadania dodatkowe
#############################
###################    zadanie 1 d
#############################

par(mfcol=c(1,1))
curve(dt(x,50),xlim=c(-4,4),ylim=c(0,0.4),col=5)
curve(dt(x,5),add=T, col=2)
curve(dt(x,10),add=T, col=3)
curve(dt(x,30),add=T, col=6)
curve(dt(x,50),add=T, col=9)

#dorysujmy jeszcze na czarno g?sto?? rozk?adu N(0,1)
curve(dnorm(x),add=T)  #jaki wniosek z tego p?ynie?

#############################
###################    zadanie 2 d
#############################

curve(dchisq(x,1),xlim=c(0,40),ylim=c(0,0.2),col=1)
curve(dchisq(x,2),add=T, col=2)
curve(dchisq(x,5),add=T, col=3)
curve(dchisq(x,10),add=T, col=4)
curve(dchisq(x,30),add=T, col=5)

#############################
###################    zadanie 3 d
#############################

curve(dgamma(x,1,1),xlim=c(0,20),ylim=c(0,0.6),col=1)
curve(dgamma(x,1,3),add=T, col=2)
curve(dgamma(x,1,10),add=T, col=3)
curve(dgamma(x,2,1),add=T, col=4)
curve(dgamma(x,5,1),add=T, col=5)
curve(dgamma(x,10,1),add=T, col=6)
curve(dgamma(x,5,2),add=T, col=7)
curve(dgamma(x,5,10),add=T, col=8) # ta krzywa ju? si? nie mie?ci, mozna jeszcze raz
#wywo?a? np pierwszy rysunek z innymi xlim i ylim:

curve(dgamma(x,1,1),xlim=c(0,2),ylim=c(0,2),col=1)
curve(dgamma(x,5,10),add=T, col=8)

#############################
###################    zadanie 4  d
#############################

curve(dbeta(x,1,1),ylim=c(0,4.5),col=1)
curve(dbeta(x,1,4),add=T, col=2)
curve(dbeta(x,4,1),add=T, col=3)
curve(dbeta(x,4,4),add=T, col=4)
curve(dbeta(x,4,10),add=T, col=5)
curve(dbeta(x,10,4),add=T, col=6)

#############################
###################    zadanie 5  d
#############################

curve(df(x,1,1),xlim=c(0,5),ylim=c(0,1.8),col=1)
curve(df(x,1,4),add=T, col=2)
curve(df(x,4,1),add=T, col=3)
curve(df(x,5,5),add=T, col=4)
curve(df(x,20,10),add=T, col=5)
curve(df(x,20,25),add=T, col=6)

#############################
###################    zadanie 6  d
#############################
# a)

1-ppois(4,5)

# b)

ppois(3,5)-ppois(0,5)

# c)

pt(-1,7)+1-pt(2,7)

# d)

pt(3,7)-pt(1,7)

# e)

pf(8,6,10)-pf(3,6,10)

# f)

pf(5,6,10) + 1 -pf(7,6,10)

#############################
###################    zadanie 7   d
#############################


# t(2)

set.seed(102)
nmax= 10000
n=1:nmax
X=rt(nmax,2)
S=cumsum(X)
M=S/n
plot(n,M,type='l')

# Pa(1)
library(VGAM)  # w tym pakiecie mamy funkcje wyznaczaj?ce gestosc 
#itp dla rozkladu Pareto
set.seed(102)
nmax= 10000
n=1:nmax
X=rpareto(nmax,1,1)
S=cumsum(X)
M=S/n
plot(n,M,type='l')

## dwupunktowy z p=0.25

nmax= 10000
n=1:nmax
X=rbinom(nmax,1,0.25)
S=cumsum(X)
M=S/n
plot(n,M,type='l')
abline(h=0.25, col=3)




#############################
###################    zadanie 8  d
#############################

#a)

n=10000
X=runif(n,pi/2,pi)
Y=pi/2*cos(X)/X
sum(Y)/n


## b)

n=100000
X=rexp(n)
Y=X^1.5
sum(Y)/n

## c)

n=100000
X=runif(n)
Y=exp(X^2)
sum(Y)/n

#############################
###################    zadanie 9  d
#############################
N=10000
n=12
S=replicate(N,sum(runif(n)))
nS=(S-n/2)/(sqrt(n)/sqrt(12))  # ci?g unormowanych sum
hist(nS,prob=T,br="Scott")
curve(dnorm(x,mean=0,sd=1), col=4,add=T)

#############################
###################    zadanie 10   d
#############################

# EX =2200=sigma_x
mi=2200
n=52
sig=mi
pnorm((200000-n*mi)/(sig*sqrt(n)))-pnorm((100000-n*mi)/(sig*sqrt(n)))




