
###moje notki

x<-0:10
x[c(-2,-5)]
x[c(T,F)]

x[0:5]<-100
x

x <- 1:1000000000

x[length(x)+1] <- 2

x[1000000000] <- 2
x
x <- 1:10
any(x>1)
x>1

x <- rnorm(1000000)
?norm

x

hist(x)


## ---- Pierwsze zajecia ----
2 + 2
12 ^ 8

cat("Hello \"World\"")
cat('Hello "World"')

?cat
cat
sum

c(1, 24, 83)
?rep
rep(c(3,85,313), times=3)
c(3,85, rep(313, times=3))
rep(c(3,84,313), times=c(1, 1, 3))

rep(c(3,84,313), length.out=10)
rep(c(3,84,313), each=2)

?seq
seq(from = 100, to = 1)
seq(from = 10,  by=-4,, length.out = 5)

1:100

## ---- Przypisywanie zmiennych ----

seq(from = 1, to = 10) # nie tworzy sie
seq(from <- 2, to = 10) # tworzy sie nowa zmienna

x <- 19:4
x = 1:4

typeof(x)
mode(x)
mode(1.5:5)
typeof(1.5:5)

class(matrix(1:5))
class(1:5)

matrix(1:5, 1,5)

x <- list(12:5, "ABC", TRUE)
typeof(x)
mode(x)
class(x)
x

x <- c(TRUE, 1)
x <- c("ABC", x)
x

as.character(c(TRUE, FALSE))
as.logical(c(1,0, -2))
as.integer(as.logical(c(1,0, -2)))

as.integer(x)
as.logical(x)

2 + 4i

0/0
1/0
Inf + Inf
Inf - Inf
0/Inf
1/Inf

length(1:5)
length(10)

x <- 1:7
y <- 3.5:5
x; y
x + y
x * y
1/1:10

x <- y <- 1:10 
m <- x %o% y
m
#mnożenie macierzowe
(x %o% y) %*% (x %o% y)

#mnożenie "wektorowe" - element po elemencie
(x %o% y) * (x %o% y)
    
typeof(m)
print(m)
str(m)
?str
class(m)

1:3 %in% 8:2

1:10 > c(2,5)
TRUE; T
T <- FALSE # można nadpisać, co spowoduje dziwne działanie programu!
TRUE == T
c(TRUE, FALSE) == c(T,F) 
c(TRUE, FALSE) == c(TRUE,F)

abs(-4:4)
sign(-4:4)
x <- -4:4
all(abs(x) * sign(x) == x)
abs(x) * sign(x) == x

i <- 20
z <- 2 - 1i
Re(z)
Im(z)
Conj(z)

sum(1:10)
prod(1:5)
min(abs(-4:4))
var(-4:4)
sqrt(var(-4:4))
sd(-4:4)
mean(1:1000)

?any
any(c(FALSE, FALSE, TRUE, FALSE))
all(c(FALSE, FALSE, TRUE, FALSE))
any()
all()

x <- 1:10
x[x > 5]
x>5

x <- abs(-400:400)
x==2 | x==0
x[x==2 | x==0]
y <- which(x==2 | x==0)
x[y]
y

x <- 1:10
x[1:2] <- 2:10
x

x[12]
length(x)
x[12] <- 12
x
x[100] <- 100
x
x[50] <- 50
x

rev(1:10)
x[length(x):1]

?sample

sort(sample(1:49, 49, replace = T)) # totolotek
sample(LETTERS, 20, replace = TRUE)
letters
LETTERS

#operacje na zbiorach
setequal(c(1,1,2,1), 1:2)
?setequal
setdiff(c(1,1,2,1,3), 1:2)
is.element(c(1,2,3), c(3:19))
intersect(x,m)


x <- LETTERS
x


probkaLiter <- sample(LETTERS, 30, TRUE)
length(probkaLiter) == length(unique(probkaLiter))
length(probkaLiter)
sort(probkaLiter)
length(unique(probkaLiter))
sort(unique(probkaLiter))
which(duplicated(probkaLiter))
anyDuplicated(probkaLiter)

hist(probkaLiter)
?hist

sort(probkaLiter)
duplicated(probkaLiter)
probkaLiter

?anyDuplicated
?any
any(duplicated(probkaLiter))

lista <- list(TRUE, 1:4, "abc")

str(lista[2])
str(lista[[2]])
lista[[2]][3] # drugi element listy a następnie trzeci element wektora

lista2 <- list(1:5, 2.5:8, -4:4)
lapply(lista2, mean)
unlist(lapply(lista2, mean))

sapply(lista2, mean)
?lapply
?sapply

?rnorm
hist(rnorm(100000000)) # zajawka na przyszłość

x <- 1:100
sum ( x [ round ( x ) %% 3 == 0 ])


a <- character(length(x))
a[x%%2==0] <- "parzyste"
a[x%%2==1] <- "nieparzyste"
a

x

c("a", "b", "c")[c(1,1,2)]
c("parzyste", "nieparzyste")[x %% 2 + 1]

?rle
wynik <- rle(c(1,1,1,2,2,2,1))
wynik
str(wynik)
wynik$lengths

(x^2)+5
x<-2
x<-5   #co to jest?
x <- 5 #przypisanie
x < -5 #czy porównanie z wartością ujemną?

3.141592653589793115998 == pi
format(pi, digits = 22)

1/10 + 1/10 + 1/10 == 3/10 # czemu nie jest rowne skoro to to samo
1/10 + 1/10 + 1/10 - 3/10 < .Machine$double.eps
10^-16

## do dużej dodajemy małą, odejmujemy dużą i odejmujemy małą, powinno wyjść zero
# a + b - a - b == 0
.Machine$double.xmax + .Machine$double.xmin - .Machine$double.xmax - .Machine$double.xmin 
# ale niestety "gubi" się dokładność
.Machine$double.eps


x <- sample(20, 10)
x

normalizuj <- function(x){
  a <- min(x)
  b <- max(x)
  x1 <- x-a
  x2 <- x1/(b-a)
  x3 <- 2*x2
  x4 <- x3 - 1
  x4
}

normalizuj(x)

normalizuj2 <- function(x){
  2*(x - min(x))/(max(x)-min(x))-1
}

all(normalizuj2(x) == normalizuj(x))


# zad 1.10
n <- 10000000
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)
intersect(x,y)
x[1];y[1]
wynik <- x^2 + y^2 < 1
as.integer(wynik)
sum(wynik)
table(wynik)
mean(wynik)*4



x <- -100:100
sum(x>0)
x>0
as.numeric(x>0)


## zadania copy paste z kartki

1.1
A. liczba dodatnich elementów
sum  ( x > 0)
# x>0 zwraca wektor logiczny, więc sum zwróci liczbę wystąpień
wartości TRUE (bo wektor logiczny przed sumowaniem zostanie
               niejawnie przekonwertowany na wektor liczbowy, FALSE przejdzie
               na 0, a TRUE na 1)
B. podzielne przez 3
sum(round(x)%%3==0)


C. odległość od 8
abs  (  x  -  8)
abs(x-8)

D. normalizacja: https://www.wikiwand.com/pl/Interpolacja_liniowa
y0  <-   -  1  ;  y1  <-   1
x0  <-  min  (  x  );  x1  <-  max  (  x  )
y0  +   ((  y1  -  y0  )/(  x1  -  x0  ))*(  x  -  x0)
E. średnia wartość kwadratów liczb >5 lub < 2
mean  (   (  x  [  x  >   5   |  x  <   2  ])^  2  )
F. parzysta/nieparzysta
c  (  "parzysta"  ,  "nieparzysta"  )[  x  %%   2   +  1  ]
#druga opcja z tworzeniem i uzupe ł nianiem wektora:
y  <-  character  (  length  (  x  ))
y  [  x  %%  2  ==  0  ]   <-   "parzysta"
y  [  x  %%  2  ==  1  ]   <-   "nieparzysta"
G. średnia
sum  (  x  )/  length  (  x)
H. wariancja
sum  ((  x  -  mean  (  x  ))^  2  )/(  length  (  x  )-  1)
I. min maks
sort (x)[c(1,length(x))]

1.2 kalkulator
#musimy dodać 1 do x aby przesunąć liczby, pierwszym
# elementem wektora jest 0 a nie 1
cat("\n", top[x+1], "\n", mid[x + 1 ], "\n", bot[x + 1])

x<- 4:1

top <- c(" _ ", "  ", " _ ", " _ ", "   ", " _ ", " _ ", " _ ", " _ ", " _ ")
mid <- c("| |", " |", " _|", " _|", "|_|", "|_ ", "|_ ", "  |", "|_|", "|_|")
bot <- c("|_|", " |", "|_ ", " _|", "  |", " _|", "|_|", "  |", "|_|", " _|")

?\n


1.3 unikatowe
#przeczytaj w dokumentacji jak działa funkcja rle
rle (  sort (  x  )) $values

x<-sample(1:5, 5, replace = TRUE)

rle(sort(x))$values

?rle

1.4 korelacja
#ważna jest kolejność potęgowania i sumowania w mianowniku!
sum (( x  -  mean (  x  ))*( y  -  mean (  y  )))  /
    ( sqrt (  sum (( x  -  mean (  x  ))^ 2  ))  *  sqrt (  sum (( y  -  mean (  y  ))^ 2  )))

x <- 1:100
y <- 100:1

r <- (sum((x-mean(x)*(y-mean(y)))))/(sqrt(sum(x-mean(x))^2)*(sqrt(sum(y-mean(y))^2)))
r
cor(x,y)

1.5
# jeśli różnica dwóch kolejnych elementów jest równa 0 to oznacza,
# że te elementy są sobie równe
which (  diff (  x  )   ==  0)

1.6
# do wektora dodajemy ten sam wektor,ale odwrócony (dzięki funkcji
# rev) a następnie ograniczamy go do połowy elementów
(x+rev(x))[1:(length(x)/2)]
# Nawias podczas indeksowania jest istotny. Jeśli nie wiesz dlaczego
# porównaj działanie poniższych operacji i zastanów się nad kolejnością
# działań
1:10/2
1:(10/2)

1.7
# do wektora x na miejsca gdzie jest NA wpisujemy średnia z wektora x
# policzoną BEZ wartości NA - dzięki ustawieniu parametru
# na.rm na  TRUE
x[is.na(x)] <- mean( x , na.rm =  TRUE)

1.8
#mnożymy pi przez kolejne potęgi 10 (1, 10, 100, 1000 itd),
#pozbywamy się części ułamkowej (przez funkcję floor) i na koniec
#dzielimy modulo przez 10 aby z liczby kilkucyfrowej wydobyć jednostki
floor (  pi *  10 ^( 0  :  10 ))%% 10

1.9 Leibniz
#dzięki regule zawijania możemy mnożyć dwuelementowy wektor
# c(1,-1) z dowolnie długim (w tym wypadku 1000 elementów)
4 *  sum (  c  (  1  ,   -  1  )   /   (  2  *   (  0  :  999 )   +   1  ))
1.10
#losujemy punkty (x,y) - pierwszy element z wektora x i pierwszy z y
# to jeden punkt, x[2] i y[2] to drugi itd
x  <- runif (  1000 ,- 1  ,  1  ); y <
    - runif (  1000 ,- 1  ,  1)
mean (  sqrt (  x  ^  2  +  y  ^  2  )   <   1  )*4 #sprawdzamy które punkty są w kole
[ 1  ]   3.144
1.11
#tworzymy trzyelementową listę
list (  x  [  x  <  0  ],  x  [  x  == 0  ],  x  [  x  >  0  ])

x <- 1:100
y <- character ( length ( x ))
y [ x %% 2 == 0 ] <- "parzysta"
y [ x %% 2 == 1 ] <- "nieparzysta"

?rle

y <- 100:1
z <- x+ y
rle ( sort ( z ))
rle(sort(x))

inverse.rle(sort(z))
inverse.rle(c, rle ( sort ( z )) )




