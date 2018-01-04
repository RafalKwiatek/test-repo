
?which

?seq_along
?seq_len


x[which(x>0)]
x[x>0]

wyrazy <- c("Ala ", "ma ", "kota ")
length(wyrazy)
for(i in 1:length(wyrazy)) cat(wyrazy[i])

seq(wyrazy)
wyrazy <- 100:1


# 2.1

x <- -4:4

dajZnak <- function(x){
  wynik <- integer(length(x))
  wynik[x < 0] <- -1
  wynik[x > 0] <- 1
return(wynik)
}

dajZnak(x)

#sprawdzamy poprawnosc naszej funkcji z wbudowana
all(dajZnak(x) == sign(x))
dajZnak(x)

# instalacja nowego pakietu z repozytorium CRAN
install.packages("microbenchmark")
# instalacje na danej maszynie robimy raz, pozniej wystarczy
# tylko zaladowac pakiet
require(microbenchmark)

?microbenchmark
x <- -5:5
dajZnak(x)
# sprawdzamy co dziala szybciej
microbenchmark(dajZnak(x), sign(x), times = 1000)
microbenchmark(dajZnak(-10:10), dajZnak(-100:100), dajZnak(-1000:1000))

# 2.2 wart bezwzgledna

x <- -4:4

wartBezwzgledna <- function(x){
  wynik <- x
  # zapisujemy na zmienna pomocniczna
  # zeby nie liczyc tego dwa razy
  ktore <- x < 0
  wynik[ktore] <- -1*wynik[ktore]
  return(wynik)
}

all(dajZnak(x) == sign(x))

2.10

okreslMonotonicznosc <- function(x){
  roznica <- diff(x)
  if(all(roznica == 0)){
    return("staly")
  }else if(all(roznica >= 0)){
    return("rosnacy")
  }else if(all(roznica <= 0)){
    return("malejacy")
  }else{
    return("nieokreslony")
  }
}
rosnacy <- c(1,1,1,3)
rosnacy <- 1:5
malejacy <- 4:2
staly <- rep(0, 5)
nieokreslony <- c(24,1,4)
okreslMonotonicznosc(rosnacy) == "rosnacy"
okreslMonotonicznosc(malejacy) == "malejacy"
okreslMonotonicznosc(staly) == "staly"

## roznica miedzy print i return
drukuj <- function(x){
  print(x)
}
zwroc <- function(x){
  return(x)
}
drukuj(x)
zwroc(x)
typeof(drukuj(x))
y <- dajZnak(drukuj(x))
y <- dajZnak(zwroc(x))

zsumujPodniesDoPotegi <- function(potega, ...){
  sum(...)^potega
}

zsumujPodniesDoPotegi(2, 1:5, 2, 15, 85, 73)

bezpiecznaSuma <- function(x){
  # stopifnot(is.numeric(x))
  if(is.double(x)){
    warning("Hej, spodziewałem się całkowitych, ale dam radę")
  }
  if(!is.numeric(x)){
    stop("ŹLE")
  }
  sum(x)
}
bezpiecznaSuma("abc")
bezpiecznaSuma(1.5:5)


## petla while
i <- 0
while(i < 10){
  cat("teraz jest i=",i,"\n")
  i <- i+1
}

## petla for
x <- -4:4
x <- integer(0)
# petla 1:length(x) zadziala niepoprawnie dla pustego wektora!
#for(i in 1:length(x)){
for(i in seq_along(x)){
  if(x[i] > 0){
    cat("wieksze dla i=",i,'\n')
  }else if(x[i] < 0){
    cat("mniejsze dla i=",i,'\n')
  }else{
    cat("zero dla i=",i,'\n')
  }

}
x[1]

naszaSuma <- function(x){
  suma <- 0
  for(i in seq_along(x)){
    suma <- suma + x[i]
  }  
  suma
}
x <- -4:4
naszaSuma2 <- function(x){
  suma <- 0
  for(i in x){
    suma <- suma + i
  }  
  suma
}
microbenchmark(naszaSuma(x),naszaSuma2(x), sum(x))

liczby <- c(20,16,128,127)
unlist(lapply(liczby,function(liczba)
{
 stopifnot(liczba > 0, liczba %% 1 == 0)
  k <- floor(log2(liczba)+1)
  wynik <- character(k)
  index <- k
  while(liczba != 0)
  {
    wynik[index] <- liczba%%2
    liczba = liczba %/% 2
    index <- index - 1
  }
  wynik
  paste(wynik, collapse="")
}))

rev(wynik)

x <- 1:10
x = 1:10

remove(x)
x
dajZnak(x = 1:10)
x
dajZnak(x <- 1:10)
x
