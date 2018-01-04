# ---- macierze  ----

m <- matrix(1:6, ncol = 2, nrow = 3)
m

# nie trzeba podawać wszystkich parametrów
m <- matrix(1:6, ncol = 2)
m
m <- matrix(1:6, nrow = 2)
m
# jeśli wymiary nie zgadzają się z podanym wektorem to zadziała reguła zawijania
m <- matrix(1:6, ncol = 4)
m
matrix(1:6, ncol=8, nrow=2)
# macierze domyślnie są zorientowane kolumnowo - byrow
matrix(1:6, ncol=2, byrow = TRUE)

# macierz ma atrybuty, które odpowiadają za liczbę kolumn i wierszy
attributes(m)

# macierz może być rozszerzona przez dołączanie do niej wierszy
rbind(1:5,5:1)
rbind(m, 1:4) #ok
rbind(m, 1:2) #regula zwijania
rbind(m, 1:5) # za dlugi - obcinamy

rbind(m,m,m,m,m)


# oraz kolumn
cbind(m,1:5)

# macierze można tworzyć poprzez "upraszczanie" listy
x <- list(1:2, 3:4, 5:6)
chybaMacierzX <- simplify2array(x)

y <- list(1:5,3)
chybaMacierzY <-simplify2array(y)

class(chybaMacierzX) == "matrix"
class(chybaMacierzY) == "matrix"

## indeksowanie macierzy

m <- matrix(1:15, nrow=3)

#macierz to wektor, więc tak samo można go indeksować
m[2:3]
m[7:8]

# można również wybierać wiersze
## m[wiersz, kolumna]
m[ 1, ]
m[2,]
m[c(1:2,1:2),]

# lub kolumny
m[, 2:3]
m[, c(T,F)] #lepiej używać TRUE/FALSE

# a także jedno i drugie
m[2,3]
m[1:2, 3:4]
m
# macierz można transponować
m
t(m)
# wziąć jej przekątną
diag(m)
det(matrix(-3:5,ncol=3))

# macierz to wektor, więc wszystkie funkcje wektorowe działają
#sumowanie
sum(m)

# średnia
mean(m)

# dodawanie
x <- matrix(0.1:4, ncol =2)
y <- matrix((4:1)^2, nrow=2)
x;y
x + y
x - y
x * y

x %*% y

c(T,F) %o% c(T,F)
# również reguła zawijania obowiązuje
x
x + 1:2
## ---- czynniki ----
# mało średnio dużo malo srednio duzo
f <- factor(rep(c("mało","średnio", "dużo"), times=3:1))
f
str(f)
# czynnik ma poziomy
levels(f)

# ale pod spodem to zwykły wektor liczbowy
as.integer(f)

levels(f)
class(levels(f))
levels(f)[as.integer(f)]
as.character(f)
levels(f)[2]

# jak sprawdzić która liczba to który poziom
cbind(1:length(levels(f)), levels(f))

## ---- ramki danych----

##tworzenie ramki danych
df <- data.frame(typ=c("A","B","C"), wartosc=1:3, zapas=50)

class(df)

# ramka to zwykła lista
str(unclass(df))


## ---- odczyt/zapis ----

df <- data.frame(typ=c("A","B","C"), wartosc=1:3, zapas=50.5)

write.table(df, "write.table",quote = FALSE)
write.csv(df, "write.csv")
write.csv2(df, "write.csv2")
writeLines(df, "writeLines")
writeLines(c("Ala ma kota","Janek lubi programować w R"), "writeLines")
dump(c("df","m"), "dump")
dput(df, "dput")
save(df, file="save")
saveRDS(df, "saveRDS")
                                                                         "wartosc", "zapas"), row.names = c(NA, -3L), class = "data.frame")

if(!require(package)){
  stop("nie ma ")
}
library()

## ---- dplyr ----
require(dplyr)
?mtcars

auta <- data.frame(car=row.names(mtcars), mtcars)
auta$am <- factor(c("a","m")[auta$am+1])
rownames(auta) <- NULL

auta <- tbl_df(auta)

# filter() - po warunku (domyślnie AND & )
filter(auta, am=="m", mpg > 30, cyl==4, qsec < 17)
filter(auta, am=="m" & mpg > 30) ## równoważne

filter(auta, am=="m" | mpg > 30) ## OR |

auta[auta$am=="m" & auta$mpg > 30 , ]

# slice() 
nowaramka <- slice(auta, 1:2)
slice(auta, 10:n())

auta[1:2, ]
auta[10:nrow(auta), ]

# arrange()
arrange(auta, mpg)
arrange(auta, am, desc(gear), desc(hp))

auta[order(auta$am, auta$gear)] # dużo bardziej skomplikwoane :(

# select()  
# wybieranie kolumn
select(auta, car, hp, gear) 
select(auta, car:hp)
select(auta, -(mpg:hp))
select(auta, car, starts_with("d"))
select(auta, car, contains("g"), contains("m"))

select(auta, nazwa_samochodu=car)

# rename()
#zmiana nawz kolumn z pozostawieniem pozostalych
rename(auta, nazwa_samochodu=car)

# distinct()
distinct(auta, gear)
distinct(auta, am, gear)

# x lp100km == 235.2 / y mpg
cbind(auta[,1:2], 235.2 /auta$mpg)
# 1 cu in = 0.016387064 lit
cbind(auta[,1:2],235.2 /auta$mpg, auta$disp* 0.016387064)

# mutate()  
# tworzy nowe i dokleja na koncu
mutate(auta, lp100km = 235.2/mpg, ccm = round(disp * 0.016387064,1))

# transmute()
# zostawia tylko te nowe
transmute(auta, car, lp100km = 235.2/mpg, ccm = round(disp * 0.016387064,1))

# summarise()
summarise(auta, mean(mpg), mean(disp))

# count()
count(auta, am, gear)

# sample_n() 
sample_n(auta, 3)
# sample_frac()
sample_frac(auta, 0.1)

auta[sample(nrow(auta),3),]

## składanie operacji

cyl6i8 <- filter(auta, cyl > 4)
tylkoHp <- select(cyl6i8, car, hp)
posortowaneHp <- arrange(tylkoHp, desc(hp))
posortowaneHp

arrange(select(filter(auta, cyl > 4), car, hp), desc(hp))

arrange(
  select(
    filter(auta, cyl > 4)
    , car, hp)
  , desc(hp)
)

## operator przekierowania - pipe

auta %>% filter(cyl > 4) %>%  select(car, hp) %>% arrange(desc(hp))

## grupowanie

group_by(auta, am) %>% summarise(ile = n(), moc=mean(hp), biegi = mean(gear), zasieg = mean(mpg))


auta %>% mutate(lp100km = 235.2/mpg, ccm = round(disp * 0.016387064,1)) %>%
  group_by(am, cyl) %>% 
  summarise(ile = n(), moc=mean(hp), biegi = mean(gear), spalanie = mean(lp100km), pojemnosc= mean(ccm))

group_by(auta, am,cyl,gear) %>% arrange(desc(hp)) %>% slice(1) %>% select(car,cyl,am,gear,hp)


## ---- JOIN laczenie wielu ramek ----

df1 <- data_frame(x = c(1, 1, 2, 3), y = 1:4*10)
df2 <- data_frame(x = c(1, 1, 2, 4), z = letters[1:4])

df3 <- data_frame(x1 = c(1, 1, 2, 3), y = 1:4*10)
df4 <- data_frame(x2 = c(1, 1, 2, 4), z = letters[1:4])

# left  - wszystko z lewego, z prawego te co pasuja
colnames(df1)
left_join(df1, df2) #stara sie domyslic jest sa wspolne
left_join(df1, df2, by=c("x")) 
left_join(df3, df4, by=c("x1"="x2"))
# right  - wszystko z prawego, z lewego te co pasuja
right_join(df1, df2)
# left(x,y) == right(y,x) - z dokładnością do kolejności kolumn

# full - wszystko z obu, nawet jak nie pasuje
full_join(df1, df2)
# inner - tylko te co pasują
inner_join(df1, df2)
# parametr by

## ---- operacje na zbiorach ----
A <- data.frame(x=1:3, y=1:3*10)
B <- data.frame(x=1:3, y=10)

union(A,B)

intersect(A,B)

setdiff(A,B)
setdiff(B,A)
