## ---- przetwarzanie napisów ----


require(stringi)

# długość napisu
x <- c("Ala ma kota", "Bardzo lubię programować w R")
length(x)
stri_length(x)

# porównywanie
letters
LETTERS

letters == "a"
which(letters == "a")
letters > "b"
# w jezyku polskim sortujemy tak
chmiel
czysto
# 
czysto 
dom
chałupa
hamak
chmiel

strile

stri_sort(c("hladny", "chladny"), locale="pl_PL")

stri_sort(c("hladny", "chladny"), locale="sk_SK")
stri_trans_toupper("i",locale="tr_TR")
i -> I

# łączenie i powielanie
stri_paste(x)
stri_paste(letters[1:6], 1:3, "-X1")
stri_paste(2000:2014,"-", 5,"-", 1:3)
stri_paste(2000:2014, 5, 1:3, sep = "-")
stri_paste(2000:2014, 5, 1:3, collapse = "")
stri_paste(letters, 1:5, collapse = "")

# wyznaczanie/podmiana podnapisów
stri_sub(x,c(1,1,1,1),c(3,6,9,12))
stri_sub("Ala ma kota",1,1:10)
stri_sub(x, stri_length(x)-3, stri_length(x))
stri_sub(x, -4, -1)
stri_sub(x, 4, length = 5)
stri_sub(x, 1:4, length = 5)
x
stri_sub(x, 1,c(3,6)) <- "Stefan"
stri_sub(x, 3,length=0) <- "Stefan"
x
napis <- "ABCDefgdsfajd"
stri_sub(napis, 1:stri_length(napis), length = 1)

## ---- wyszukiwanie wzorców ----
## wzorce ustalone
x <- c("Ala ma kota", "Bardzo lubię programować w R")

# wykrywanie
stri_detect_fixed(x, "ba")
stri_detect_fixed(stri_trans_tolower(x), "ba")
# ignorujemy wielkość liter
stri_detect_fixed(x, "ba", opts_fixed = stri_opts_fixed(case_insensitive=TRUE))

# zliczanie
stri_count_fixed("ababa", c("a","ab","aba"))
# pozwalamy aby wzorce na siebie nachodziły
stri_count_fixed("ababa", c("a","ab","aba"), opts_fixed = stri_opts_fixed(FALSE, TRUE))
stri_count_fixed(x[1], letters)
stri_count_fixed(x[1], LETTERS)
y <- stri_count_fixed(x[1], c(letters,LETTERS))
names(y) <- c(letters,LETTERS)
y
# ile razy każda z liter wystąpiła w napisie
stri_count_fixed(x[1], letters, opts_fixed = stri_opts_fixed(TRUE))

# lokalizowanie - pierwsze, ostatnie i wszystkie
stri_locate_first_fixed("Ala ma kota i ma psa", "ma")
stri_locate_last_fixed("Ala ma kota i ma psa", "ma")

pozycje <- stri_locate_last_fixed("Ala ma kota i ma psa", c("ma","psa", "kotlet"))
pozycje[,1] #początki
pozycje[,2] #końce

pozycje2 <- stri_locate_all_fixed("Ala ma kota i ma psa", c("ma","psa", "kotlet"))
pozycje2[[1]] # lokalizacje wystapienia słowa "ma"

napis <- "Ala ma kota i ma psa"
stri_sub(napis, pozycje) <- c("posiada","chomika","")
napis
stri_sub(napis, pozycje)

# wydobywanie - pierwsze, ostatnie i wszystkie
stri_count_fixed(napis, "ma")
stri_extract_all_fixed(napis, "ma")

# zastępowanie
stri_replace_first_fixed("abc123 abc 321", "1", "#")
stri_replace_last_fixed("abc123 abc 321", c("1","2"), c("#","$"))
stri_replace_all_fixed("abc123 abc 321", "1", "#")

stri_replace_all_fixed("abc123 abc 321", c("1","2"), c("#","$"), vectorize_all = FALSE)

# dzielenie
stri_split_fixed("Ala ma kota i ma psa",c(" ","a"))
stri_split_fixed("Ala ma kota, psa i chomika."," ")

stri_split_fixed("bab","a")
stri_split_fixed("ababa","a")

# zmienianie wielkości
stri_trans_tolower("Ala ma kota")
stri_trans_toupper("Ala ma kota")
stri_trans_totitle("Ala ma kota")

## wyrażenia regularne

stri_detect_regex("aabaaba","ab*")
stri_count_regex("aabaaba","ab*")
stri_split_fixed("Ala ma kota, psa i chomika. Chomik ma na imię Bogdan."," ")
stri_split_regex("Ala ma kota, psa i chomika.","[,\\. :\\-]")

# znaki specjalne:
# . \ | ( ) [ ] { } ^ $ * + ? -

email <- c("ala.ma.kota@sages.pl",
           "JanKowalski123@programowanie.r",
           "zamowienia@sklep.abc",
           "to.nie.jest.em@il")
bartektartanus@gmail.com

# letters[1] -> letters[1+13]
# letters[13] -> letters[13+13]
# letters[14] -> letters[1]
cezar0 <- function(x){
  stri_replace_all_fixed(x, letters, letters[(1:26+13-1)%%26+1],
                         vectorize_all = FALSE)
}
cezar <- function(x){
  stri_replace_all_fixed(stri_trans_tolower(x), letters, LETTERS[(1:26+13-1)%%26+1],
                         vectorize_all = FALSE)
}
cezar0(x)
cezar0(cezar0(x))
cezar(x)
cezar(cezar(x))

all(stri_detect_regex(email, "[A-Za-z0-9\\.]+(@|AT)[A-Za-z0-9]+\\.[A-Za-z]{1,3}") == c(TRUE, TRUE, TRUE, FALSE))

zlicz1 <- function(str){
  x <- stri_count_fixed(str[1], letters, 
                   opts_fixed = stri_opts_fixed(TRUE))
  names(x) <- letters
  x  
}

zlicz2 <- function(str){
  x <- stri_split_regex(str[1], "[^a-zA-Z]|")[[1]]
  x <- x[x!=""]
  table(stri_trans_tolower(x))
}
napis <- "To jest bardzo fajna funkcja"
zlicz1(napis)
zlicz2(napis)

# zad 5.3
adresy <- c("bartek@gmail.com to mój, a ten nie jest mój adres ala.ma.kota@sages.pl","to.nie.jest.email bo nie ma @",
"JanKowalski123@programowanie.r","zamowienia@sklep.abc")

regex_email <- "[\\w.]+@\\w+\\.[A-Za-z0-9]{1,3}"

all(stri_detect_regex(adresy, regex_email) 
    == c(TRUE, FALSE, TRUE, TRUE))

stri_extract_all_regex(adresy,regex_email)

cezar <- function(napis){
  stri_trans_tolower(stri_replace_all_fixed(stri_trans_tolower(napis), 
                                            letters, 
                         LETTERS[(1:26+13-1)%%26+1],
                         vectorize_all = FALSE))
}

table(sample(letters[1:5],50,,rep=TRUE),sample(letters[1:5],50,,rep=TRUE))

punktyZaLitery <- c(1,5,3,2,6,2,1,5,5,3,3,1,3,2,2,3,2,1,7,1,5,2,1,1,5,2,3,1,2,1,9,5)
names(punktyZaLitery) <- c("a","ą","b","c","ć","d","e","ę","f","g","h","i","j","k","l","ł","m","n","ń","o","ó","p","r","s","ś","t","u","w","y","z","ź","ż")
punktyZaLitery

require(stringi)
napis <- "obrotomierz"
sum(punktyZaLitery[stri_split_regex(napis,"|")[[1]][-c(1, stri_length(napis)+2)]])
