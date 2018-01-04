## ---- przetwarzanie napisów ----


require(stringi)
require(microbenchmark)

# długość napisu
x <- c("Ala ma kota", "Bardzo lubię programować w R")
length(x)
nchar(x)
stri_length(x)

microbenchmark(nchar(x), stri_length(x))
y <- stri_rand_lipsum(100)
microbenchmark(nchar(y), stri_length(y))
# porównywanie

"b" == letters
"b" < letters
"ł" < letters
stri_compare("b",letters)
stri_sort(c("hladny", "chladny"), locale="pl_PL")

stri_sort(c("hladny", "chladny"), locale="sk_SK")
stri_trans_toupper("i")
stri_trans_toupper("i",locale="tr_TR")
i -> I

# łączenie i powielanie
stri_paste(letters, 1:13, c("-","#","$"),"XYZ")
stri_paste(letters,"-", 1:26)
stri_paste(letters, 1:26, "a","#43", sep="-")

stri_paste(letters, 1:26, "a","#43", sep="-", collapse = NULL)
stri_paste(letters, 1:26, "a","#43", sep="-", collapse = "*")

stri_paste(letters[1:3],10,collapse = "")
stri_paste(letters[1:3],10,sep = "-", collapse="*")
stri_join()

# wyznaczanie/podmiana podnapisów
stri_sub(x,from=5,to=10)
stri_sub(x,to=11)

#ujemne to liczenie od końca napisu
stri_sub(x,from=-5)
# to - określona liczba znaków
stri_sub(x, to=10)

x
stri_sub(x,1, c(3,6)) <- c("Stefan","Strasznie")
x

## ---- wyszukiwanie wzorców ----
## wzorce ustalone
x <- c("Ala ma kota", "Bardzo lubię programować w R")

# wykrywanie
stri_detect_fixed(x, "ma")
stri_detect_fixed(x, "ba")


# ignorujemy wielkość liter
stri_detect_fixed(x, "ba", opts_fixed = stri_opts_fixed(case_insensitive = TRUE))

# zliczanie
stri_count_fixed(x, "ma")
stri_count_fixed(x, "a", opts_fixed = stri_opts_fixed(case_insensitive = TRUE))

stri_count_fixed("ababa", c("a","ab","aba"))
# pozwalamy aby wzorce na siebie nachodziły
stri_count_fixed("ababa", c("a","ab","aba"), opts_fixed=stri_opts_fixed(,TRUE))


# ile razy każda z liter wystąpiła w napisie
ile <- stri_count_fixed("Ala ma kota", letters)
names(ile) <- letters
ile

ile <- stri_count_fixed(x, letters)
# lokalizowanie - pierwsze, ostatnie i wszystkie
stri_locate_first_fixed(x, "ma")
stri_locate_first_fixed(x, "a")
stri_locate_last_fixed(x, "a")

stri_locate_all_fixed(x, "a")

# wydobywanie - pierwsze, ostatnie i wszystkie
gdzie <- stri_locate_first_fixed(x, "ma")
stri_sub(x,gdzie)



# zastępowanie
gdzie <- stri_locate_first_fixed(x, "a")
stri_sub(x,gdzie) <- "@"
x

stri_replace_all_fixed("Ala ma kota i psa","a","@", opts_fixed = stri_opts_fixed(TRUE))

stri_replace_all_charclass("Ala ma kot'a, psa; i chomika.","[:punct:]","")

# dzielenie
stri
stri_split_fixed("Ala ma kota, chomika i ma psa.", " ")


# zmienianie wielkości
stri_trans_tolower("Ala MA KOTA")
stri_trans_toupper("Ala MA KOTA")
stri_trans_totitle("Ala MA KOTA")

## wyrażenia regularne
stri_split_regex("Ala ma kota, psa i chomika.","[ ,\\.]")

stri_extract_all_regex("Mam 5 psów i 3 koty.","\\d \\w+")
stri_extract_all_regex("Mam 5 psów i 3 koty.","[0-9] [A-Za-złó\\.]+")

stri_extract_all_regex("Mam 5 psów i 3 koty.","[A-Za-z]+\\.")

1480772708 /60 /60/24/365

23:59:59
23:59:60
00:00:00

stri_stats_latex("Mam 5 psów i 3 koty. Ala ma osiem kaczek i trzy kury.")
stri_stats_general("Mam 5 psów i 3 koty. Ala ma osiem kaczek i trzy kury.")

stri_replace_all_fixed("Cezar",letters, LETTERS, vectorize_all = FALSE)
  
cat("a      \n a\" \t a \\.")

movieid, title, genres
10, "Toy Story", 
10, Raz\, dwa\, trzy,
letters 
letters[(1:26+13-1)%%26+1]
x <- "Ala ma kota"
stri_sub(x,1:stri_length(x), length = 1)

# znaki specjalne:
# . \ | ( ) [ ] { } ^ $ * + ? -

email <- c("ala.ma.kota@sages.pl",
           "JanKowalski123@programowanie.r",
           "zamowienia@sklep.abc",
           "to.nie.jest.em@il")

