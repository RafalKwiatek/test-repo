require(dplyr)
require(stringi)


filmy <- read.csv("ml-1m/movies.dat", sep="@", header = FALSE)
colnames(filmy) <- c("movieid", "title", "genres")
filmy <- tbl_df(filmy)

stri_sub("Toy Story (1999)",1,3)
stri_sub("Toy Story (1999)",-5,-2)

filmy <- mutate(filmy, year = stri_sub(title, -5, -2))

## A
nrow(filmy) # sposob czystego R
filmy %>% summarise(ile = n()) # dplyr
filmy %>% count()

## B
filmy %>% count(year) %>% arrange(desc(n)) %>% slice(1:5)

## sed 's/::/@/g' ratings.dat > wynik.dat

oceny <- read.csv("ml-1m/ratings.dat", header = FALSE, sep=":")
oceny <- oceny[, c(T,F)]
colnames(oceny) <- c("userid", "movieid", "rank", "time")
oceny <- tbl_df(oceny)
## C

inner_join(filmy, oceny) %>% select(title, rank) %>% 
  group_by(title) %>% summarise(srednia = mean(rank), ile=n()) %>% 
  arrange(desc(srednia)) %>% filter(ile > 100) %>% slice(1:100) -> ranking
write.csv(ranking, "ranking.csv")
