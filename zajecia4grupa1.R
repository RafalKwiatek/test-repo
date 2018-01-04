## ---- Grafika ----

set.seed(42)
x <- rnorm(200, mean = 10, sd = 3)

## ---- Wizualizacja jednowymiarowa ----

#histogram
hist(x)

# funkcja hist zwraca obiekt klasy histogram, który jest automatycznie rysowany
h <- hist(x, plot = FALSE) # nie rysuje sie od razu
plot(h) # tylko na żądanie

# histogram domyślnie rysuje częstość (freq == frequency)
# ale można zmienić na gęstość
hist(x, freq = FALSE)

# liczba podziałów nam nie odpowiada? breaks
hist(x, breaks = 3)
hist(x, breaks = 0:40) #za dużo, wykres jest pusty w części
hist(x, breaks = 0.5:20) 

# kolor zły?
hist(x, col = "red")
hist(x, col = "red", border = "blue")

# może wypełnić liniami?
hist(x, density = 8)
hist(x, density = 18)


# ten tytuł też mi się nie podoba...
hist(x, main = "To jest mój pierwszy histogram")

# to jeszcze osie popraw
hist(x, main = "To jest mój pierwszy histogram",
     xlab = "To jest OŚ X", ylab = "Częstość")


# albo całkiem je wyrzuć
hist(x, main = NULL,
     xlab = "", ylab = "Częstość")

## określanie kolorów
# 1,nazwa
length(colours())
head(colours())
hist(x, col = colours()[16])


# 2.składowe RGB
# format: #rrggbb lub #rrggbbaa

hist(x, col = "#00A00060")
hist(x, col = "#00F000")
hist(x, col = rgb(200, 100, 50, maxColorValue = 255))


par(mar=rep(0,4))
plot.new()
plot.window(c(0,255),c(0,1))
for(i in 0:255){
    lines(c(i,i), c(0,1), col=rgb(44,77,0,i, maxColorValue = 255), lwd=4)
}

par(mar=rep(0,4))
plot.new()
plot.window(c(0,255),c(0,255))
axis()
for(i in 0:255){
  for(j in 0:255)
    lines(c(i,i), c(j,j), col=rgb(i,j,0, maxColorValue = 255), lwd=4)
}

# wykres kołowy
probka <- sample(c("K","M"), size = 200, replace = TRUE)
tab <- table(probka)
pie(tab)

#wieksze koło!
pie(tab, radius = 0.2)


# faktycznie koło czy wielokąt?

pie(tab, edges = 50)

for(i in 50:1){
  pie(tab, edges=i, main = i)
}

# obrót
pie(tab, init.angle = 90)
pie(tab, init.angle = 180)
pie(tab, labels = c("Kobiety", "Mężczyzn"))

# wykres słupkowy
barplot(tab)
barplot(1:5)
barplot(tab, width = 1:2, col=1:2, 
        legend.text = c("kobiety", "mężczyźni"), 
        args.legend = list(x="topright"), ylim=c(0, 140))

# wykres skrzynkowy
boxplot(x)

# spłaszczony jak naleśnik, weź go obróć
boxplot(x, horizontal = TRUE, col=3, main="wykres skrzynkowy")

## ---- Wizualizacja wielowymiarowa ----

#wykresy skrzynkowe mogą być rysowana po grupach
boxplot(mtcars$hp[mtcars$cyl==4], horizontal = TRUE)
boxplot(mtcars$hp[mtcars$cyl==6], horizontal = TRUE)

boxplot(mtcars$hp ~ mtcars$cyl, horizontal=TRUE)
boxplot(mtcars$hp ~ mtcars$cyl + mtcars$am, horizontal=TRUE)

# wykres rozrzutu (scatter plot)
plot(mtcars$mpg, mtcars$hp, xlim = c(0, 40),
     ylim=c(0, 350), main="Zasieg i konie mech", 
     sub="Podtytul", col=c("blue","red"), pch=1:10)


# wykres liniowy
x <- seq(-5,5, by=0.5)
y <- sin(x) * x^2 / 2 + sqrt(abs(cos(x-pi)))

plot(x, y, type = "p")
plot(x, y, type = "l")
plot(x, y, type = "b")
plot(x, y, type = "c")
plot(x, y, type = "h")


# skala nie musi być liniowa
plot(1:10, 2^(1:10), type="b")
plot(1:10, 2^(1:10), type="b", log = "y", col="green")



# wykresy w tytułach mogą mieć formuły matematyczne
plot(x,y, type="l", main="sin(x) * x^2 / 2 + srtq(cos(x))")
plot(x,y,type="l", 
     main = expression(sin(x) * frac(x^2,2) + sqrt(abs(cos(x-pi))) + integral(x)))

## wiele wykresów na jednym
x <- seq(-2*pi, 2*pi, by=0.1)
plot(x, sin(x), type="l")
plot(x, cos(x), type="l")

# funkcje lines i points rysują na aktualnym
plot(x, sin(x), type="l", col="red")
lines(x, cos(x), lty=4)
legend("bottomright", legend = c("sin","cos"),
       col = c("red","black"), lty = c(1, 4))


# co jak się nie zgadza?
plot(x, sin(x), type="l", col="blue")
lines(x, cos(x)+1, col = "red")

plot(x, sin(x), type="l", col="blue", ylim = range(sin(x),cos(x)+1))
lines(x, cos(x)+1, col = "red")

# wygodniej matplot matlines matpoints
y2 <- cbind(sin(x), cos(x)+1, atan(x)*2)

matplot(x, y2, type = "l", lty =1:3, lwd=1:3 )

# bez legendy ciężko stwierdzić co jest co
legend("bottomright", legend = c("sin","cos","atan"),
       lty=1:3, lwd=1:3, col=1:3)
# te kolory 1:3 brane są jako elementy wektora z palety
# palette() zwraca aktualnie ustawione barwy
# jeśli chcemy to można zmienić

# zapis do pliku
jpeg("nazwa.jpg")
plot(x,sin(x))
dev.off()

pdf()
bmp()
png()
tiff()


## ---- pakiet ggplot2 ----
require(ggplot2)
require(dplyr)

## intro

ggplot(mpg, aes(hwy, cty)) +
  geom_point(aes(color = cyl)) +
  geom_smooth() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()


###

mtcars$am <- factor(c("a","m")[mtcars$am+1])
mtcars$cyl <- factor(mtcars$cyl)

# podstawa to ggplot(dane, aes(co-x, co-y))
g <- ggplot(mtcars, aes(cyl))

# powiedzieliśmy co, teraz jak
ggplot(mtcars, aes(cyl)) + geom_bar()
g + geom_bar()

# grupowanie wykresów
ggplot(mtcars, aes(cyl, fill=am)) + geom_bar()
ggplot(mtcars, aes(cyl, fill=am)) + geom_bar(position = "dodge")

# za ciasno?
ggplot(mtcars, aes(cyl, fill=am)) + geom_bar(stat="count",width = .75)

# rozdzielamy grupy
ggplot(mtcars, aes(interaction(cyl, am), fill=am)) + 
  geom_bar(width = .75)

# dodajmy jakieś etykietki
meanHp <- mtcars %>% group_by(am, cyl) %>% 
  summarise(hp=round(mean(hp)))

ggplot(meanHp, aes(x=interaction(cyl, am), y = hp, fill="red")) + 
  geom_bar(stat = "identity",width = .5, show.legend = FALSE) +
  geom_text(aes(y=hp, label=hp), vjust=1,col="black")


countAmCyl <- mtcars %>% count(am, cyl)

ggplot(countAmCyl, aes(x=cyl, y = n, fill=am)) + 
  geom_bar(stat = "identity",width = .5, position = "dodge") +
  geom_label(aes(label=n),position=position_dodge(0.5), vjust=1)

# histogram
ggplot(mtcars,aes(hp)) + geom_histogram(binwidth = 30)

#gęstość
ggplot(mtcars,aes(hp)) + geom_density()

# histogram i gęstość
ggplot(mtcars,aes(wt)) + geom_histogram(aes(y=..density..), bins=10) + 
  geom_density()

#

## liniowe wykresy
x <- seq(-2*pi, 2*pi, by=0.1)

dane <- data.frame(x, sin=sin(x), cos=cos(x)+2, 
                   atan=atan(x)*2, exp = exp(x))

ggplot(dane) + geom_line(aes(x, cos),col="red") +
  geom_line(aes(x, sin))

# chcemy zero na osi?
ggplot(dane) + geom_line(aes(x, cos),col="red") +
  ylim(0, 5) + xlim(-10,10)

# punkty
ggplot(dane) + geom_line(aes(x, cos),col="red") +
  geom_point(aes(x, cos), col="red")

# dziedziczenie estetyki
ggplot(dane, aes(x, cos)) + geom_line() + 
  geom_point(shape=4, colour="green", size = 5)

# zmiana skali 
ggplot(dane, aes(x, exp)) + geom_line() + scale_y_log10()

# wiele lini
# przecież mamy dziedziczenie!
ggplot(dane, aes(x)) + geom_line(aes(y=sin), col="red") +
  geom_line(aes(y=cos), col="blue") + geom_point(aes(y=sin), col="red") +
  xlab("Oś X") + ggtitle("To jest tytuł", subtitle = "Podtytuł")+
  ylab("Oś Y")

# dodajemy opisy




## Drugi wariant
dane2 <- data.frame(x=rep(dane$x, 3),funkcja=rep(c("sin","cos","atan"),each=nrow(dane)),
                    y=c(dane$sin, dane$cos, dane$atan))

ggplot(dane2, aes(x=x,y=y, fill=funkcja, shape=funkcja)) + geom_line() +
  geom_point() +
  scale_shape_manual(values=c(21, 23,24)) + scale_fill_manual(values=c("red","green", "blue"))


# wykres z pokolorowanym obszarem pod wykresem
ggplot(dane2, aes(x=x,y=y, fill=funkcja, shape=funkcja)) + geom_area()

# wykres składany
library(gcookbook) 
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area() +
  scale_fill_brewer("blue")

sps <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) +
  geom_point() +
  scale_colour_brewer(palette="Set1")
sps

sps + geom_smooth()
sps + geom_smooth(method=lm)
sps + geom_smooth(method=loess)

## zapis do pliku
# automatyczne dopasowanie typu pliku do podanego rozszerzenia
ggsave("ggplot.png", plot=sps)

#domyślnie zapisuje ostatni wykres używając:
last_plot()

## wielowymiarowy wykres rozproszenia
pairs(mtcars[,2:7])

# wykresy są symetryczne, szkoda marnować miejsca
pairs(mtcars[,2:7], upper.panel = panel.smooth)

plotmatrix()

