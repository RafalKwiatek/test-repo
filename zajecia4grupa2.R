## ---- Grafika ----
## generowanie ziarna 
#x <- round(runif(1, max=2^31),0)

set.seed(42)
x <- rnorm(200, mean = 10, sd = 3)

## ---- Wizualizacja jednowymiarowa ----

#histogram
hist(x)

# funkcja hist zwraca obiekt klasy histogram, który jest automatycznie rysowany
h <- hist(x, plot = FALSE)
h
plot(h)

# histogram domyślnie rysuje częstość (freq == frequency)
# ale można zmienić na gęstość
hist(x, freq = FALSE)


# liczba podziałów nam nie odpowiada? breaks
hist(x, breaks = 20)
hist(x, breaks = min(x) +  diff(range(x))/10 * 0:9)

# kolor zły?
hist(x, col="red")
hist(x, border = "green")

# może wypełnić liniami?
hist(x, density = 4, angle=140)

# ten tytuł też mi się nie podoba...
hist(x, main="To jest histogram próbki losowej")

# to jeszcze osie popraw
hist(x, ylab = "Częstotliwość")

# albo całkiem je wyrzuć
hist(x, xlab = "")

## określanie kolorów
# 1,nazwa
head(colours())
length(colours())
sample(colours(), size = 5)

# 2.składowe RGB
# format: #rrggbb lub #rrggbbaa
hist(x, col="#4F803050")
hist(x, col=rgb(0.5, 1, 0.3))
hist(x, col=rgb(200, 100, 30, maxColorValue = 255))


par(mar=rep(0,4))
plot.new()
plot.window(c(0,255),c(0,1))
for(i in 0:255){
    lines(c(i,i), c(0,1), col=rgb(44,77,0,i, maxColorValue = 255), lwd=4)
}

par(mar=rep(0,4))
plot.new()
plot.window(c(0,255),c(0,255))
for(i in 0:255){
  for(j in 0:255)
    lines(c(i,i), c(j,j), col=rgb(i,j,0, maxColorValue = 255), lwd=4)
}

par(mar=c(5,4,4,2))
# wykres kołowy
probka <- sample(c("K","M"),size=200, replace = TRUE)
probka <- sample(letters[1:5],size=200, replace = TRUE)
tab <- table(probka)
tab <- c(table(probka),x=1,y=1,z=1)
pie(2:5, labels=letters[1:4])
pie(table(probka))

#wieksze koło!
pie(tab, radius = 1)

# faktycznie koło czy wielokąt?
pie(tab, edges = 0)

for(i in 50:1){
  pie(tab, edges=i, main = i)
  #Sys.sleep(1)
}

# obrót
pie(tab)
pie(tab, init.angle = 180)
pie(tab, clockwise = TRUE)
pie(tab, clockwise = FALSE)

# wykres słupkowy
barplot(tab)
barplot(tab[4:8])

# jak przekłamać wyniki sondaży
barplot(tab, ylab = "Procent głosów", ylim = c(39,50))

# wykres skrzynkowy
boxplot(x)

# spłaszczony jak naleśnik, weź go obróć
boxplot(x, horizontal = TRUE)

## ---- Wizualizacja wielowymiarowa ----


mtcars
boxplot(mtcars$hp[mtcars$am=="a"], horizontal = TRUE)
boxplot(mtcars$hp[mtcars$am=="m"], horizontal = TRUE)
#wykresy skrzynkowe mogą być rysowana po grupach
boxplot(mtcars$hp ~ mtcars$cyl + mtcars$am)

mtcars %>% count(cyl, am)

# wykres rozrzutu (scatter plot)
plot(mtcars$hp, mtcars$mpg)


# wykres liniowy
x <- seq(-5,5, by=0.1)
y <- sin(x) * x^2 / 2 + sqrt(abs(cos(x-pi)))

plot(x,y)
plot(x,y, type = "l")
plot(x,y, type = "b")
plot(x,y, type = "c")
plot(x,y, type = "h")

# skala nie musi być liniowa
plot(x, exp(x), type="l", log = "y")

# wykresy w tytułach mogą mieć formuły matematyczne
plot(x,y,type="l", main = "sin(x)*x^2/2 + (|cos(x-pi)|)^(1/2)")
plot(x,y,type="l", main = expression(sin(x) * frac(x^2, 2)+integral(xdx,0,1) + sqrt(abs(cos(x-pi)))))

## wiele wykresów na jednym
x <- seq(-2*pi, 2*pi, by=0.1)
plot(x, sin(x), type="l")
plot(x, cos(x), type="l")

# funkcje lines i points rysują na aktualnym
plot(x, sin(x), type="l", col="red", lwd=2, lty=7, col.main="red", main="wykres")
lines(x, cos(x))

pie(rep(1, 50), col = rainbow(50), radius = 0.9)
# co jak się nie zgadza?

plot(x, sin(x), type="l", col="blue", ylim = range(sin(x), cos(x)+1))
lines(x, cos(x)+1, col = "red")

# wygodniej matplot matlines matpoints
matplot(x, cbind(sin(x), cos(x)+1, atan(x)*2), type = "l", lty=1:3, col=1:3)

# zapis do pliku
jpeg("nazwa.jpg", quality = 100)
matplot(x, cbind(sin(x), cos(x)+1, atan(x)*2), type = "l" )
dev.off()

bmp("nazwa.bmp")
matplot(x, cbind(sin(x), cos(x)+1, atan(x)*2), type = "l" )
dev.off()

pdf("plik.pdf")
matplot(x, cbind(sin(x), cos(x)+1, atan(x)*2), type = "l" )
dev.off()


## ---- pakiet ggplot2 ----
require(ggplot2)
require(dplyr)

mtcars$am <- factor(c("a","m")[mtcars$am+1])
mtcars$cyl <- factor(mtcars$cyl)

# podstawa to ggplot(dane, aes(co-x, co-y))
ggplot(mtcars, aes(am))

# powiedzieliśmy co, teraz jak
ggplot(mtcars, aes(am)) + geom_bar()
ggplot(mtcars, aes(hp)) + geom_histogram() 

# grupowanie

ggplot(mtcars, aes(x=cyl)) + geom_bar()
ggplot() + geom_bar(aes(x=cyl, fill=am), mtcars)
ggplot(mtcars, aes(x=cyl, fill=am)) + geom_bar(position = "dodge")

# za ciasno?
ggplot(mtcars, aes(x=cyl, fill=am)) + geom_bar(position = "dodge", width = .5)

# rozdzielamy grupy
ggplot(mtcars, aes(x=interaction(cyl, am))) + geom_bar(width = .8)


# dodajmy jakieś etykietki
meanHp <- mtcars %>% group_by(am, cyl) %>% summarise(hp=round(mean(hp)))

ggplot(meanHp, aes(x=interaction(cyl, am), y = hp, col="red")) + 
  geom_bar(stat = "identity",width = .5, show.legend = FALSE) +
  geom_text(aes(y=hp, label=hp), vjust=-1.5,col="blue")


countAmCyl <- mtcars %>% count(am, cyl)

ggplot(countAmCyl, aes(x=cyl, y = n, fill=am)) + 
  geom_bar(stat = "identity",width = .5, position = "dodge") +
  geom_label(aes(label=n),position=position_dodge(0.5), vjust=-1.5)

# histogram
ggplot(mtcars, aes(wt)) + geom_histogram()

#gęstość
ggplot(mtcars, aes(wt)) + geom_density()

# histogram i gęstość

ggplot(mtcars, aes(wt)) + geom_histogram(aes(y=..density..), bins=10)  + geom_density()

## liniowe wykresy
x <- seq(-2*pi, 2*pi, by=0.1)
dane <- data.frame(x, sin=sin(x), cos=cos(x)+2, atan=atan(x)*2, exp = exp(x))
ggplot(dane) + geom_line(aes(x, cos),col="red")
# chcemy zero na osi?
ggplot(dane) + geom_line(aes(x, cos),col="red") +
  ylim(0, max(dane$cos))

# punkty
ggplot(dane) + geom_point(aes(x, cos),col="green",fill="#808080FF", shape=21, size=5) +
  ylim(0, max(dane$cos))

# dziedziczenie estetyki
ggplot(dane,aes(x, cos)) + geom_point(col="green",fill="#808080FF", shape=21, size=5) +
  ylim(0, max(dane$cos))

# zmiana skali 
ggplot(dane,aes(x, exp)) + geom_point(col="green",fill="#808080FF", shape=21, size=5) +
  ylim(0, max(dane$cos)) + scale_y_log10()

# wiele lini
ggplot(dane, aes(x=x)) + geom_line(aes(y=cos),col="red") +
  geom_line(aes(y=sin),col="blue")


# przecież mamy dziedziczenie!
ggplot(dane, aes(x)) + geom_line(aes(y=cos),col="red", linetype="dashed", size=2.5) + 
  geom_line(aes(y=sin),col="blue")+
  geom_point(aes(y=sin), size=14, shape=4, col="green")

# dodajemy opisy
ggplot(dane, aes(x)) + geom_line(aes(y=cos),col="red", linetype="dashed", size=2.5) + 
  geom_line(aes(y=sin),col="blue")+geom_line(aes(y=atan),col="black")+
  geom_point(aes(y=sin), size=14, shape=4, col="green") +
  xlab(NULL) + ylab("oś Y") + ggtitle("To jest tytuł")


## Drugi wariant
dane2 <- data.frame(x=rep(dane$x, 3),funkcja=rep(c("sin","cos","atan"),each=nrow(dane)),
                    y=c(dane$sin, dane$cos, dane$atan))

ggplot(dane2, aes(x=x,y=y,fill=funkcja, shape=funkcja)) + geom_line() + geom_point() +
  scale_shape_manual(values=c(21, 24, 23)) + scale_fill_manual(values=c("black","green","blue"))


# wykres z pokolorowanym obszarem pod wykresem
ggplot(dane, aes(x,sin))  + geom_area()

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
ggsave("ggplot.png")

#domyślnie zapisuje ostatni wykres używając:
last_plot()

## wielowymiarowy wykres rozproszenia
pairs(mtcars[,2:7])

# wykresy są symetryczne, szkoda marnować miejsca
pairs(mtcars[,2:7], upper.panel = panel.smooth)

plotmatrix()

ggplot(mpg, aes(hwy, cty)) +
  geom_point(aes(color = cyl)) +
  geom_smooth(method ="lm") +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()
