#Microsoft stocks data years: 2015-2021

library(ggplot2) # Budowanie biblioteki ggplot2

# ustawienie working directory na folder, w kt�rym przechowujemy nasz plik
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Wczytywanie danych z pliku CSV
df <- read.csv(file='data.csv', sep=',')


# Formatowanie daty i przypisywanie kolumn do zmiennych
date <- df[[1]]
bdate <- as.Date(date, format="%m/%d/%Y")
open <- df[[2]]
close <- df[[5]]
volume <- df[[6]]
year <- strftime(bdate, format="%y")
#yearMonth <- strftime(bdate, format="%Y %m")
month <- strftime(bdate, format="%m")
wday <- strftime(bdate, format="%u")


#Deklaracja kolor�w u�ytych jako motyw przewodni
col1 <- "salmon"
col2 <- "salmon4"


# Wykres liniowy zestawiaj�cy ceny 'Open' i 'Close' ka�dego dnia
colors <- c("Open" = col1, "Close" = col2)
ggplot( 
) + geom_line(
  data = data.frame(y = open, x = bdate),
  aes(y = open, x = bdate, color = "Open")
) + geom_line(
  data = data.frame(y = close, x = bdate),
  aes(y = close, x = bdate, color = "Close")
) + labs(
  x = "Czas",
  y = "Warto�c akcji w dolarach",
  color = "Legenda"
)


# Wykres punktowy przedstawiaj�cy ilo�c zakupionych akcji ka�dego dnia
colors <- c("Volume" = col2)
ggplot( 
) + geom_point(
  data = data.frame(y = volume, x = bdate),
  aes(y = volume/1000000, x = bdate, color = "Volume"),
  size = 0.6
) + labs(
  x = "Czas",
  y = "Ilo�� zakupionych  dla ka�dego dnia [w milionach]",
  color = "Legenda"
)


# Wykres liniowy pokazuj�cy rosn�ca tendencj� akcji(warto�ci 'Open') 
# Wykres punktowy reprezentuj�cy ceny akcji(warto�� 'Open') 
ggplot(
  data.frame(y = open, x = bdate),
  aes(y = open, x = bdate)
) + geom_point(colour = col1) + 
  stat_smooth(colour = col2) +
  labs(
    title = "Warto�� akcji w latach 2015-2021",
    y = "warto�� otwarcia w dolarach",
    x = "Czas"
  )
'''
radial <- coord_polar(theta="x", direction=1)
bar <- geom_bar(aes(fill = x), stat = "identity")
'''


# Wykres pude�kowy przedstawiaj�cy r�nic� pomi�dzy warto�ciami 'Open i 'Close;
boxplot(x = close - open, col = col1, border = col2, notch = FALSE, horizontal = TRUE, xlab = "R�nica pomi�dzy warto�ciami przy zamkni�ciu oraz otwarciu gie�dy")



# Wykres pude�kowy wykazuj�cy procentowe odchylenie 
# cen 'Open' od 'Close' pogrupowane na kolejne lata
d <- data.frame((close - open)/close*100)
dn <- aggregate(d, by=list(year), c)
data <- c(data.frame("Rok 15" = dn[[2]][[1]]), 
          data.frame("Rok 16" = dn[[2]][[2]]),
          data.frame("Rok 17" = dn[[2]][[3]]),
          data.frame("Rok 18" = dn[[2]][[4]]),
          data.frame("Rok 19" = dn[[2]][[5]]),
          data.frame("Rok 20" = dn[[2]][[6]]),
          data.frame("Rok 21" = dn[[2]][[7]]))
boxplot(
  data, 
  xlab = "",
  ylab ="wzgl�dna r�nica [%]", 
  col=c(col1, 
        col2, 
        col1, 
        col2, 
        col1, 
        col2, 
        col1),
  notch = TRUE)


# �rednia warto�� dziennego handlu akcjami
mean(open*volume)
# Mediana warto�ci dziennego handlu akcjami
median(open*volume)
# Wariancja cen akcji
var(open)
# Odchylenie standardowe cen akcji
sd(open)
# Korelacja pomi�dzy cenami akcji a ilo�ci� wymienianych akcji
cor(open, volume) 


# Wykres przedstawiaj�cy �rednie ceny akcji dla ka�dego miesi�ca
# Analogiczne miesi�ce kolejnych lat zosta�y pogrupowane
monthsOne <- table(month)
namesMonth <- names(monthsOne)
mOpen = aggregate(open, by=list(month), mean)
ggplot(
  data.frame(y = mOpen[[2]], x = namesMonth),
  aes(y = mOpen[[2]], x = namesMonth)
) + geom_col(aes( fill=y)) +  scale_fill_gradient(low=col2, high=col1) + labs(
  x = "Miesi�c",
  y = "Warto�� w dolarach",
  title = "�rednia warto�� akcji dla danego miesi�ca (cena otwarcia)",
  subtitle = "Adekwatne miesi�ce z kolejnych lat zosta�y u�rednione",
  fill = "Legenda"
)


# Wykres przedstawiaj�cy �rednie ceny akcji dla ka�dego dnia tygodnia
nameswday <- names(table(wday))
x <- c("1" = "Pon", "2" = "Wt", "3" = "�r", "4" = "Czw", "5" = "Pt")
wOpen = aggregate(open, by=list(wday), mean)
ggplot(
  data.frame(y = wOpen[[2]], x = nameswday),
  aes(y = wOpen[[2]], x = nameswday)
) + geom_col(aes( fill=y)) +  scale_fill_gradient(low=col2, high=col1) + labs(
  x = "Robocze dni tygodnia (pon..pt = 1..5)",
  y = "Warto�� w dolarach",
  title = "�rednia warto�� akcji dla danego dnia tygodnia (cena otwarcia)",
  subtitle = "Adekwatne dni z kolejnych tygodni/miesi�cy/lat zosta�y u�rednione",
  fill = "Legenda"
)

  



