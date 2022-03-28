#Microsoft stocks data years: 2015-2021

library(ggplot2) # Budowanie biblioteki ggplot2

# ustawienie working directory na folder, w którym przechowujemy nasz plik
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


#Deklaracja kolorów u¿ytych jako motyw przewodni
col1 <- "salmon"
col2 <- "salmon4"


# Wykres liniowy zestawiaj¹cy ceny 'Open' i 'Close' ka¿dego dnia
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
  y = "Wartoœc akcji w dolarach",
  color = "Legenda"
)


# Wykres punktowy przedstawiaj¹cy iloœc zakupionych akcji ka¿dego dnia
colors <- c("Volume" = col2)
ggplot( 
) + geom_point(
  data = data.frame(y = volume, x = bdate),
  aes(y = volume/1000000, x = bdate, color = "Volume"),
  size = 0.6
) + labs(
  x = "Czas",
  y = "Iloœæ zakupionych  dla ka¿dego dnia [w milionach]",
  color = "Legenda"
)


# Wykres liniowy pokazuj¹cy rosn¹ca tendencjê akcji(wartoœci 'Open') 
# Wykres punktowy reprezentuj¹cy ceny akcji(wartoœæ 'Open') 
ggplot(
  data.frame(y = open, x = bdate),
  aes(y = open, x = bdate)
) + geom_point(colour = col1) + 
  stat_smooth(colour = col2) +
  labs(
    title = "Wartoœæ akcji w latach 2015-2021",
    y = "wartoœæ otwarcia w dolarach",
    x = "Czas"
  )
'''
radial <- coord_polar(theta="x", direction=1)
bar <- geom_bar(aes(fill = x), stat = "identity")
'''


# Wykres pude³kowy przedstawiaj¹cy ró¿nicê pomiêdzy wartoœciami 'Open i 'Close;
boxplot(x = close - open, col = col1, border = col2, notch = FALSE, horizontal = TRUE, xlab = "Ró¿nica pomiêdzy wartoœciami przy zamkniêciu oraz otwarciu gie³dy")



# Wykres pude³kowy wykazuj¹cy procentowe odchylenie 
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
  ylab ="wzglêdna ró¿nica [%]", 
  col=c(col1, 
        col2, 
        col1, 
        col2, 
        col1, 
        col2, 
        col1),
  notch = TRUE)


# Œrednia wartoœæ dziennego handlu akcjami
mean(open*volume)
# Mediana wartoœci dziennego handlu akcjami
median(open*volume)
# Wariancja cen akcji
var(open)
# Odchylenie standardowe cen akcji
sd(open)
# Korelacja pomiêdzy cenami akcji a iloœci¹ wymienianych akcji
cor(open, volume) 


# Wykres przedstawiaj¹cy œrednie ceny akcji dla ka¿dego miesi¹ca
# Analogiczne miesi¹ce kolejnych lat zosta³y pogrupowane
monthsOne <- table(month)
namesMonth <- names(monthsOne)
mOpen = aggregate(open, by=list(month), mean)
ggplot(
  data.frame(y = mOpen[[2]], x = namesMonth),
  aes(y = mOpen[[2]], x = namesMonth)
) + geom_col(aes( fill=y)) +  scale_fill_gradient(low=col2, high=col1) + labs(
  x = "Miesi¹c",
  y = "Wartoœæ w dolarach",
  title = "Œrednia wartoœæ akcji dla danego miesi¹ca (cena otwarcia)",
  subtitle = "Adekwatne miesi¹ce z kolejnych lat zosta³y uœrednione",
  fill = "Legenda"
)


# Wykres przedstawiaj¹cy œrednie ceny akcji dla ka¿dego dnia tygodnia
nameswday <- names(table(wday))
x <- c("1" = "Pon", "2" = "Wt", "3" = "Œr", "4" = "Czw", "5" = "Pt")
wOpen = aggregate(open, by=list(wday), mean)
ggplot(
  data.frame(y = wOpen[[2]], x = nameswday),
  aes(y = wOpen[[2]], x = nameswday)
) + geom_col(aes( fill=y)) +  scale_fill_gradient(low=col2, high=col1) + labs(
  x = "Robocze dni tygodnia (pon..pt = 1..5)",
  y = "Wartoœæ w dolarach",
  title = "Œrednia wartoœæ akcji dla danego dnia tygodnia (cena otwarcia)",
  subtitle = "Adekwatne dni z kolejnych tygodni/miesiêcy/lat zosta³y uœrednione",
  fill = "Legenda"
)

  



