#### cvicenia 2023-09-21
setwd("D:/Skola/PaS2")


#### cvicenie 1.1 ####

films <- c("Terminator 3", "Titanic", "Matrix", "Love actually", "The Godfather","12 Angry Men","The Dark Knight")
score <- c(99,92,87,93,98,97,92)
time <- c(109,194,136,135,175,96,152)

films

# vytvorit vektor own_score

own_score <- c(99,90,87,93,96,97,88)
own_score

# spravime z toho data frame
?data.frame

db <- data.frame(films, score, time, own_score)

# pozrieme si data frame
db

# ked chceme len vektor casov
db$time

# ked chceme len casy dlhsie ako 120
db$time>120

# mozeme si ich indexy ulozit
ind_120 <- db$time>120

# prve 3 casy
db$time[1:3]
# casy/skore/filmy dlhsie ako 120 minut (volam pomocou tych ulozenych indexov)
db$time[ind_120]
db$score[ind_120]
db$films[ind_120]

# priemer dlzky pre 3 najneoblubenejsie filmy
?sort
?order

# SORT: zoradi, ale tazsie sa dostak k informacii, ktory film to je
# dobre, ak chcem vediet len hodnotu
sort(db$own_score)

# ORDER: vypise poradie indexov, na ktore sa mam pozerat, ak to chcem zoradene
# ulozim si tie indexy do vektora s nazvom poradie
# vyisem si prve 3
poradie <- order(db$own_score)
poradie[1:3]
# [1] 3 7 2
# tj. najhorsie skore na indexe 3, potom na 7, treti najhorsi na indexe 2

# vypisem si nazvy a cas troch nami najhorsie ohodnotenych filmov
# a zistim si priemerny cas tychto troch filmov

db$films[poradie[1:3]]
db$time[poradie[1:3]]
mean(db$time[poradie[1:3]])

# zobrazime vlastne hodnotenie voci tomu zo zadania
plot(db$own_score, db$score, pch = 16)
# ak si to chcem porovnat voci priamke prechadzajucej cez stred (cez 0)
# so smernicou 1
abline(0, 1, col = "red")

# zistujeme, ci mame radsej dlhsie alebo kratsie filmy
plot(db$time, db$own_score, pch = 16)

# ak si chcem pozriet kazdu ciselnu voci kazdej
pairs(db[, c(2:4)])

pairs(db[, c(3:4)])

# riadok 1, stlpec 3
db[1,3]


#### cvicenie 1.2 ####

# pozrieme sa na to, co robi seq a rep
?seq
1:10
seq(1,10, 2)
# postupnost od 1 po 10 po 2 krokoch
?rep

rep(x=10, times = 5)
rep(x=1:9, times = 14:6)

matrix(rep(x=2:7, times = 10:5), nrow = 10, ncol = 10, byrow = TRUE)

# ak si to chceme ulozit do matice mm a
# zaroven si to aj vypisat v jednom riadku, tak dam zatvorky okolo
# (inak by sa to len ulozilo, ale nevypisalo)
(mm = matrix(rep(x=1:9, times = 14:6), nrow = 10, ncol = 9, byrow = TRUE))


#### cvicenie 1.4 ####
rep(seq(1,9,2)*2,2)+2
length(rep(seq(1,9,2)*2,2))
length(rep(seq(1,9,2)*2,2)+2)+2
sqrt(sqrt(9*(length(rep(seq(1,9,2)*2,2)+2)-1)))

# rozmyslame odvnutra
## 1 3 5 7 9
# vynasobene o 2
## 2 6 10 14 18
# zopakujem 2x tuto postupnost
## 2 6 10 14 18 2 6 10 14 18

#### CHYBA
## 2 6 10 14 18 2 6 10 14 18 kazdemu clenu sa pripocita 2


# dlzka tohto minus jedna
## 10-1 = 9
# toto vynasobim *9
## 81
# odmocnina z 81
## 9
# odmocnina z 9
## 3




#### cvicenie 1.3 ####

install.packages("ggplot2movies")
library(ggplot2movies)
movies
head(movies)

# pozriet doma, co vsetko je v tom datasete