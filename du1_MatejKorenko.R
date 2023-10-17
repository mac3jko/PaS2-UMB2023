# Matej Korenko - 16.10.2023 verzia 22:35
# Domaca Uloha 1

getwd()

#Set working directory 
setwd("D:/Skola/PaS2")


#Clearing old stuff
rm(list=ls())

#nacitanie kniznic
library(ggplot2movies)
library(dplyr)
library(ggplot2)

#importing dataset
data_books <- read.csv("bestsellers with categories.csv")

#--------------------------------------------------------------------------------
#1 Vykreslite si vzťah medzi cenou a hodnotením. Oplatí sa kúpiť drahšie knižky?

# inspiracia cvicenim 2.6

ggplot(data=data_books, aes(x = User.Rating, y = Price)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~ x)

#--------------------------------------------------------------------------------
#2 Nájdite 2 najvýraznejšie iné pozorovania na grafe a zistite, ktoré knihy to sú a prečo sú výrazne iné. Vykreslite graf bez týchto kníh.

# cez summary najdeme odlisnosti

data_books
summary(data_books)
data_books$Price
data_books$Reviews
# vypisanie zlych dat - pointless
# ind_ceny_knih <- (data_books$Price < 1) | (data_books$Reviews < 100)
# data_books[ind_ceny_knih,]
# 
# #filtrovanie dat  cez filter
# #filtrovanie dat cez kniznicu dplyr
# filtered_data <- data_books %>%
#   filter(Price > 1 & Reviews > 100)
# 
# #vykreslenie
# ggplot(data=filtered_data, aes(x = User.Rating, y = Price)) +
#   geom_point() +
#   geom_smooth(method="lm", formula = y ~ x)


#filtrovanie najvyssej ceny a najhorsieho ratingu
print("UL1_2 - Filtrujem knihy s cenou nad 75 a ratingom pod 3.7")
filtered_data_bonus <- data_books %>%
  filter(Price < 75 & User.Rating > 3.7)

ggplot(data=filtered_data_bonus, aes(x = User.Rating, y = Price)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~ x)

#--------------------------------------------------------------------------------
#3 Vezmite si knihy s viac ako 10 000 recenziami, s cenou medzi 10 a 20 dolárov,
# obsahujúce fikciu. Koľko z nich bolo bestsellerom práve v roku 2018?

data_books
#overenie akeho typu je zaner (ci nahodou nie boolean)
#is.character(data_books$Genre)

#filtrovanie dat cez kniznicu dplyr
filtered_data_3 <- data_books %>%
  filter(Reviews > 10000 
         & Price > 10 
         & Price < 20  
         & Genre == "Fiction" 
         & Year == 2018)

#dimenzia - rozmer datasetu
#chceme iba pocet riadkov - pocet odfiltrovanych knih
dimenzia <- dim(filtered_data_3)
pocet_bestsellerov <- dimenzia[1] #prvy rozmer su riadky

sprintf("UL1_3 pocet bestsellerov je %d",pocet_bestsellerov)

#--------------------------------------------------------------------------------
#4 Vypíšte názvy 10 najlepšie hodnotených a 10 najhoršie hodnotených kníh s viac ako 15 000 recenziami,
# ktoré stoja menej ako 15 dolárov. Vysvetlite, prečo sa niektoré knihy opakujú.
data_books

#Podmienka pre indexovanie
ind_overene_lacne <- (data_books$Price < 15 & data_books$Reviews > 15000)

#Zoradenie podla hodnoteni
ind_OvLa_najhorsi = order(data_books[ind_overene_lacne,]$User.Rating)[1:10]
ind_OvLa_najlepsi = order(data_books[ind_overene_lacne,]$User.Rating, decreasing=TRUE)[1:10]

#data_books[ind_overene_lacne,][ind_OvLa_najhorsi,]
#data_books[ind_overene_lacne,][ind_OvLa_najlepsi,]

#spojenie zoradeni do jedneho vektora
ind_OvLa_sum = c(ind_OvLa_najhorsi, ind_OvLa_najlepsi)
#vypis
sprintf("UL1_4 Knihy sa opakuju z dovodu roznych vydani rovnakych knih - ine roky vydania")
sprintf("UL1_4 10 najlepsich/najhosich knih je")
data_books[ind_overene_lacne,][ind_OvLa_sum,]


#--------------------------------------------------------------------------------
#5 Zistite, aká je priemerná cena za knihy J.K. Rowlingovej, 
# ktoré sa nachádzajú v tomto rebríčku. Koľko majú tieto knihy v priemere recenzií?


#hladanie podla Nazvu - chce som cez str_detect=, ale dalo sa lahsie
filtered_data_5 <- data_books %>%
  filter(Author == "J.K. Rowling")

#priemer cien
sprintf("UL1_5 Priemer cien J.K. Rowling %f", mean(filtered_data_5$Price))

#priemer ratingu
sprintf("UL1_5 Priemer ratingu J.K. Rowling %f", mean(filtered_data_5$User.Rating))


                                                                       #--------------------------------------------------------------------------------
#-----------------------------BONUS---------------------------------------------
#Pomocou cyklu alebo funkcie zistite, v ktorých rokoch prevláda fikcia medzi 20 najlepšie hodnotenými knihami daného roku.
#Vysvetlite, ako postupujete, aby sme tam mali každú knihu len raz.
#V roku, kedy bol pomer fikcie najväčší, vypíšte pre top 20 kníh názov, autora a to, či je to fikcia.

#?? este sa na to pozriem

#--------------------------------------------------------------------------------
#Cvičenie 3.13 (DÚ 1.2, guličky) Z vrecúška vyťahujete guličky s číslami od 1 po 20 bez vracania.
#Simuláciou zistite, či je pravdepodobnejšie, že na 3 vytiahnutých guličkách je dokopy súčet 20 alebo na 5 guličkách je súčet 35.

#podla cvicenia 3.1
set.seed(5102023)

#generovanie random cisla od 1 po 20
hodGulickou<- function(){
  return(ceiling(runif(1)*20))
}

hodGulickou()
 #inicializacia premennych
pocetSim <- 100000
suma3gulicky <- numeric(pocetSim)
suma5gulicky <- numeric(pocetSim)
count3gulicky = 0
count5gulicky = 0

for (iHod in 1:pocetSim){
  #sucet hodov
  suma3gulicky[iHod] <- hodGulickou()+hodGulickou()+hodGulickou()
  suma5gulicky[iHod] <- hodGulickou()+hodGulickou()+hodGulickou()+hodGulickou()+hodGulickou()
  #pocitanie kolkokrat padol isty pripad
  if(suma3gulicky[iHod] == 20){
    count3gulicky = count3gulicky + 1
  }
  if(suma5gulicky[iHod] == 35){
    count5gulicky = count5gulicky + 1
  }

}
#count3gulicky
#count5gulicky
#vypis porovnania <1 / >1
#(porovnanie = count3gulicky/count5gulicky)
#vypis statistik
sprintf("UL1.2 Pravdpodobnost vysledku suctu 20 pri troch gulickach je %f ", count3gulicky / pocetSim)
sprintf("UL1.2 Pravdpodobnost vysledku suctu 35 pri piatich gulickach je %f ", count5gulicky / pocetSim)



#--------------------------------------------------------------------------------
# Cvičenie 3.14 (DÚ 1.3, vzorec) Pomocou počítača vypočítajte

suma1 <- 0
for (i in 5:100){
  suma1 <- suma1 + ((i)/((i+1)*(i-1)))
}
sprintf("UL1.3 Vysledok prvej sumy je %f ",suma1)

 #cez for s velkym poctom opakovani
suma2 <- 0
for (i in 2:10000000){
  suma2 <- suma2 + 4/((i+1)*(i-1))
}
sprintf("UL1.3 Vysledok druhej sumy je %f ",suma2)


#cez While za pomoci interrupcie, zaciname od 2
i = 2
suma2_1 = 0
while (TRUE) {
  suma2_1 <- suma2_1 + 4/((i+1)*(i-1))
  i = i + 1 #lebo while v sebe nema index opakovania
}
sprintf("Prerus cyklus")
sprintf("UL1.3 Vysledok prvej sumy cez while je %f ",suma2)

#--------------------------------------------------------------------------------
#Cvičenie 3.15 (DÚ 1.4, porota, cvičenie 3.10) Piati ľudia v porote majú nasledovné šance,
#že sa správne rozhodnú: 95%, 80%, 90%, 82%, 99%. O výsledku rozhodne väčšina.
#Aká je šanca, že sa celá porota zhodne na správnom výsledku?

#vektor pravdepodobnosti rozhodcov
por_prob <- c(0.95, 0.80, 0.90, 0.82, 0.99)
rozhodcovia = numeric(5)

nSim = 10000

#pre overenie spravnosti treba zmenit seed
set.seed(20231000)  # 0.5583
set.seed(20231005)  # 0.5488

## prikaz pre zhodu vsetkych 5, vacsina nizsie-----------------------------

#simulacia vysledku s parametrami poctu simulacii a vektora pravdepodobnosti porotcov
simVysledku <- function(nSim, por_prob){
  count = 0
  for (iSim in 1:nSim){ # kolko pokusov suboju chcem nasimulovat
    for(iPor in 1:5){   #mame 5 porotcov, prejdeme kazdeho po jednom, iPor - index porotcu
      #pre kazdeho rozhodcu ziskame 0/1 podla pravdepodobnmosti z vektora
      rozhodcovia[iPor] <- rbinom(1,1,por_prob[iPor])
    }
    if(sum(rozhodcovia) == 5){  #ak sa zhodli, priratame k poctu zhodnutych
      count = count + 1
    }
  }
  return(count)
}
#vzorec "pocet spravnych/pocet vsetkych"
results = simVysledku(nSim, por_prob)/nSim
sprintf("UL1.4 Pravdepodobnost zhody vsetkych je %f ", results)

## prikaz pre zhodu vacsiny-----------------------------

#simulacia vysledku s parametrami poctu simulacii a vektora pravdepodobnosti porotcov
simVysledku1 <- function(nSim, por_prob){
  count = 0
  for (iSim in 1:nSim){ # kolko pokusov suboju chcem nasimulovat
    for(iPor in 1:5){   #mame 5 porotcov, prejdeme kazdeho po jednom, iPor - index porotcu
      #pre kazdeho rozhodcu ziskame 0/1 podla pravdepodobnmosti z vektora
      rozhodcovia[iPor] <- rbinom(1,1,por_prob[iPor])
    }
    if(sum(rozhodcovia) > 2 ){  #ak sa zhodli, priratame k poctu zhodnutych
      count = count + 1
    }
  }
  return(count)
}
#vzorec "pocet spravnych/pocet vsetkych"
results1 = simVysledku1(nSim, por_prob)/nSim
sprintf("UL1.4 Pravdepodobnost zhody vacsiny je %f ", results1)


