#### cvicenia 2023-10-05

ls()
rm(list=ls())

#### Cvicenie 2.6 ####

# na nacitavanie csv treba Rku povedat, z ktoreho adresara ho ma tahat
# treba nastavit working directory
# getwd()
# setwd()

# session --> set working directory ---> choose/to source file location
# mozem si to napisat sem, ak ten working directory nebudem menit

# setwd("C:/Users/------/2023/pas2ex")


data_wine <- read.csv("winequality-red.csv")

plot(data_wine$alcohol,data_wine$quality)
with(data_wine,plot(alcohol,quality))

library(ggplot2)


ggplot(data=data_wine, aes(x = alcohol, y = quality)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~ x)

library(GGally)
# https://r-charts.com/correlation/ggpairs/


ggpairs(data_wine,                 # Data frame
        columns = c("quality","fixed.acidity","residual.sugar","chlorides","pH","alcohol"),        # Columns
        aes(alpha = 0.5))     # Transparency

# ak to chceme farebne podla jednotlivych skupin (kvalitu)
# musime tu premennu ale dat ako factor
is.factor(data_wine$quality)
# je na to funkcia as.factor, skontrolujeme si ju

is.factor(as.factor(data_wine$quality))

# vidime, ze uz to bude faktorova premenna, ale nezmenili sme povodnu premmenu

ggpairs(data_wine,                 # Data frame
        columns = c("fixed.acidity","residual.sugar","chlorides","pH","alcohol"),        # Columns
        aes(color = as.factor(quality),  # Color by group (cat. variable)
            alpha = 0.5))     # Transparency


#### Cvicenie 2.7 ####


data_student <- read.csv("StudentsPerformance.csv")

summary(data_student)

# vidime, ze vzdelanie rodica nie je faktorova premenna

data_student$parental.level.of.education <- as.factor(data_student$parental.level.of.education)

# skontrolujeme, ci je to uz faktorova premenna
is.factor(data_student$parental.level.of.education)
summary(data_student)
# vidime aj v sumamry, ze uz to je faktorova

# chceme si zobrazit napr. skore z matiky a citania pre deti rodicov, ktori maju len nejaku strednu skolu
library(dplyr)

ggplot(data=data_student %>% filter(parental.level.of.education=="some high school"), aes(x = math.score, y = reading.score,col=parental.level.of.education)) +
  geom_point() 

# pozrieme si aj deti rodicov s vysokou skolou, ci sa to nejako prejavi

ggplot(data=data_student %>% filter(parental.level.of.education %in% c("some high school","master's degree")), aes(x = math.score, y = reading.score,col=parental.level.of.education)) +
  geom_point() 

# vyzera, ze ak rodicia boli na vysokej, deti maju vyssie skore v matike aj v citani




#### KAPITOLA 3 ####



#### KAPITOLA 3

#### Cvicenie 3.1 ####

# hod kockou pomocou runif
# hodit 100 krat a histogram

# reprodukovatelnost kodu zabezpecime pomocou set seedu

set.seed(5102023)

?runif

hodKockou <- function(){
  return(ceiling(runif(1)*6))
}

hodKockou()

hody <- numeric(100)
for (iHod in 1:100){
  hody[iHod] <- hodKockou()  
}

round(table(hody)/100,2)

hist(hody)
?hist
hist(hody, breaks = 0:6)


#### Cvicenie 3.2 ####

# hadzem troma kockami a pozeram sa na sucet

pocetSim <- 100000
suma3kocky <- numeric(pocetSim)
for (iHod in 1:pocetSim){
  suma3kocky[iHod] <- hodKockou()+hodKockou()+hodKockou()  
}
suma3kocky

plot(table(suma3kocky)/pocetSim)


#### Cvicenie 3.3 ####

# ak ho zafixujeme v kode, tak vzdy dostaneme rovnaku hodnotu



#### Cvicenie 3.4 ####


cisla <- c("2","3","4","5","6","7","8","9","10","J","Q","K","A")
farby <- c("srdce","list","zalud","gula")
karty <- outer(cisla,farby,paste)

karty # pozrieme sa, ako vyzeraju nase karty

ruka <- sample(karty,5) # na ruku si vezmem 5 kariet

# chcem vediet, ci mam postupku
# ked sa oizreim na nase karty

# ked chcem vediet, kde sa na tom mojom poli nachadza prva karta
which(karty==ruka[1],arr.ind=TRUE)

# teraz potrebujem zistit, kde sa nachadzaju vsetky nase karty

iR <- rbind(which(karty==ruka[1],arr.ind=TRUE),
            which(karty==ruka[2],arr.ind=TRUE),
            which(karty==ruka[3],arr.ind=TRUE),
            which(karty==ruka[4],arr.ind=TRUE),
            which(karty==ruka[5],arr.ind=TRUE))

# ak si chcem pozriet, ci mam postupku, zaujima nas, v ktorom riadku su
sort(iR[,1])

# chcem postupku - potrebujem zistit, ake rozdiely mam medzi kartami
diff(sort(iR[,1]))
# tu mam rozdiely

# ak mam mat postupku, tak vsetky 4 rozdiely musia byt rovne jednotke

diff(sort(iR[,1]))==c(1,1,1,1)

# ak by boli vsetky rovne 1, tak tam mam 4*true, teda suma by bola 4
# staci mi teda pozerat na sumu, ci je rovna 4


sum((diff(sort(iR[,1]))==c(1,1,1,1)) == 4)
# ak nie je, a dostanem 0, znamena to, ze postupku som nemala

# teraz to idem zaimplementovat do funkcie

simKarty <- function(nSim){
  resPost <- numeric(nSim)
  for (iSim in 1:nSim){
    ruka <- sample(karty,5)
    iR <- rbind(which(karty==ruka[1],arr.ind=TRUE),
                which(karty==ruka[2],arr.ind=TRUE),
                which(karty==ruka[3],arr.ind=TRUE),
                which(karty==ruka[4],arr.ind=TRUE),
                which(karty==ruka[5],arr.ind=TRUE))
    resPost[iSim] <- (sum(diff(sort(iR[,1]))==c(1,1,1,1))==4)
  }
  return(resPost)
}

postupky <- simKarty(10000)
sum(postupky)/length(postupky)



#### Cvicenie 3.5 ####
# nahodne vybrat 200 akcnych filmov, starsich ako 20 rokov
# zobrazit vztah medzi hodnotenim a dlzkou filmu
# rozne vzorky a pozerat sa na to, ako sa tento vztah meni

library(ggplot2movies)
library(dplyr)

# vyfiltrujem si, co potrebujem

act_old <- movies %>% filter(Action==1,
                             year<2002)

# pozriem si, z kolkych riadkov vyberame
nRows  <- dim(act_old)[1]

# uz samplujem
sampledRows <- sample(1:nRows,200)
randSample  <- act_old[sampledRows,]
with(randSample,plot(length,rating))



#### Cvicenie 3.6 ####

# palicu zlomim na dvoch miestach
# viem vytvorit trojuholnik?
# musi platit trojuholnikova nerovnost:
# sucet dlzok dvoch lubovolnych stran je vacsi ako dlzka tretej strany

# spravim si funkciu, ktora mi situaciu nasimuluje, kolkokrat chcem

simTriangle <- function(nSim){
  triangleResult <- numeric(nSim)  # sem budem ukladat vysledky
  for (iSim in 1:nSim){  # tolkoto simulacii chcem
    break1 <- runif(1)  # zlomim prvykrat (zaujima nas "pomer" stran)
    break2 <- runif(1) # zlomim druhykrat
    
    # teraz potrebujem vypocitat dlzku jednotlivych papekov
    x <- min(break1,break2) - 0  # prvy ide od kraja (0) po mensi zo zlomov
    y <- max(break1,break2) - min(break1,break2)  # od jedneho zlomu po druhy
    z <- 1 - max(break1,break2)  # od kraja (1) po vacsi zlom
    
    # idem overovat trojuholnikovu nerovnost
    triangleResult[iSim] <- ( (x + y > z) + (x + z > y) + (y + z > x) ) == 3
    # da sa aj pomocou "nieco a nieco a nieco == true", ale takto to je jednoduchsie
  }
  
  # potrebujem si vypisat vysledky - TRUE/FALSE
  return(triangleResult)
}


pokusM <- simTriangle(1000000)
mean(pokusM)


#### Cvicenie 3.8 ####

# Adam triafa prvy, s pravdepodobnostou 30%
# Bozena triafa, ak sa netrafi Adam, ona trafi s ravdepodobnostou 35%

# kto ma vaciu sancu na vyhru?
# aka by musea byt Bozenina presnost, aby bol suboj spravodlivy?


set.seed(20231005)

simSuboj <- function(nSim, pravd = 0.35){
  strelRes <- numeric(nSim)  # sem si ulozim vysledky
  for (iSim in 1:nSim){ # kolko pokusov suboju chcem nasimulovat
    sAdam <- rbinom(1,1,0.3) # vygenerujem z binomickeho s p=0.3
    sBozena <- rbinom(1,1,pravd)   # vygenerujem z binomickeho s p=0.3
    strelRes[iSim] <- (sAdam < sBozena) # zaujima nas, ci vyhrala Bozena
  }
  return(strelRes)
}

vyhryBoz <- simSuboj(100000)
mean(vyhryBoz)

# aku by musela mat Bozena presnost, aby bol zapas spravodlivy?
vyhryBoz <- simSuboj(100000, pravd = 0.5)
mean(vyhryBoz)

vyhryBoz <- simSuboj(100000, pravd = 0.72)
mean(vyhryBoz)

# pri Bozeninej presnosti 0.72 to vyzera cca spravodlivo


#### Cvicenie 3.9 robili na hodine ####


simStret <- function(nSim){
  res <- numeric(nSim)
  for (iSim in 1:nSim){
    tA <- runif(1) # vygenerujem, kedy na usecke medzi 10 a 11 pride Astrian
    tB <- runif(1) # vygenerujem, kedy na usecke medzi 10 a 11 pride Barbora
    res[iSim] <- abs(tA-tB) < (1/6) # zistim, ci rozdiel abs. hodnot je mensi ako 10 minut, co je 1/6 z usecky (1/6 z hodiny)
  }
  return(res)
}

a <- simStret(1000000)
mean(a)



#### Cvicenie 3.11 ####

# aka je sanca, ze na tomto kurze bude dvojica s rovnakym datumom narodenia
# ako by sa to dalo?
# nasimulujem si den narodenia pre 50 ludi

narod <- sample(1:365,50,replace=TRUE) # replace true, lebo narodeniny mozu byt aj v ten isty den
# pozriem sa do tabulky
tblNarod <- table(narod)
tblNarod

# ak v nejakom chlieviku vidim aspon 2 kusy, znamena to, ze maju narodeniny v ten isty den
# nasimulujeme teda velakrat, ze aka je sanca, ze nsim ludi z kurzu bude mat narodeniny v ten isty den

sum(tblNarod==1)
50-sum(tblNarod==1) # v tolkychto chlievikoch je cosi ine ako 1
sum(tblNarod==1)==50 # vidime, ze sa to nerovna, lebo v nejakom chlieviku som mala viac narodenin

# mna zaujima len to, ci maju narodeniny viacery alebo kazdy po samom
# chcem teda len logicku premennu TRUE, ak maju narodky aj viaceri, FALSE, ak kazdy po samom

# posledny riadok kodu mi dava FALSE na to, ?o ja chcem TRUE
# teda potrebujem to jednoduchou upravou naopak
1-(sum(tblNarod==1)==50) 
# niekto ma narodeniny naraz --> dostanem 1

# teraz to uz len implementujem do funkcie

simNarod <- function(nSim,nPersons){
  resNarod <- numeric(nSim)
  for (iSim in 1:nSim){
    daysB <- sample(1:365,nPersons,replace=TRUE) # nasimulujeme narodeniny
    tbl <- table(daysB) # spravim z toho tabulku
    resNarod[iSim] <- 1-(sum(tbl==1)==nPersons)
  }
  return(resNarod)
}

resTF <- simNarod(1000,20)
mean(resTF)

# pri 20 ludoch by som cca v 40% tried mala ludi, co sa narodili v ten isty den
