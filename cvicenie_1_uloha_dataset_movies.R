#### cvicenie 1.3 ####
setwd("D:/Skola/PaS2")
# z minula sme uz mali
# ak uz mame nainstalovane, tak znova netreba
# install.packages("ggplot2movies")
library(ggplot2movies)
movies
head(movies)

#### cvicenie 1.3 ####

?movies

### ktore filmy dominuju na trhu - review
summary(movies)

# drama, komedia, short

#------------------------KOMEDIE PRIKLADY---------------------------------------

### najstarsi komedialny film
movies$Comedy == 1

# ulozim indexy comedy filmov a pozriem sa na ich roky
ind_comedy <- (movies$Comedy == 1)
year_comedy <- movies[ind_comedy, ]$year

# pozriem, kolko mam rokov pri komediach
length(year_comedy)

# ak chcem len zistit rok - zoradim, alebo pozriem minimum
sort(year_comedy)
min(year_comedy)
# [1] 1896

# keby som chcela teraz hladat, na akom indexe to je
# musim manualne najst, na akych indexoch sa nachazaju komedie s takym rokom
which(movies[ind_comedy, ]$year == min(year_comedy))
# [1] 16054
# len na tomto jednom riadku
# zobrazim si prislusny riadok
# (pozor!, len z casti komedialnych filmov)
movies[ind_comedy, ][16054,]

#ak bz sme chceli cez indexovanie
najst_comedy = which(movies[ind_comedy, ]$year == min(year_comedy))
#vypis nazvu a roku danej komedie
movies[ind_comedy,1:2][16054,]

# vidime, ze je to comedy z daneho roku

# ina, priamociarejsia moznost pomocou order
order(year_comedy)
# vypise nam indexy, na ktore sa mame pozerat,
# ak to chceme zoradene od najmensieho

# pozriem sa na prvy index, na ktory mam pozerat (teda na najstarsi film)
com_old_ind <- order(year_comedy)[1]
year_comedy[com_old_ind]
# ak chcem vediet informacie o filme
movies[ind_comedy, ][com_old_ind,]
# ak chcem vediet nazov filmu (nazov je prvy index v polozkach informacii o filme)
movies[ind_comedy, 1][com_old_ind,]

# keby som teraz chcela 4 najstarsie
?order
com_old_4 <- order(year_comedy)[1:4]
movies[ind_comedy, ][com_old_4,]

# keby som teraz chcela 4 najnovsie
?order
com_old_4 <- order(year_comedy, decreasing = TRUE)[1:4]
movies[ind_comedy, 1:2][com_old_4,]

# pozor, tych z roku 1898 moze byt aj viac, vypise prvy 
# lepsie pomocou order, ak to chcem potom aj pozerat
#--------------------------------------------------------------------------------

#------------------------------ANIMAKY-------------------------------------------

### KTORY animovany, neskor ako 1980, dlhsi ako 30 minut -
### najlepsi a najhorsi rating

#vsetky animovane
movies[movies$Animation==1, 1:2]
#vsetky LEN starsie ako 1980 (nemusia byt animovane)
movies[movies$year>1980,]
#vsetky LEN dlhsie ako 30 minut
movies[movies$length>30,]

##vlastny pokus o kombinaciu
movies[(movies$year>1980) & (movies$length>30), 1:2]

# chcem vsetky tieto podmienky naraz
ind_a_80_30 <- (movies$Animation==1) & (movies$year>1980) & (movies$length>30)

# pozriem si ratingy na riadkoch, kde platia vsetky tie 3 podmienky naraz
movies[ind_a_80_30,]$rating

# keby som chcela len rating
sort(movies[ind_a_80_30,]$rating)

# ale ja chcem aj film - lepsie takto
order(movies[ind_a_80_30,]$rating)
# staci mi jedno najhorsie hodnotenie
ind_najhorsi = order(movies[ind_a_80_30,]$rating)[1]
# [1] 89
# a jedno najlepsie hodnotenie - zoradim v klesajucom poradi
ind_najlepsi = order(movies[ind_a_80_30,]$rating,decreasing=T)[1]
# [1] 195

# pozriem si tie filmy
movies[ind_a_80_30,][ind_najhorsi,]
movies[ind_a_80_30,][ind_najlepsi,]
# keby som chcela len nazov
movies[ind_a_80_30,][ind_najhorsi,]$title

# lepsie by bolo si indexy ulozit, a nie takto priamo cislo pisat
ind_rat_low = order(movies[ind_a_80_30,]$rating)[1]
movies[ind_a_80_30,][ind_rat_low,]

# alebo rovno naraz v jednom riadku
movies[ind_a_80_30,][order(movies[ind_a_80_30,]$rating)[1],]

# potom keby som chcela 3 najhorsie, staci prepisat, kolko z order chcem zobrat
movies[ind_a_80_30,][order(movies[ind_a_80_30,]$rating)[1:3],]

# vsimli sme si, ze ma sice zle hodnotenie, ale malo hlasov
### dalsia otazka - este taky, co ma aspon 1000 hlasov

ind_v_a_80_30 <- (movies$votes>=1000) & (movies$Animation==1) & (movies$year>1980) & (movies$length>30)

movies[ind_v_a_80_30,]$rating
sort(movies[ind_v_a_80_30,]$rating)
order(movies[ind_v_a_80_30,]$rating)
ind_v_najhorsi = order(movies[ind_v_a_80_30,]$rating)[1]
ind_v_najlepsi = order(movies[ind_v_a_80_30,]$rating,decreasing=T)[1]

movies[ind_v_a_80_30,][ind_v_najhorsi,]
movies[ind_v_a_80_30,][ind_v_najlepsi,]


#-----------------------AKCNE FILMY ULOHY---------------------------------------

### stupa pocet recenzii v case pre akcne filmy?

# vytriedim si akcne filmy
ind_action <- (movies$Action == 1)
# zobrazim si na grafe vzdy rok a pocet recenzii
plot(movies[ind_action, ]$year, movies[ind_action, ]$votes)

# stupa

# ktorych filmov je viacej, Terminator, Batman alebo Star Wars

install.packages("stringr")
library(stringr)
?str_detect
sum(str_detect(movies$title,"Batman"))
sum(str_detect(movies$title,"Terminator"))
sum(str_detect(movies$title,"Star Wars"))

# o Batmanovi
# berieme do uvahy len take, ktore to maju priamo v nazve

# pomocou pairs zobrazte year, length, rating
head(movies)
pairs(movies[,c(2,3,5)])

# zopar filmov je podozrivo dlhych, zistite, ze ktore to su

order(movies$length, decreasing=T)
ind_long <- order(movies$length, decreasing=T)[1:3]
movies[ind_long, ]

# ak by sme chceli, aby nam ten graf viac ukazal, 
# tak mozeme napr. 6 najdlhsich vyhodit
ind_long_6 <- order(movies$length, decreasing=T)[1:6]
movies[ind_long_6, ]
pairs(movies[-ind_long_6,c(2,3,5)])
# teraz je to uz krajsie na tom grafe


#### DU vyskusat ####-----------------------------------------------------------

## ako sa vola najhorsia romanticka komedia minuleho tisicrocia - DU 

ind_rom_comedy <- (movies$Comedy=1) & (movies$year<2000) & (movies$Romance=1)
ind_romcom_najhorsi = order(movies[ind_rom_comedy,]$rating)[1]
movies[ind_rom_comedy,][ind_romcom_najhorsi,]

## zoznam 10 najhorsich filmov s rozpoctom vacsim ako 20 milionov - DU

ind_rozpocet <- (movies$budget > 20000000 & !is.na(movies$budget))
ind_rozpocet_najhorsi = order(movies[ind_rozpocet,]$rating)[1]
movies[ind_rozpocet,][ind_rozpocet_najhorsi,]

## ktory zaner je najuspesnejsi po 2000? DU
ind_po_2000 <- (movies$year>2000)
ind_po_2000_best = order(movies[ind_rozpocet,]$rating, decreasing = TRUE)
length(ind_po_2000_best)
sum(movies[ind_po_2000_best], 18)
pairs(movies[ind_po_2000,][ind_po_2000_best,c(18:23)])
