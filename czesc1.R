## ------------------------------------------------------------------------
mieszkania <- read.csv("data/mieszkania_wroclaw_ceny.csv", 
                       encoding = "UTF-8")

## ------------------------------------------------------------------------
# wymiary tabeli
dim(mieszkania)

# pierwsze wiersze z tabeli
head(mieszkania)

## ------------------------------------------------------------------------
# podsumowanie tabeli
summary(mieszkania)

## ---- message=FALSE, warning = FALSE-------------------------------------
library(dplyr)
filter(mieszkania, dzielnica == "Stare Miasto")

## ------------------------------------------------------------------------
filter(mieszkania, dzielnica == "Stare Miasto", rok == 2017)

## ------------------------------------------------------------------------
select(mieszkania, dzielnica, cena_m2)

## ------------------------------------------------------------------------
mieszkania %>%
  filter(dzielnica == "Srodmiescie") %>%
  select(dzielnica, pietro, cena_m2)

## ------------------------------------------------------------------------
mieszkania %>%
  group_by(n_pokoj) %>%
  summarise(srednia_cena = mean(cena_m2))

## ---- message=FALSE, warning=FALSE---------------------------------------
library(ggplot2)
srednia_cena_pokoje <- mieszkania %>%
  group_by(n_pokoj) %>%
  summarise(srednia_cena = mean(cena_m2))

## ---- eval = FALSE-------------------------------------------------------
## ggplot(srednia_cena_pokoje, aes(x = n_pokoj, y = srednia_cena))

## ---- eval = T-----------------------------------------------------------
ggplot(srednia_cena_pokoje, aes(x = n_pokoj, y = srednia_cena)) +
  geom_bar(stat = "identity")

## ------------------------------------------------------------------------
mieszkania %>%
  filter(dzielnica == "Psie Pole", n_pokoj > 2) %>%
  head

## ------------------------------------------------------------------------
mieszkania %>%
  select(pietro, metraz)

## ------------------------------------------------------------------------
mieszkania_srodmiescie <- mieszkania %>%
  filter(dzielnica == "Srodmiescie")

## ------------------------------------------------------------------------
mieszkania %>%
  filter(dzielnica == "Krzyki") %>%
  select(pietro, pietro_maks)

## ---- message=FALSE, warning=FALSE---------------------------------------
srednia_cena_dzielnice <- mieszkania %>%
  group_by(dzielnica) %>%
  summarise(srednia_cena = mean(cena_m2))

## ------------------------------------------------------------------------
ggplot(srednia_cena_dzielnice, aes(x = dzielnica, y = srednia_cena)) +
  geom_bar(stat = "identity")

