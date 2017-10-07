## ---- message=FALSE, warning = FALSE-------------------------------------
library(dplyr)
library(tidyr)

## ---- eval = FALSE-------------------------------------------------------
## install.packages("dplyr")

## ----wczytanie_danych----------------------------------------------------
mieszkania <- read.csv("data/mieszkania_wroclaw_ceny.csv")

## ----struktura-----------------------------------------------------------
str(mieszkania)

## ------------------------------------------------------------------------
head(mieszkania)

## ----podstawowe_statystyki-----------------------------------------------
summary(mieszkania)

## ------------------------------------------------------------------------
# typ numeryczny (numeric)
mieszkania[5:10, "n_pokoj"]

# typ czynnikowy (factor)
mieszkania[5:10, "dzielnica"]

## ------------------------------------------------------------------------
mieszkania %>% 
  #grupujemy obserwacje
  group_by(dzielnica) %>% 
  #osobno w każdej grupie liczymy średnią
  summarise(sredni_metraz = mean(metraz)) 

## ---- warning=FALSE------------------------------------------------------
mieszkania %>%
  #filtrujemy wiersze
  filter(dzielnica == "Srodmiescie") %>% 
  #wybieramy zmienne
  select(dzielnica, pietro, cena_m2) %>% 
	head

## ------------------------------------------------------------------------
mieszkania <- mieszkania %>%
  filter(! dzielnica == "Brak")

## ------------------------------------------------------------------------
mieszkania %>% 
  #wybieramy zmienne
	select(dzielnica, metraz, cena_m2) %>% 
	head

## ----mutate_parter-------------------------------------------------------
mieszkania %>% 
	mutate(czy_parter = (pietro == 0) ) %>%
  select(dzielnica, pietro, czy_parter) %>%
  head(11)

## ----mutate_wysoki_budynek-----------------------------------------------
mieszkania %>% 
	mutate(czy_wysoki_budynek = (pietro_maks > 5) ) %>%
  select(dzielnica, pietro_maks, czy_wysoki_budynek) %>%
  head(9)

## ----dzielnice_posortowane-----------------------------------------------
mieszkania %>% 
  group_by(dzielnica) %>%
	summarize(cena_m2 = mean(cena_m2)) %>%
  arrange(cena_m2)

## ----zliczanie_obserwacji------------------------------------------------
mieszkania %>% 
  group_by(dzielnica) %>%
	summarize(liczba_ofert = n()) %>%
  arrange(desc(liczba_ofert))

## ------------------------------------------------------------------------
mieszkania %>% 
  top_n(5, wt = cena_m2)

## ----top_w_dzielnicach---------------------------------------------------
mieszkania %>% 
  group_by(dzielnica) %>%
  top_n(2, wt = cena_m2) %>% ungroup %>%
  arrange(dzielnica, cena_m2)

## ------------------------------------------------------------------------
mieszkania_60 <- mieszkania %>% 
  filter(metraz > 60)
mieszkania_60

## ------------------------------------------------------------------------
mieszkania %>%
  arrange(n_pokoj)

## ------------------------------------------------------------------------
mieszkania %>%
  filter(dzielnica == 'Stare Miasto') %>%
  arrange(metraz)

## ------------------------------------------------------------------------
mieszkania %>%
  summarise(srednia_cena = mean(cena_m2),
            mediana_cena = median(cena_m2))

## ------------------------------------------------------------------------
mieszkania %>%
  mutate(calkowita_cena = cena_m2 * metraz)

## ------------------------------------------------------------------------
mieszkania %>%
  filter(dzielnica != 'Stare Miasto') %>%
  top_n(1, wt=cena_m2)

## ------------------------------------------------------------------------
mieszkania %>%
  filter(metraz > 60) %>%
  summarise(srednia_cena = mean(cena_m2))

## ------------------------------------------------------------------------
mieszkania_60 %>%
  summarise(srednia_cena = mean(cena_m2))

## ------------------------------------------------------------------------
mieszkania %>%
  group_by(pietro) %>%
  summarise(sredni_metraz = mean(metraz))

## ------------------------------------------------------------------------
ludnosc <- read.csv("data/ludnosc_wroclaw.csv")
ludnosc

## ------------------------------------------------------------------------
head(mieszkania)

## ----join_ludnosc_mieszkania, warning=FALSE------------------------------
mieszkania %>%
  inner_join(ludnosc, by = c("dzielnica"="Dzielnica")) %>%
  head

## ---- warning=FALSE------------------------------------------------------
mieszkania %>%
  group_by(dzielnica) %>%
  summarise(liczba_ofert = n()) %>%
  inner_join(ludnosc, by = c("dzielnica"="Dzielnica"))

## ------------------------------------------------------------------------
ludnosc %>%
  arrange(liczba_mieszkancow)

## ------------------------------------------------------------------------
mieszkania %>%
  group_by(dzielnica) %>%
  summarise(liczba_ofert = n()) %>%
  inner_join(ludnosc, by = c("dzielnica"="Dzielnica")) %>%
  mutate(liczba_ofert_na_mieszkanca = liczba_ofert/liczba_mieszkancow/1000)

## ------------------------------------------------------------------------
mieszkania %>%
  group_by(dzielnica) %>%
  summarise(liczba_ofert = n()) %>%
  inner_join(ludnosc, by = c("dzielnica"="Dzielnica")) %>%
  mutate(liczba_ofert_na_mieszkanca = liczba_ofert/liczba_mieszkancow/1000) %>%
  arrange(liczba_ofert_na_mieszkanca)

## ------------------------------------------------------------------------
mieszkania %>%
  group_by(dzielnica) %>%
  summarise(liczba_pokojow = sum(n_pokoj)) %>%
  inner_join(ludnosc, by = c("dzielnica"="Dzielnica")) %>%
  mutate(pokojow_na_mieszkanca = liczba_pokojow/liczba_mieszkancow)

## ------------------------------------------------------------------------
dzielnice_srednia_cena <- mieszkania %>%
  group_by(dzielnica) %>%
  summarise(srednia_cena = mean(cena_m2))

## ------------------------------------------------------------------------
mieszkania %>%
  inner_join(dzielnice_srednia_cena, by=c('dzielnica')) %>%
  mutate(roznica_od_sredniej = cena_m2 - srednia_cena) %>%
  select(dzielnica, cena_m2, roznica_od_sredniej)

## ---- warning=FALSE------------------------------------------------------
mieszkania %>%
  group_by(dzielnica) %>%
  summarise(liczba_ofert = n()) %>%
  inner_join(ludnosc, by = c("dzielnica"="Dzielnica")) %>%
  mutate(liczba_ofert/liczba_mieszkancow)

## ----srednia-------------------------------------------------------------
mieszkania %>% 
  group_by(dzielnica) %>%
	mutate(srednia_cena_m2 = mean(cena_m2)) %>%
  ungroup %>%
  mutate(roznica = cena_m2 - srednia_cena_m2) %>%
  select(dzielnica, cena_m2, srednia_cena_m2, roznica) %>%
  head(5)

## ------------------------------------------------------------------------
mieszkania %>%
  mutate(pietro = if_else(pietro == 0, 
                          "parter",
                          if_else(pietro == pietro_maks, 
                                  "ostatnie_pietro", 
                                  "inne"))) %>%
  head

## ------------------------------------------------------------------------
mieszkania %>%
  mutate(pietro = case_when(
    pietro == 0 ~ "parter",
    pietro == pietro_maks ~ "ostatnie_pietro",
    TRUE ~ "inne"
  )) %>%
  head

## ------------------------------------------------------------------------
mieszkania %>%
  mutate(cena_wroclaw = mean(cena_m2)) %>%
  group_by(dzielnica) %>%
  summarise(cena_dzielnica = mean(cena_m2), cena_wroclaw = first(cena_wroclaw)) %>%
  mutate(stosunek_ceny_dz_wr = cena_dzielnica / cena_wroclaw)

## ------------------------------------------------------------------------
mieszkania %>%
  group_by(dzielnica) %>%
  mutate(cena_dzielnica = mean(cena_m2)) %>%
  top_n(1, wt=abs(cena_dzielnica-cena_m2))

## ------------------------------------------------------------------------
set.seed(23)
mieszkania %>%
  sample_n(10)

## ------------------------------------------------------------------------
mieszkania %>%
  group_by(dzielnica) %>%
  summarise(laczna_powierzchnia = sum(metraz)) %>%
  inner_join(ludnosc, by = c("dzielnica"="Dzielnica")) %>%
  mutate(laczna_powierzchnia/liczba_mieszkancow)

## ------------------------------------------------------------------------
mieszkania %>%
  mutate(czy_duze = metraz > 75) %>%
  mutate(rodzaj_zabudowy = case_when(
    pietro_maks < 3 ~ "mniej niż 3 piętra",
    pietro_maks < 6 ~ "3 do 6 pięter",
    TRUE ~ "ponad 6 pięter"
  )) %>% head

