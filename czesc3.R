## ---- message=FALSE, warning = FALSE-------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
knitr::opts_chunk$set(fig.height = 3)

## ------------------------------------------------------------------------
mieszkania <- read.csv(file = "data/mieszkania_wroclaw_ceny.csv") %>% 
  mutate(duze = metraz > 50,
         pietro = ifelse(pietro == 0, "parter",
                         ifelse(pietro == pietro_maks, "ostatnie_pietro",
                                ifelse(pietro > 15, "wysoko",
                                       ifelse(pietro_maks < 3, "niska_zabudowa", "inne")))),
         pietro = factor(pietro),
         pokoj = factor(ifelse(n_pokoj > 3, ">3", n_pokoj)))

## ---- message=FALSE------------------------------------------------------
srednia_cena_pokoje <- mieszkania %>%
  group_by(n_pokoj) %>% summarise(srednia_cena = mean(cena_m2))
ggplot(srednia_cena_pokoje, aes(x = n_pokoj, y = srednia_cena)) +
  geom_bar(stat = "identity")

## ------------------------------------------------------------------------
ggplot(mieszkania, aes(x = n_pokoj)) +
  geom_bar()

## ------------------------------------------------------------------------
ggplot(mieszkania, aes(x = dzielnica, y = ..count.., fill = pokoj)) +
  geom_bar(position = "stack")

## ------------------------------------------------------------------------
ggplot(mieszkania, aes(x = dzielnica, fill = pokoj)) +
  geom_bar(position = "fill") 

## ----barplot_fill--------------------------------------------------------
ggplot(mieszkania, aes(x = dzielnica, fill = pokoj, 
                       label = ..count..)) +
  geom_bar(position = "fill") +
  geom_text(stat = "count", position = "fill", vjust = 2)

## ------------------------------------------------------------------------
dat_srednia <- group_by(mieszkania, pietro, dzielnica, duze) %>% 
  summarise(mean_cena = mean(cena_m2),
            sd_cena = sd(cena_m2))
head(dat_srednia)

## ------------------------------------------------------------------------
ggplot(dat_srednia, aes(x = dzielnica, y = mean_cena, fill = pietro)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~ duze, ncol = 1)

## ------------------------------------------------------------------------
ggplot(dat_srednia, aes(x = dzielnica, y = mean_cena, fill = pietro)) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ duze, ncol = 1)

## ------------------------------------------------------------------------
ggplot(dat_srednia, aes(x = dzielnica, y = mean_cena, fill = pietro)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymax = mean_cena + sd_cena, 
                    ymin = mean_cena, color = pietro), position = "dodge") +
  facet_wrap(~ duze, ncol = 1)

## ------------------------------------------------------------------------
ggplot(mieszkania, aes(x = dzielnica, y = cena_m2)) +
  geom_point()

## ------------------------------------------------------------------------
ggplot(mieszkania, aes(x = dzielnica, y = cena_m2, col = duze)) +
  geom_point()

## ------------------------------------------------------------------------
ggplot(mieszkania, aes(x = dzielnica, y = cena_m2, col = duze)) +
  geom_point(position = "jitter")

## ------------------------------------------------------------------------
set.seed(1410)
ggplot(mieszkania, aes(x = dzielnica, y = cena_m2)) +
  geom_point(position = "jitter")

## ----punty_facet_pokoj---------------------------------------------------
ggplot(mieszkania, aes(x = dzielnica, y = cena_m2)) +
  geom_point(position = "jitter") +
  facet_wrap(~ pokoj)

## ---- fig.height=4.2-----------------------------------------------------
ggplot(mieszkania, aes(x = dzielnica, y = cena_m2)) +
  geom_point(position = "jitter") +
  facet_wrap(~ pokoj, labeller = label_both)

## ---- fig.height=4.2-----------------------------------------------------
ggplot(mieszkania, aes(x = dzielnica, y = cena_m2)) +
  geom_point(position = "jitter") +
  facet_wrap(~ pokoj + pietro, labeller = label_both)

## ---- fig.height=4.2-----------------------------------------------------
ggplot(mieszkania, aes(x = dzielnica, y = cena_m2)) +
  geom_point(position = "jitter") +
  facet_grid(pietro ~ pokoj, labeller = label_both)

## ---- fig.height=4.2-----------------------------------------------------
ggplot(mieszkania, aes(x = dzielnica, y = cena_m2)) +
  geom_boxplot() +
  facet_wrap(~ pokoj, labeller = label_both)

## ---- fig.height=4.2-----------------------------------------------------
ggplot(mieszkania, aes(x = dzielnica, y = cena_m2, color = duze)) +
  geom_point(position = "jitter") +
  facet_grid(pietro ~ pokoj, labeller = label_both)

## ------------------------------------------------------------------------
ggplot(mieszkania, aes(x = cena_m2)) +
  geom_density()

## ------------------------------------------------------------------------
ggplot(mieszkania, aes(x = cena_m2, fill = pokoj)) +
  geom_density()

## ------------------------------------------------------------------------
ggplot(mieszkania, aes(x = cena_m2, fill = pokoj)) +
  geom_density(alpha = 0.2) 

## ------------------------------------------------------------------------
ggplot(mieszkania, aes(x = cena_m2, fill = pokoj)) +
  geom_density() +
  facet_wrap(~pokoj, ncol=1)

## ------------------------------------------------------------------------
ggplot(mieszkania, aes(x = cena_m2, fill = pokoj)) +
  geom_density(adjust = 0.5) +
  facet_wrap(~pokoj, ncol=1)

## ------------------------------------------------------------------------
ggplot(mieszkania, aes(x = metraz, fill = pokoj)) +
  geom_density(adjust = 2) +
  facet_wrap(~pokoj, ncol=1)

## ---- warning=FALSE------------------------------------------------------
load("data/mapa_dzielnic.Rdata")

plot_data <- mieszkania %>%
  group_by(dzielnica) %>%
  summarise(cena_m2 = mean(cena_m2)) %>%
  inner_join(granice_dzielnic, by=c("dzielnica"="id")) 

## ---- fig.height = 6, fig.width = 7--------------------------------------
ggplot(plot_data) +
  geom_polygon(aes(x=long, y=lat, group = dzielnica, fill = cena_m2))

## ------------------------------------------------------------------------
ggplot(plot_data) +
  geom_polygon(aes(x=long, y=lat, group = dzielnica, fill = cena_m2)) +
  coord_map()

## ------------------------------------------------------------------------
library(ggthemes)

## ------------------------------------------------------------------------
ggplot(srednia_cena_pokoje, aes(x = n_pokoj, y = srednia_cena, fill = srednia_cena)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2_tableau(palette = "Light Red-Green") +
  theme_economist_white()

## ------------------------------------------------------------------------
ggplot(plot_data) +
  geom_polygon(aes(x=long, y=lat, group = dzielnica, fill = cena_m2)) +
  coord_map() +
  scale_fill_gradient2_tableau() + #skala kolorów z programu tableau
  theme_fivethirtyeight()  #wygląd wykresu wzorowany na fivethirtyeight

## ---- warning=FALSE, message=FALSE---------------------------------------
ggplot(mieszkania, aes(x = dzielnica, y = cena_m2, color = duze)) +
  geom_point() +
  facet_grid(pietro ~ pokoj, labeller = label_both)

## ---- warning=FALSE, message=FALSE---------------------------------------
ggplot(mieszkania, aes(x = dzielnica, y = cena_m2, color = duze)) +
  geom_point(position = "jitter") +
  facet_grid(pietro ~ pokoj, labeller = label_both)

## ---- warning=FALSE, message=FALSE---------------------------------------
library(ggbeeswarm)
ggplot(mieszkania, aes(x = dzielnica, y = cena_m2, color = duze)) +
  geom_quasirandom() +
  facet_grid(pietro ~ pokoj, labeller = label_both)

