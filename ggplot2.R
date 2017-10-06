library(dplyr)
library(ggplot2)

dat <- read.csv(file = "./data/mieszkania_wroclaw_ceny.csv") %>% 
  mutate(duze = metraz > 50,
         pietro = ifelse(pietro == 0, "parter",
                         ifelse(pietro == pietro_maks, "ostatnie_pietro",
                                ifelse(pietro > 15, "wysoko",
                                       ifelse(pietro_maks < 3, "niska_zabudowa", "inne")))),
         pietro = factor(pietro),
         pokoj = factor(ifelse(n_pokoj > 3, ">3", n_pokoj)))

ggplot(dat, aes(x = dzielnica, y = cena_m2)) +
  geom_point()

ggplot(dat, aes(x = dzielnica, y = cena_m2)) +
  geom_point(position = "jitter")

set.seed(1410)
ggplot(dat, aes(x = dzielnica, y = cena_m2)) +
  geom_point(position = "jitter")

# facets -----------------------------------

ggplot(dat, aes(x = dzielnica, y = cena_m2)) +
  geom_point(position = "jitter") +
  facet_wrap(~ pokoj)

ggplot(dat, aes(x = dzielnica, y = cena_m2)) +
  geom_point(position = "jitter") +
  facet_wrap(~ pokoj, labeller = label_both)

ggplot(dat, aes(x = dzielnica, y = cena_m2)) +
  geom_point(position = "jitter") +
  facet_wrap(~ pokoj + pietro, labeller = label_both)

ggplot(dat, aes(x = dzielnica, y = cena_m2)) +
  geom_point(position = "jitter") +
  facet_grid(pietro ~ pokoj, labeller = label_both)

ggplot(dat, aes(x = dzielnica, y = cena_m2)) +
  geom_boxplot() +
  facet_wrap(~ pokoj, labeller = label_both)

ggplot(dat, aes(x = dzielnica, y = cena_m2, color = duze)) +
  geom_point(position = "jitter") +
  facet_grid(pietro ~ pokoj, labeller = label_both)


library(ggbeeswarm)
ggplot(dat, aes(x = dzielnica, y = cena_m2, color = duze)) +
  geom_quasirandom() +
  facet_grid(pietro ~ pokoj, labeller = label_both)

# gestosc ------------------------------

ggplot(dat, aes(x = cena_m2)) +
  geom_density()

ggplot(dat, aes(x = cena_m2, fill = pokoj)) +
  geom_density() 

ggplot(dat, aes(x = cena_m2, fill = pokoj)) +
  geom_density(alpha = 0.2) 

ggplot(dat, aes(x = cena_m2, fill = pokoj)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ pietro)


#kartogram

load("data/mapa_dzielnic.Rdata")

plot_data <- dat %>%
  group_by(dzielnica) %>%
  summarise(cena_m2 = mean(cena_m2)) %>%
  inner_join(granice_dzielnic, by=c("dzielnica"="id")) 

ggplot(plot_data) +
  geom_polygon(aes(x=long, y=lat, group = dzielnica, fill = cena_m2))

ggplot(plot_data) +
  geom_polygon(aes(x=long, y=lat, group = dzielnica, fill = cena_m2)) +
  coord_map()

# 1. Create a density plot for each pathotype and medium.


ggplot(dat, aes(x = duze)) +
  geom_bar()

ggplot(dat, aes(x = duze, fill = dzielnica)) +
  geom_bar()

ggplot(dat, aes(x = duze, fill = dzielnica)) +
  geom_bar(position = "fill")


# 1. Using facets and bar charts show threshold data separately
# for each active substance.
# 2. Show on a barchart number of strains from each pathotype.

thr_dat2 <- group_by(thr_dat, medium) %>% 
  summarise(thr = mean(thr))

rbind(mutate(thr_dat2, thr_et = TRUE),
      mutate(thr_dat2, thr_et = FALSE,
             thr = 1 - thr)) %>% 
  ggplot(aes(x = medium, y = thr, fill = thr_et, label = formatC(thr, 2))) +
  geom_bar(stat = "identity") +
  geom_text(vjust = 2)


mean_dat <- group_by(final_dat, active, medium, pathotype) %>% 
  summarise(mean_value = mean(value),
            sd_value = sd(value))

ggplot(mean_dat, aes(x = pathotype, y = active, fill = mean_value)) +
  geom_tile(color = "black") +
  facet_wrap(~ medium)

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_bar(position = "dodge", stat = "identity") + 
  facet_wrap(~ active, ncol = 1)

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  facet_wrap(~ active, ncol = 1)

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = mean_value + sd_value, ymin = mean_value, color = medium), position = "dodge") +
  facet_wrap(~ active, ncol = 1) +
  coord_flip()

# 1. Using a bar chart compare median values for each medium and pathotype. 
# Use median absolute deviation (mad()) as a dispersion measure.
# 2. Using a heat map compare median values for each medium and pathotype. 

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = mean_value + sd_value, ymin = mean_value, color = medium), position = "dodge") +
  facet_wrap(~ active, ncol = 1) 

ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = mean_value + sd_value, ymin = mean_value, color = medium), position = "dodge") +
  facet_wrap(~ active, nrow = 1) + 
  coord_flip()

p <- ggplot(mean_dat, aes(x = pathotype, y = mean_value, fill = medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = mean_value + sd_value, ymin = mean_value, color = medium), position = "dodge") +
  facet_wrap(~ active, ncol = 1)

p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

p + theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "bottom")

my_theme <- theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  legend.position = "bottom")

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill") +
  my_theme


ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill") +
  theme_bw()
# 1. Create your own theme. See ?theme
# 2. See possible themes using theme_bw

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill")

unique(thr_dat[["medium"]])

thr_dat2 <- mutate(thr_dat,
                   medium = factor(medium, levels = c("LB", "BHI", "M63", "TSB")))

ggplot(thr_dat2, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill")

# Reverse the sequence of active (W3, W2, W1) and create a bar chart, 
# with the fraction of strains above threshold for each possible value
# of active.

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill") +
  scale_fill_discrete("Threshold")

ggplot(thr_dat, aes(x = medium, fill = thr)) +
  geom_bar(position = "fill") +
  scale_fill_manual("Threshold", values = c("orange", "lightblue3"))

ggplot(mean_dat, aes(x = pathotype, y = active, fill = mean_value, color = sd_value)) +
  geom_tile(color = "black") +
  geom_point() +
  facet_wrap(~ medium) +
  scale_color_continuous(low = "white", high = "black")

ggplot(mean_dat, aes(x = pathotype, y = active, fill = mean_value, color = sd_value)) +
  geom_tile(color = "black") +
  geom_point() +
  facet_wrap(~ medium) +
  scale_color_continuous(low = "white", high = "black") +
  scale_fill_continuous(low = "blue", high = "red")

ggplot(mean_dat, aes(x = pathotype, y = active, color = mean_value, size = sd_value)) +
  geom_point() +
  facet_wrap(~ medium) 

ggplot(mean_dat, aes(x = pathotype, y = active, color = mean_value, size = sd_value)) +
  geom_point() +
  facet_wrap(~ medium) +
  scale_size_continuous(range = c(5, 10))

ggplot(mean_dat, aes(x = pathotype, y = active, fill = mean_value, color = sd_value)) +
  geom_tile(color = "black") +
  facet_wrap(~ medium) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = (max(mean_dat[["mean_value"]]) - min(mean_dat[["mean_value"]]))/2)

ggplot(mean_dat, aes(x = pathotype, y = active, fill = mean_value, color = sd_value)) +
  geom_tile(color = "black") +
  facet_wrap(~ medium) +
  scale_fill_gradient2(low = "yellow", high = "red", mid = "black", 
                       midpoint = median(mean_dat[["mean_value"]]))


# Create a heatmap with gradient scale, where midpoint is the median of mean_value

ggplot(final_dat, aes(x = value, fill = active)) +
  geom_density(alpha = 0.2) +
  facet_wrap( ~ medium)

ggplot(final_dat, aes(x = value, fill = active)) +
  geom_density(alpha = 0.2) +
  facet_wrap( ~ medium) +
  coord_cartesian(xlim = c(0, 0.1))


library(plotly)
ggplotly(ggplot(thr_dat, aes(x = medium, fill = thr)) +
           geom_bar(position = "fill"))

ggplotly(ggplot(final_dat, aes(x = pathotype, y = value, color = active)) +
           geom_boxplot(position = "dodge") +
           facet_wrap(~ medium))

