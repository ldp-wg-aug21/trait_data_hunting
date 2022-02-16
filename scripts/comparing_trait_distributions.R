## load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
# library(ggpubr)
# library(effectsize)
# library(traitdata)
library(patchwork)
# library(overlapping)
library(dabestr)
library(ggthemes)

## set a plotting theme
theme_set(theme_few() + 
            theme(legend.position = "top"))

## read in data
verts <- as_tibble(
  readRDS("data-clean/lpi_and_wild_trait_data.RDS")) %>% 
  relocate(lpi, .before = Binomial) %>% 
  relocate(Group, TrophicLevel, .after = Binomial)

## check table
names(verts)
table(verts$lpi)

## create subsets
vertslpi <- verts %>% 
  filter(lpi == "C-LPI") %>% 
  arrange(desc(Group), Binomial) %>% 
  select(-lpi)
# unique(vertslpi$Binomial)

vertswild <- verts %>% 
  filter(lpi == "C-Vertebrates") %>% 
  arrange(desc(Group), Binomial) %>% 
  select(-lpi)
# n_distinct(vertswild$Binomial)

## find C-Vertebrates species not included in C-LPI data
wild_only <- setdiff(vertswild$Binomial, vertslpi$Binomial)

## create a new version of vert2, with only a single row per species
## (rather than duplicating species found in both lpi and wild), and 
## adding a column indicating if the spcies is found in C-LPI
verts2 <- vertswild %>% 
  mutate(clpi = ifelse(Binomial %in% unique(vertslpi$Binomial), "yes", "no"), 
         clpi = factor(clpi, levels = c("yes", "no")))

## plot data
## body size (log)
p1.a <- ggplot(data = verts, 
               mapping = aes(x = BodySize.log, fill = lpi), 
               colour = "grey80") + 
  geom_density(alpha = 0.4) + 
  scale_fill_manual(values = c("olivedrab", "azure"), name = "Dataset") + 
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 0.3), expand = c(0.01, 0)) +
  labs(x = "Body size (log)", 
       y = "Density")

## lifespan
p1.b <- ggplot(data = verts, 
               mapping = aes(x = LifeSpan.log, fill = lpi), 
               colour = "grey80") + 
  geom_density(alpha = 0.4) + 
  scale_fill_manual(values = c("olivedrab", "azure"), name = "Dataset") + 
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 0.5), expand = c(0.01, 0)) +
  labs(x = "Life span (log)", 
       y = "Density")

## make the same plots as above, but using verts2 instead
## (i.e. comparing LPI species to non-LPI species)
## body size (log)
p2.a <- ggplot(data = verts2, 
               mapping = aes(x = BodySize.log, fill = clpi), 
               colour = "grey80") + 
  geom_density(alpha = 0.4) + 
  scale_fill_manual(values = c("olivedrab", "azure"), 
                    name = "Included in C-LPI?") + 
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 0.3), expand = c(0.01, 0)) +
  labs(x = "Body size (log)", 
       y = "Density")

## lifespan
p2.b <- ggplot(data = verts2, 
               mapping = aes(x = LifeSpan.log, fill = clpi), 
               colour = "grey80") + 
  geom_density(alpha = 0.4) + 
  scale_fill_manual(values = c("olivedrab", "azure"), 
                    name = "Included in C-LPI") + 
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 0.5), expand = c(0.01, 0)) +
  labs(x = "Life span (log)", 
       y = "Density")


( p1.a + p1.b ) / ( p2.a + p2.b )

## dabestr

## body size (C-LPI versus non-C-LPI vertebrates)
bs_est <- verts2 %>% 
  select(Binomial, BodySize.log, clpi) %>% 
  drop_na() %>% 
  dabest(clpi, BodySize.log, 
         idx = c("yes", "no"))

bs_est.md <- median_diff(bs_est)
# mean_diff(bs_est)
# cohens_d(tt)
# hedges_g(tt)
# cliffs_delta(bs_est)

plot(bs_est.md, 
     rawplot.ylabel = "Body size (log)", 
     effsize.ylabel = "Median difference")

## life span
ls_est <- verts2 %>% 
  select(Binomial, LifeSpan.log, clpi) %>% 
  drop_na() %>% 
  dabest(clpi, LifeSpan.log, 
         idx = c("yes", "no"))

ls_est.md <- median_diff(ls_est)
# mean_diff(bs_est)
# cohens_d(tt)
# hedges_g(tt)
# cliffs_delta(bs_est)

plot(median_diff(ls_est), 
     rawplot.ylabel = "Lifespan (log)", 
     effsize.ylabel = "Median difference")



## try doing it with interaction between clpi and group
## body size
bs_est2 <- verts2 %>% 
  unite(col = "group_clpi", Group, clpi, remove = FALSE) %>% 
  select(Binomial, BodySize.log, Group, clpi, group_clpi) %>% 
  drop_na() %>% 
  dabest(group_clpi, BodySize.log, 
         idx = list(c("Birds_yes", "Birds_no"), 
                    c("Fish_yes", "Fish_no"), ## this should be Fishes!! 
                    c("Herpetofauna_yes", "Herpetofauna_no"), 
                    c("Mammals_yes", "Mammals_no")))

bs_est2.md <- median_diff(bs_est2)

plot(bs_est2.md, 
     color.column = clpi, 
     rawplot.ylabel = "Body size (log)", 
     effsize.ylabel = "Median difference", 
     palette = "Dark2", 
     theme = theme(legend.position = "none"))

## life span
ls_est2 <- verts2 %>% 
  unite(col = "group_clpi", Group, clpi, remove = FALSE) %>% 
  select(Binomial, LifeSpan.log, Group, clpi, group_clpi) %>% 
  drop_na() %>% 
  dabest(group_clpi, LifeSpan.log, 
         idx = list(c("Birds_yes", "Birds_no"), 
                    c("Fish_yes", "Fish_no"), ## this should be Fishes!! 
                    c("Herpetofauna_yes", "Herpetofauna_no"), 
                    c("Mammals_yes", "Mammals_no")))

ls_est2.md <- median_diff(ls_est2)

plot(ls_est2.md, 
     color.column = clpi, 
     rawplot.ylabel = "Lifespan (log)", 
     effsize.ylabel = "Median difference", 
     palette = "Dark2", 
     theme = theme(legend.position = "none"))
