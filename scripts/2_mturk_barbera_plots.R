setwd("C:/Users/Subhayan/Work/twitter-landscape/")

library(tidyverse)
library(corrr)
library(cowplot)
library(broom)
library(stargazer)
library(knitr)
library(ggridges)

exclude_barbera_NAs <- FALSE
MTurk_NA_threshold <- 100 # set to 100 for all elites

# function to scale a vector to between [0,1]
scale_01 <- function(x){(x-min(x))/(max(x)-min(x))}

mturk_tbl <- read_csv("data/mturk_ideologies.csv")
barbera_tbl <- read_csv("data/weak_elite_ideologies.csv")
elite_freq_tbl <- read_csv("data/elites_activity.csv")

if(exclude_barbera_NAs) {
  barbera_tbl <- barbera_tbl %>%
    mutate(handle = tolower(username)) %>%
    filter(!is.na(ideology)) %>%
    rename(recoded_ideology = ideology) %>%
    mutate(recoded_ideology = ifelse(recoded_ideology == -Inf, -3, recoded_ideology)) %>%
    mutate(recoded_ideology = ifelse(recoded_ideology == Inf, 3, recoded_ideology)) %>%
    rename(barbera_ideology = recoded_ideology) %>%
    select(handle, barbera_ideology)
} else {
  barbera_tbl <- barbera_tbl %>%
    mutate(handle = tolower(username)) %>%
    mutate(recoded_ideology = ifelse(is.na(ideology), 0, ideology)) %>%
    mutate(recoded_ideology = ifelse(recoded_ideology == -Inf, -3, recoded_ideology)) %>%
    mutate(recoded_ideology = ifelse(recoded_ideology == Inf, 3, recoded_ideology)) %>%
    rename(barbera_ideology = recoded_ideology) %>%
    select(handle, barbera_ideology)
}

mturk_tbl <- mturk_tbl %>%
  filter(NA_percent_attn <= MTurk_NA_threshold)

elite_freq_tbl <- elite_freq_tbl %>%
  mutate(handle = tolower(handle)) %>%
  rename(freq = numberoftweets) %>%
  mutate(freq_scaled = scale_01(freq))
  
full_tbl <- mturk_tbl %>%
  inner_join(barbera_tbl) %>%
  inner_join(elite_freq_tbl)


# different kinds of plots with only mturk
plot1 <- ggplot(full_tbl, aes(mean_rating_all)) +
  geom_density() +
  ggtitle("all annotations") +
  theme_bw()

plot2 <- ggplot(full_tbl, aes(mean_rating_attn)) +
  geom_density() +
  ggtitle("annotations with trump minus sanders > 0") +
  theme_bw()

plot3 <- ggplot(full_tbl, aes(mean_rating_twtr)) +
  geom_density() +
  ggtitle("annotations with twitter account == 1") +
  theme_bw()

plot4 <- ggplot(full_tbl, aes(mean_rating_pk)) +
  geom_density() +
  ggtitle("annotations all knowledge questions correctly answered") +
  theme_bw()

plot5 <- ggplot(full_tbl, aes(mean_rating_attn_twtr)) +
  geom_density() +
  ggtitle("correct answers AND trump - sanders > 0") +
  theme_bw()

mturk_only_plots <- plot_grid(plot1, plot2, plot3, plot4, plot5, ncol = 2)

# check correlations with barbera estimates

compare_tbl <- full_tbl %>%
  select(handle, mean_rating_all, mean_rating_attn, barbera_ideology) %>%
  mutate(scaled_mturk_ideology = scale_01(mean_rating_all),
         scaled_mturk_ideology_attn = scale_01(mean_rating_attn),
         scaled_barbera_ideology = scale_01(barbera_ideology)) %>%
  select(scaled_mturk_ideology, scaled_mturk_ideology_attn, scaled_barbera_ideology)

cor.test(compare_tbl$scaled_mturk_ideology, compare_tbl$scaled_barbera_ideology, method = "pearson") %>%
  tidy()

cor.test(compare_tbl$scaled_mturk_ideology_attn, compare_tbl$scaled_barbera_ideology, method = "pearson") %>%
  tidy()

###

## for all visualizations from here on, we only use mean_rating_attn,
# ie. we remove all those who did not pass the attention check

# compare mturk vs barbera ideology

viz_tbl <- full_tbl %>%
  select(handle, mean_rating_attn, barbera_ideology) %>%
  mutate(mturk_ideology_shifted = mean_rating_attn - 4) %>%
  select(-mean_rating_attn) %>%
  rename(barbera = barbera_ideology,
         mturk = mturk_ideology_shifted) %>%
  pivot_longer(cols = c(2,3), names_to = "type", values_to = "ideology") %>%
  mutate(type = ifelse(type == "barbera", "ideal points", "perceived ideologies")) 

medians <- viz_tbl %>%
  group_by(type) %>%
  summarise(type_median = median(ideology))

unweighted_distribution_plot <- viz_tbl %>%
  ggplot(aes(x=ideology, fill=type)) +
    geom_density(alpha = 0.4) +
    geom_vline(data=medians, aes(xintercept=type_median, color=type),
             linetype="dashed") +
  xlim(c(-4,4)) +
  labs(x="unweighted ideology",
       fill = "ideology type",
       color = "ideology type") + 
  theme_bw() +
  theme(legend.position = c(0.18, 0.85),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size = 14),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave(file="figures/fig2.svg", plot=unweighted_distribution_plot, width=8, height=6)

# paired wilcox test (are mturk ideologies significantly lesser than barbera ideologies)

wilcoxon_comparison_tbl <- viz_tbl %>%
  pivot_wider(values_from = ideology, names_from = type) %>%
  select(`ideal points`, `perceived ideologies`)

wilcox.test(wilcoxon_comparison_tbl$`perceived ideologies`,
            wilcoxon_comparison_tbl$`ideal points`, paired = TRUE,
            alternative = "less") %>% tidy()

NA_density <- full_tbl %>%
  select(NA_percent_all) %>%
  mutate(NA_percent_all = round(NA_percent_all)) %>%
  rename(NA_percent = NA_percent_all) %>%
  ggplot(aes(NA_percent)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 5) +
  geom_density(alpha= 0.2, fill="#FF6666") +
  scale_x_continuous(breaks=seq(0, 100, by = 5)) +
  theme_bw() +
  labs(x = "Percent of MTurkers responded with 'don't know/can't say",
       y = "Frequency of opinion leaders")

# weighted

viz_tbl <- full_tbl %>%
  select(handle, mean_rating_attn, barbera_ideology, freq_scaled) %>%
  mutate(weighted_barbera_ideology = barbera_ideology * freq_scaled,
         weighted_mturk_ideology_shifted = (mean_rating_attn - 4) * freq_scaled) %>%
  select(-mean_rating_attn, -barbera_ideology, -freq_scaled) %>%
  rename(barbera = weighted_barbera_ideology,
         mturk = weighted_mturk_ideology_shifted) %>%
  pivot_longer(cols = c(2,3), names_to = "type", values_to = "weighted_ideology") %>%
  mutate(type = ifelse(type == "barbera", "ideal points", "perceived ideologies"))

medians <- viz_tbl %>%
  group_by(type) %>%
  summarise(type_median = median(weighted_ideology))

weighted_distribution_plot <- viz_tbl %>%
  ggplot(aes(x=weighted_ideology, fill=type)) +
  geom_density(alpha = 0.4) +
  geom_vline(data=medians, aes(xintercept=type_median, color=type),
             linetype="dashed") +
  theme_bw() +
  xlim(c(-4,4)) +
  labs(x="weighted ideology",
       fill = "ideology type",
       color = "ideology type")+
  theme(legend.position = c(0.18, 0.85),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size = 14),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave(file="figures/fig3.svg", plot=weighted_distribution_plot, width=8, height=6)

# paired wilcox test (are mturk ideologies significantly lesser than barbera ideologies)

wilcoxon_comparison_tbl <- viz_tbl %>%
  pivot_wider(values_from = weighted_ideology, names_from = type) %>%
  select(`ideal points`, `perceived ideologies`)

wilcox.test(wilcoxon_comparison_tbl$`perceived ideologies`, wilcoxon_comparison_tbl$`ideal points`, paired = TRUE, alternative = "less") %>% tidy()

########################################
# within deciles

qs <- quantile(full_tbl$freq, probs = seq(0, 1, by = 0.1))

all_decile_tbl = NULL
for(i in 1:10) {
  
  decile_tbl <- full_tbl %>%
    filter(freq >= qs[i] & freq <= qs[i+1]) %>%
    mutate(decile = i)

  all_decile_tbl <- all_decile_tbl %>% rbind(decile_tbl)
}

medians <- all_decile_tbl %>%
  group_by(decile) %>%
  summarize(barbera = median(barbera_ideology),
            mturk = median(mean_rating_attn-4)) %>%
  ungroup() %>%
  pivot_longer(cols = c(2,3), names_to = "type", values_to = "decile_median_ideology") %>%
  mutate(type = ifelse(type == "barbera", "ideal points", "perceived ideologies"))

viz_tbl <- all_decile_tbl %>%
  select(handle, decile, mean_rating_attn, barbera_ideology) %>%
  mutate(mean_rating_attn = mean_rating_attn - 4) %>%
  rename(mturk = mean_rating_attn,
         barbera = barbera_ideology) %>%
  pivot_longer(cols = c(3,4), names_to = "type", values_to = "ideology") %>%
  mutate(type = ifelse(type == "barbera", "ideal points", "perceived ideologies")) %>%
  inner_join(medians, by = c("decile", "type"))

decile_plots <- ggplot(viz_tbl, aes(x = ideology, y = as.factor(decile),
                                    fill = type)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  geom_vline(xintercept=0, linetype="dashed") +
  labs(x="ideology", y="decile") +
  facet_wrap(~type) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12))

ggsave(file="figures/fig4.svg", plot=decile_plots, width=8, height=6)

dec_wilcox_tbl <- NULL
for(dec in unique(viz_tbl$decile)){
  message(dec)
  wilcoxon_comparison_tbl <- viz_tbl %>%
    filter(decile == dec) %>%
    select(handle, type, ideology) %>%
    pivot_wider(values_from = ideology, names_from = type) %>%
    select(`ideal points`, `perceived ideologies`)
  
  curr_wilcox_test <- wilcox.test(wilcoxon_comparison_tbl$`perceived ideologies`, wilcoxon_comparison_tbl$`ideal points`, paired = TRUE, alternative = "less") %>% tidy()
  dec_wilcox_tbl <- dec_wilcox_tbl %>%
    rbind(c(dec, curr_wilcox_test))
}

dec_wilcox_tbl <- dec_wilcox_tbl %>%
  data.frame() %>%
  rename(decile = 1, v_statistic = 2, p_value = 3) %>%
  select(-method)

kable(dec_wilcox_tbl, "latex")

########################################
# within genres unweighted

elite_classes <- read_csv("data/elite_classification.csv")
classes <- elite_classes %>%
  pull(sector1) %>%
  unique()

classes <- classes[!classes %in% c("?", "X", "x")]

par(mfrow=c(5,2))

classes = c("hard news", "meme", "organization", "political pundit", 
            "political figure",  "brand", "media outlet", "public figure",
            "sports", "entertainment")

all_class_elites_ideologies <- NULL
for(class in classes) {
  message(class)
  class_elites <- elite_classes %>%
    filter(sector1 %in% class | sector2 %in% class | sector3 %in% class) %>%
    pull(handle) %>% tolower()
  
  class_elites_ideology <- full_tbl %>%
    select(handle, mean_rating_attn, barbera_ideology, freq_scaled) %>%
    filter(handle %in% class_elites) %>%
    mutate(genre = class,
           mturk_ideology = mean_rating_attn - 4,
           weighted_mturk_ideology = mturk_ideology * freq_scaled,
           weighted_barbera_ideology = barbera_ideology * freq_scaled) %>%
    select(-mean_rating_attn)

  all_class_elites_ideologies <- all_class_elites_ideologies %>%
    rbind(class_elites_ideology)
}

class_medians <- all_class_elites_ideologies %>%
  group_by(genre) %>%
  summarize(median_mturk_ideology_unweighted = median(mturk_ideology),
            median_barbera_ideology_unweighted = median(barbera_ideology),
            median_mturk_ideology_weighted = median(weighted_mturk_ideology),
            median_barbera_ideology_weighted = median(weighted_barbera_ideology)) %>%
  ungroup()

all_class_elites_ideologies <- all_class_elites_ideologies %>%
  select(-freq_scaled)

unweighted_medians <- class_medians %>%
  select(genre, median_barbera_ideology_unweighted, median_mturk_ideology_unweighted) %>%
  rename(mturk = median_mturk_ideology_unweighted,
         barbera = median_barbera_ideology_unweighted) %>%
  pivot_longer(cols = c(2,3), names_to = "type", values_to = "class_median_ideology")

viz_tbl <- all_class_elites_ideologies %>%
  select(handle, genre, mturk_ideology, barbera_ideology) %>%
  rename(mturk = mturk_ideology,
         barbera = barbera_ideology) %>%
  pivot_longer(cols = c(3,4), names_to = "type", values_to = "ideology") %>%
  inner_join(unweighted_medians, by = c("type", "genre")) %>%
  mutate(type = ifelse(type == "barbera", "ideal points", "perceived ideologies"))
  
unweighted_genre_plot <- viz_tbl %>%
  ggplot(aes(x=ideology, fill = type)) +
  geom_density(alpha = 0.4) +
  geom_vline(aes(xintercept = class_median_ideology, colour = type), linetype = "dashed") +
  xlim(c(-4,4))+
  facet_wrap(~genre, nrow = 5, scales = "free_y") +
  theme_bw() +
  labs(x="unweighted ideology")

unweighted_genre_ridgeplots <- ggplot(viz_tbl, aes(x=ideology, y = genre, fill = type))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  geom_vline(xintercept=0, linetype="dashed") +
  labs(x = "ideology", y = "genre") +
  facet_wrap(~type)+
  theme_bw()+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12))

ggsave(file="figures/FigA1.svg", plot=unweighted_genre_ridgeplots, width=8, height=6)

genre_wilcox_tbl <- NULL
for(g in unique(viz_tbl$genre)){
  message(g)
  wilcoxon_comparison_tbl <- viz_tbl %>%
    filter(genre == g) %>%
    select(handle, type, ideology) %>%
    pivot_wider(values_from = ideology, names_from = type) %>%
    select(`ideal points`, `perceived ideologies`)
  
  curr_wilcox_tbl <- wilcox.test(wilcoxon_comparison_tbl$`perceived ideologies`,
                                  wilcoxon_comparison_tbl$`ideal points`,
                                  paired = TRUE, alternative = "less") %>%
    tidy()
  
  genre_wilcox_tbl <- genre_wilcox_tbl %>%
    rbind(c(g, curr_wilcox_tbl))
}


# within genres weighted

weighted_medians <- class_medians %>%
  select(genre, median_barbera_ideology_weighted, median_mturk_ideology_weighted) %>%
  rename(mturk = median_mturk_ideology_weighted,
         barbera = median_barbera_ideology_weighted) %>%
  pivot_longer(cols = c(2,3), names_to = "type", values_to = "class_median_ideology")

viz_tbl <- all_class_elites_ideologies %>%
  select(handle, genre, weighted_mturk_ideology, weighted_barbera_ideology) %>%
  rename(mturk = weighted_mturk_ideology,
         barbera = weighted_barbera_ideology) %>%
  pivot_longer(cols = c(3,4), names_to = "type", values_to = "weighted_ideology") %>%
  inner_join(weighted_medians, by = c("type", "genre")) %>%
  mutate(type = ifelse(type == "barbera", "ideal points", "perceived ideologies"))

weighted_genre_plot <- viz_tbl %>%
  ggplot(aes(x=weighted_ideology, fill = type)) +
  geom_density(alpha = 0.4) +
  geom_vline(aes(xintercept = class_median_ideology, colour = type), linetype = "dashed") +
  xlim(c(-4,4))+
  facet_wrap(~genre, nrow = 5, scales = "free_y") +
  theme_bw() +
  labs(x="weighted ideology")

weighted_genre_ridgeplots <- ggplot(viz_tbl, aes(x=weighted_ideology, y = genre, fill = type))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  geom_vline(xintercept=0, linetype="dashed") +
  labs(x = "ideology", y = "genre") +
  facet_wrap(~type)+
  theme_bw()+
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12))

ggsave(file="figures/fig5.svg", plot=weighted_genre_ridgeplots, width=8, height=6)

genre_ridgeplots <- plot_grid(plotlist = list(unweighted_genre_ridgeplots, weighted_genre_ridgeplots),
                              labels = "AUTO", ncol = 1, align = "v")

ggsave(file="figures/fig5extra.svg", plot=genre_ridgeplots, width=8, height=12)

genre_wilcox_tbl <- NULL
for(g in unique(viz_tbl$genre)){
  message(g)
  wilcoxon_comparison_tbl <- viz_tbl %>%
    filter(genre == g) %>%
    select(handle, type, weighted_ideology) %>%
    pivot_wider(values_from = weighted_ideology, names_from = type) %>%
    select(`ideal points`, `perceived ideologies`)
  
  curr_wilcox_tbl <- wilcox.test(wilcoxon_comparison_tbl$`perceived ideologies`, wilcoxon_comparison_tbl$`ideal points`, paired = TRUE, alternative = "less") %>%
    tidy()
  
  genre_wilcox_tbl <- genre_wilcox_tbl %>%
    rbind(c(g, curr_wilcox_tbl))
}

genre_wilcox_tbl <- genre_wilcox_tbl %>%
  data.frame() %>%
  rename(genre = 1, v_statistic = 2, p_value = 3) %>%
  select(genre, v_statistic, p_value, alternative)

kable(genre_wilcox_tbl, "latex")
stargazer(genre_wilcox_tbl)

all_plots <- ls()[grep("plot", ls())]

# for appendix
# adding trump and bernie in the ideal points distribution

barbera_tbl_TB <- barbera_tbl %>%
  filter(handle %in% c("realdonaldtrump", "berniesanders", unique(mturk_tbl$handle))) %>%
  mutate(`ideal points with Trump and Bernie` = barbera_ideology,
          barbera_ideology = ifelse(handle %in% c("realdonaldtrump", "berniesanders"), NA, barbera_ideology)) %>%
  rename(`ideal points` = barbera_ideology) %>%
  pivot_longer(cols = c(2,3), names_to = "type", values_to = "ideology")

trump_bernie_comparison <- barbera_tbl_TB %>%
  ggplot() +
  geom_density(aes(x=ideology)) +
  # geom_vline(data=medians, aes(xintercept=type_median, color=type),
  #           linetype="dashed") +
  facet_wrap(~type) +
  theme_bw() +
  xlim(c(-4,4)) +
  labs(x="weighted ideology",
       fill = "ideology type",
       color = "ideology type")+
  theme(legend.position = c(0.18, 0.85),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size = 14),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

# mturkers with Twitter vs those without
twtr_tbl <- mturk_tbl %>%
  select(handle, mean_rating_attn, mean_rating_attn_twtr) %>%
  pivot_longer(cols = c(2,3), names_to = "rated by", values_to = "perceived ideologies") %>%
  mutate(`rated by` = ifelse(`rated by` == "mean_rating_attn", "all turkers", "turkers with Twitter"))

medians <- twtr_tbl %>%
  group_by(`rated by`) %>%
  summarise(type_median = median(`perceived ideologies`-4, na.rm = T))

twtr_comparison <- twtr_tbl %>%
  ggplot() +
  geom_density(aes(x=`perceived ideologies` -4, fill = `rated by`), alpha = 0.4) +
  geom_vline(data=medians, aes(xintercept=type_median, color=`rated by`),
             linetype="dashed") +
  theme_bw() +
  #xlim(c(-4,4)) +
  labs(x="perceived ideologies",
       fill = "rated by",
       color = "rated by") +
  theme(legend.position = c(0.14, 0.85),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size = 14),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
