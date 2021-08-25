setwd("C:/Users/Subhayan/Work/twitter-landscape/")

library(tidyverse)
library(corrr)
library(cowplot)
library(broom)
library(modelsummary)
library(knitr)
library(ggridges)
library(diptest)

exclude_barbera_NAs <- FALSE
MTurk_NA_threshold <- 100 # set to 100 for all elites

# function to scale a vector to between [0,1]
scale_01 <- function(x){(x-min(x))/(max(x)-min(x))}

mturk_tbl <- read_csv("data/mturk_ideologies.csv")
barbera_tbl <- read_csv("data/weak_elite_ideologies.csv")
elite_freq_tbl <- read_csv("data/elites_activity.csv")

elite_freq_tbl <- elite_freq_tbl %>%
  mutate(handle = tolower(handle)) %>%
  rename(freq = numberoftweets) %>%
  mutate(freq_scaled = scale_01(freq))

# plot without NAs
barbera_tbl_NA_comparison <- barbera_tbl %>%
  mutate(handle = tolower(username)) %>%
  mutate(recoded_ideology = ifelse(ideology == -Inf, -3, ideology)) %>%
  mutate(recoded_ideology = ifelse(recoded_ideology == Inf, 3, recoded_ideology)) %>%
  select(handle, recoded_ideology) %>%
  inner_join(elite_freq_tbl) %>%
  mutate(weighted_recoded_ideology = recoded_ideology * freq_scaled)

dip.test(barbera_tbl_NA_comparison$recoded_ideology[!is.na(barbera_tbl_NA_comparison$recoded_ideology)])

dip.test(barbera_tbl_NA_comparison$weighted_recoded_ideology[!is.na(barbera_tbl_NA_comparison$weighted_recoded_ideology)])

unweighted_withoutNAs <- barbera_tbl_NA_comparison %>%
  ggplot() +
  geom_density(aes(recoded_ideology)) +
  theme_bw() +
  labs(x="unweighted ideologies excluding NAs") +
  ylim(c(0,1.8))

weighted_withoutNAs <- barbera_tbl_NA_comparison %>%
  ggplot() +
  geom_density(aes(weighted_recoded_ideology)) +
  theme_bw() +
  labs(x="weighted ideologies excluding NAs") +
  ylim(c(0,1.8))

robustness_plot <- plot_grid(plotlist =  list(unweighted_withoutNAs, weighted_withoutNAs), labels = "AUTO")

ggsave(file="figures/FigA1-robustness.svg", plot=robustness_plot, width=8, height=6)

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
  
full_tbl <- mturk_tbl %>%
  inner_join(barbera_tbl) %>%
  inner_join(elite_freq_tbl)

# weighted ideologies within primary categories

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
    filter(sector1 %in% class) %>% # primary category only
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


# RnR: unimodal?
unimodality_p_values <- NULL
for(g in unique(viz_tbl$genre)) {
  for(t in c("perceived ideologies", "ideal points")) {
    
    message(paste(g, " : ", t))
    
    dip_test <- viz_tbl %>% 
      filter(genre == g, type == t) %>% 
      pull(ideology) %>% 
      dip.test() %>%
      tidy()
    
    unimodality_p_values <- unimodality_p_values %>%
      rbind(tibble_row(genre = g, t = t, HDS = dip_test$statistic, p_value = dip_test$p.value))
    
  }
}

# test
# unimodality_p_values %>%
#   ggplot() +
#   geom_bar(aes(x=HDS, y=genre, fill = t), stat = "identity") +
#   facet_wrap(~t) +
#   geom_vline(xintercept = 0.05, linetype = "dashed", color = "red") +
#   theme_bw() +
#   labs(x="Hartigan's Dip Statistic")

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
    rbind(tibble_row(genre = g, V_statistic = curr_wilcox_tbl$statistic, p_value = curr_wilcox_tbl$p.value, alternative = curr_wilcox_tbl$alternative))
}

# genre_wilcox_tbl %>%
#   datasummary_df("results/genre_unweighted_wilcox.tex", fmt = "%.5f")

# within SINGLE genres weighted

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

ggsave(file="figures/FigA3-robustness.svg", plot=weighted_genre_ridgeplots, width=8, height=6)

# ggsave(file="figures/fig5extra.svg", plot=genre_ridgeplots, width=8, height=12)

# RnR: unimodal?
unimodality_p_values <- NULL
for(g in unique(viz_tbl$genre)) {
  for(t in c("perceived ideologies", "ideal points")) {
    
    message(paste(g, " : ", t))
    
    dip_test <- viz_tbl %>% 
      filter(genre == g, type == t) %>% 
      pull(weighted_ideology) %>% 
      dip.test() %>%
      tidy()
    
    HDS <- dip_test %>%
      pull(statistic)
    
    p_value <- dip_test %>%
      pull(p.value)
    
    unimodality_p_values <- unimodality_p_values %>%
      rbind(tibble_row(genre = g, t = t, HDS = HDS, p_value = p_value))
    
  }
}

# unimodality_p_values %>%
#   # mutate(modality = ifelse(p_value >= 0.05, "unimodal", "not unimodal")) %>%
#   rename("ideology type" = 2) %>%
#   datasummary_df(output = "results/genre_weighted_unimodality.tex")

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
    rbind(tibble_row(genre = g, V_statistic = curr_wilcox_tbl$statistic, p_value = curr_wilcox_tbl$p.value, alternative = curr_wilcox_tbl$alternative))
}

# genre_wilcox_tbl %>%
#   datasummary_df(output = "results/genre_weighted_wilcox.tex", fmt = "%.5f")


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

ordinary_ideologies <- read_csv("data/ordinary_ideologies.csv")

withoutNAs <- ordinary_ideologies %>%
  mutate(recoded_ideology = ifelse(ideology == -Inf, -3, ideology),
         recoded_ideology = ifelse(recoded_ideology == Inf, 3, recoded_ideology)) %>%
  ggplot() +
  geom_density(aes(recoded_ideology)) +
  theme_bw() +
  labs(x = paste0("ideal points (N = ", nrow(ordinary_ideologies[!is.na(ordinary_ideologies$ideology),]), ")")) +
  ylim(c(0,1.9))

withNAs <- ordinary_ideologies %>%
  mutate(recoded_ideology = ifelse(is.na(ideology), 0, ideology)) %>%
  mutate(recoded_ideology = ifelse(recoded_ideology == -Inf, -3, recoded_ideology),
         recoded_ideology = ifelse(recoded_ideology == Inf, 3, recoded_ideology)) %>%
  ggplot() +
  geom_density(aes(recoded_ideology)) +
  theme_bw() +
  labs(x = paste0("ideal points (N = ", nrow(ordinary_ideologies), ")" )) +
  ylim(c(0, 1.9))

ordinary_ideology_plot <- plot_grid(plotlist = list(withoutNAs, withNAs), labels = "AUTO")

ggsave(file="figures/FigA4-robustness.svg", plot=ordinary_ideology_plot, width=8, height=6)
