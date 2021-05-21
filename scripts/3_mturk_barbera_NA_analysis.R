setwd("C:/Users/Subhayan/Work/twitter-landscape/")

library(tidyverse)
library(corrr)
library(cowplot)
library(broom)

exclude_barbera_NAs <- TRUE

# function to scale a vector to between [0,1]
scale_01 <- function(x){(x-min(x))/(max(x)-min(x))}

mturk_tbl <- read_csv("data/mturk_ideologies.csv")
barbera_tbl <- read_csv("data/weak_elite_ideologies.csv")
elite_freq_tbl <- read_csv("data/elites_activity.csv")

barbera_tbl <- barbera_tbl %>%
  rename(handle = username) %>%
  mutate(handle = tolower(handle))

barbera_NAs <- barbera_tbl %>%
  mutate(barbera_NA = is.na(ideology)) %>%
  select(handle, barbera_NA)

##############################################################
# precision recall f1 score analysis

threshold_accuracy_tbl <- NULL
for(threshold in 0:100) {
  message(threshold)
  threshold_mturk_NAs <- mturk_tbl %>%
    mutate(mturk_NA = (NA_percent_attn >= threshold)) %>%
    select(handle, mturk_NA)
  
  threshold_confusion_tbl <- barbera_NAs %>%
    inner_join(threshold_mturk_NAs) %>%
    mutate(tallyNAs = paste(barbera_NA, mturk_NA, sep = "_")) %>%
    group_by(tallyNAs) %>%
    tally()
  
  TP <- threshold_confusion_tbl %>%
    filter(tallyNAs == "TRUE_TRUE") %>%
    pull(n)
  
  TP <- ifelse(length(TP) == 0, 0, TP)
  
  FP <- threshold_confusion_tbl %>%
    filter(tallyNAs == "FALSE_TRUE") %>%
    pull(n)
  
  FP <- ifelse(length(FP) == 0, 0, FP)
  
  TN <- threshold_confusion_tbl %>%
    filter(tallyNAs == "FALSE_FALSE") %>%
    pull(n)
  
  TN <- ifelse(length(TN) == 0, 0, TN)
  
  FN <- threshold_confusion_tbl %>%
    filter(tallyNAs == "TRUE_FALSE") %>%
    pull(n)
  
  FN <- ifelse(length(FN) == 0, 0, FN)
  
  pre <- TP/(TP+FP)
  rec <- TP/(TP+FN)
  f1 <- 2*pre*rec/(pre + rec)
  
  threshold_accuracy_tbl <- threshold_accuracy_tbl %>%
    rbind(tibble(
          thr = threshold,
          precision = pre,
          recall = rec,
          f1_score = f1))
}

threshold_accuracy_tbl <- threshold_accuracy_tbl %>%
  pivot_longer(cols = 2:4, names_to = "measure", values_to = "value")

pre_rec_f1_plot <- ggplot(threshold_accuracy_tbl) +
  geom_line(aes(x=thr, y=value, color=measure)) +
  theme_bw() +
  labs(x = "classifying an elite as NA if at least x% mturkers said NA",
       y = "score comparing the set of NAs according to mturk with set of barbera NAs") +
  theme(legend.position = "bottom")

##########################################################

elite_freq_tbl <- elite_freq_tbl %>%
  mutate(handle = tolower(handle)) %>%
  rename(freq = numberoftweets) %>%
  mutate(freq_scaled = scale_01(freq))

full_tbl <- mturk_tbl %>%
  inner_join(barbera_tbl) %>%
  inner_join(elite_freq_tbl)

NAs <- full_tbl %>%
  select(NA_percent_attn) %>%
  mutate(NA_percent_attn = round(NA_percent_attn)) %>%
  rename(NA_percent = NA_percent_attn)

NA_density <- full_tbl %>%
  select(NA_percent_attn) %>%
  mutate(NA_percent_attn = round(NA_percent_attn)) %>%
  rename(NA_percent = NA_percent_attn) %>%
  ggplot(aes(NA_percent)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 5) +
  geom_density(alpha= 0.2, fill="#FF6666") +
  scale_x_continuous(breaks=seq(0, 100, by = 5)) +
  theme_bw() +
  labs(x = "Percent of MTurkers responded with 'don't know/can't say'",
       y = "Frequency of opinion leaders")

########################################
# non-parametric tests that the mturk % NA for barbera NAs is greater than for barbera non-NAs

barbera_mturk_NAs <- barbera_NAs %>%
  inner_join(mturk_tbl) %>%
  select(handle, barbera_NA, NA_percent_attn) %>%
  mutate(NA_percent_attn = round(NA_percent_attn))

barbera_NA_medians <- barbera_mturk_NAs %>%
  group_by(barbera_NA) %>%
  summarize(median_mturk_NA = median(NA_percent_attn))

wilcox.test(NA_percent_attn ~ barbera_NA, data = barbera_mturk_NAs, alternative = "less") %>% tidy()
kruskal.test(NA_percent_attn ~ barbera_NA, data = barbera_mturk_NAs) %>% tidy()

mturk_NA_distribution_for_barbera_NAs <- barbera_mturk_NAs %>%
  inner_join(barbera_NA_medians) %>%
  ggplot(aes(x=NA_percent_attn, fill = barbera_NA, colour = barbera_NA)) +
  geom_density(alpha = 0.4) +
  geom_vline(aes(xintercept = median_mturk_NA, colour = barbera_NA), linetype = "dashed") +
  theme_bw() +
  labs(x = "Percent of MTurkers responded with 'don't know/can't say'",
       y = "Frequency of opinion leaders")


########################################
# within genres NA distributions

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
    select(handle, NA_percent_attn) %>%
    filter(handle %in% class_elites) %>%
    mutate(genre = class)
  
  all_class_elites_ideologies <- all_class_elites_ideologies %>%
    rbind(class_elites_ideology)
}

genre_NA_density <- all_class_elites_ideologies %>%
  select(NA_percent_attn, genre) %>%
  mutate(NA_percent_attn = round(NA_percent_attn)) %>%
  rename(NA_percent = NA_percent_attn) %>%
  ggplot(aes(NA_percent)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 5) +
  geom_density(alpha= 0.2, fill="#FF6666") +
  scale_x_continuous(breaks=seq(0, 100, by = 10)) +
  facet_wrap(~genre, ncol = 2) +
  theme_bw() +
  labs(x = "Percent of MTurkers responded with 'don't know/can't say'",
       y = "Frequency of opinion leaders")


