setwd("C:/Users/Subhayan/Work/twitter-landscape/")

library(tidyverse)
library(ggrepel)
library(ggridges)
library(cowplot)
library(svglite)
library(rstatix)

elite_df <- read_csv("data/mturk_ideologies.csv")
elites <- elite_df$handle

elites <- c(elites, "realDonaldTrump", "BernieSanders")

load("data/master_edge_list.Rdata")

following_tbl <- following_df %>%
  as_tibble() %>%
  rename("user" = 1, "following" = 2) %>%
  mutate(user = as.character(user), following = as.character(following))

elite_following_tbl <- following_tbl %>%
  filter(tolower(following) %in% tolower(elites))


elite_following_tbl <- elite_following_tbl %>%
  filter(user != "Tip") # Tip is both elite as well as ordinary user

elite_genres <- read_csv("data/elite_classification.csv")

genres <- c("hard news", "meme", "organization", "political pundit", 
            "political figure",  "brand", "media outlet", "public figure",
            "sports", "entertainment")

elite_activity <- read_csv("data/elites_activity.csv")
relevance_df <- read_csv("data/all_relevance_ideologies.csv")
relevance_df <- relevance_df %>%
  select(userhandle, relevance_kw, relevance_delib) %>%
  mutate(handle = tolower(userhandle)) %>%
  select(-userhandle)

elite_followers_count <- elite_following_tbl %>%
  group_by(following) %>%
  tally() %>%
  rename("elite" = 1, "sample_follower_count" = 2) %>%
  mutate(elite = tolower(elite))

elite_counts <- elite_followers_count %>%
  inner_join(elite_activity, by = c("elite" = "handle"))

genre_tbl <- NULL
for(genre in genres) {
  genre_elites <- elite_genres %>% 
    filter(sector1 == genre | sector2 == genre | sector3 == genre) %>%
    pull(handle) %>% 
    unique() %>%
    tolower()
  
  genre_counts <- elite_counts %>%
    filter(elite %in% genre_elites) %>%
    mutate(genre = genre)
  
  genre_tbl <- genre_tbl %>%
    rbind(genre_counts)
}

# pairwise tests

wilcox_test_followers_holm_tbl <- genre_tbl %>%
  pairwise_wilcox_test(sample_follower_count ~ genre, p.adjust.method = "holm")

wilcox_test_followers_bonf_tbl <- genre_tbl %>%
  pairwise_wilcox_test(sample_follower_count ~ genre, p.adjust.method = "bonf")

t_test_followers_holm_tbl <- genre_tbl %>%
  pairwise_t_test(sample_follower_count ~ genre, p.adjust.method = "holm")

t_test_followers_bonf_tbl <- genre_tbl %>%
  pairwise_t_test(sample_follower_count ~ genre, p.adjust.method = "bonf")


wilcox_test_tweets_holm_tbl <- genre_tbl %>%
  pairwise_wilcox_test(numberoftweets ~ genre, p.adjust.method = "holm")

wilcox_test_tweets_bonf_tbl <- genre_tbl %>%
  pairwise_wilcox_test(numberoftweets ~ genre, p.adjust.method = "bonf")

t_test_tweets_holm_tbl <- genre_tbl %>%
  pairwise_t_test(numberoftweets ~ genre, p.adjust.method = "holm")

t_test_tweets_bonf_tbl <- genre_tbl %>%
  pairwise_t_test(numberoftweets ~ genre, p.adjust.method = "bonf")

followers_boxplot <- ggplot(genre_tbl) +
  geom_boxplot(aes(x=sample_follower_count, y=genre)) +
  theme_bw()

numbeoftweets_boxplot <- ggplot(genre_tbl) +
  geom_boxplot(aes(x=numberoftweets, y=genre)) +
  theme_bw()

# per ordinary user analysis

elite_following_genre_tbl <- elite_following_tbl %>%
  mutate(elite = tolower(following)) %>%
  inner_join(genre_tbl) %>%
  select(-c(following, sample_follower_count, numberoftweets))

user_elite_genre_count <- elite_following_genre_tbl %>%
  group_by(user) %>%
  count(genre)

# do individuals follow non-political elites less than political elites?
user_genre_count <- ggplot(user_elite_genre_count) +
  geom_boxplot(aes(x=n, y=reorder(genre,n))) +
  labs(x="# opinion leaders followed by ordinary users", y = "genre") +
  theme_bw() # yes, they do

# for those users who don't follow any elites of certain genres, insert 0 for those combinations
user_elite_genre_count_full <- expand.grid(unique(user_elite_genre_count$user), unique(user_elite_genre_count$genre)) %>%
  as_tibble() %>% 
  rename("user" = 1, "genre" = 2) %>%
  left_join(user_elite_genre_count) %>%
  mutate(n = ifelse(is.na(n), 0, n))

# "but but ... iNFeRentIaL sTAtS
wilcox_results_less <- user_elite_genre_count_full %>%
  ungroup() %>%
  wilcox_test(n ~ genre, p.adjust.method = "holm", paired = T, alternative = "l")

wilcox_results_less2 <- wilcox_results_less %>%
  select(group1, group2, p.adj, p.adj.signif) %>%
  mutate(alt = "lesser") %>%
  filter(p.adj.signif != "ns") %>%
  select(group1, group2, alt, p.adj, p.adj.signif)

wilcox_results_greater <- user_elite_genre_count_full %>%
  ungroup() %>%
  wilcox_test(n ~ genre, p.adjust.method = "holm", paired = T, alternative = "g")

wilcox_results_greater2 <- wilcox_results_greater %>%
  select(group1, group2, p.adj, p.adj.signif) %>%
  mutate(alt = "greater") %>%
  filter(p.adj.signif != "ns") %>%
  select(group1, group2, alt, p.adj, p.adj.signif)

sig_wilcox_results <- wilcox_results_greater2 %>%
  rbind(wilcox_results_less2) %>%
  arrange(group1, group2)

# compare elites versus non-elites

# for each user, get their number of friends by reading friends list from a long long time ago

friends_folder <- "C:/Users/Subhayan/Google Drive/Research Projects/Ongoing/Affective Polarization Experiment/Pilot/following lists/"
problem_handles <- "Nepal2812"

o_user_friend_tbl <- NULL
for(o_user in unique(elite_following_tbl$user)) {
  print(o_user)
  
  if (o_user %in% problem_handles)
    next
  
  o_user_friends_count <- read_csv(paste0(friends_folder, '/', o_user, '.csv')) %>% nrow()
  o_user_elite_count <- elite_following_tbl %>%
    filter(user == o_user) %>%
    nrow()
  
  o_user_friend_tbl <- o_user_friend_tbl %>%
    rbind(tibble(user = o_user, elite_count = o_user_elite_count, nonelite_count = o_user_friends_count - o_user_elite_count))
}

o_user_friend_long_tbl <- o_user_friend_tbl %>%
  pivot_longer(cols = 2:3, names_to = "type", values_to = "count") %>%
  mutate(type = ifelse(type == "elite_count", "opinion leader", "non opinion leader"))

user_elite_nonelite <- ggplot(o_user_friend_long_tbl) +
  geom_boxplot(aes(x=count, y=type)) +
  labs(x="# of accounts followed") +
  theme_bw()

o_user_friend_long_tbl %>%
  wilcox_test(count ~ type, paired = T, alternative = "greater") %>%
  add_significance()

plot_grid(plotlist = list(user_genre_count, user_elite_nonelite), nrow = 2, align = T)

sig_wilcox_results %>%
  rename(genre1 = 1, genre = 2, alternative = 3) %>%
  datasummary_df(output = "results/per_user_genre_wilcox.tex", fmt = "%.5f")
