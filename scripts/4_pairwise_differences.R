setwd("C:/Users/Subhayan/Work/twitter-landscape/")

library(tidyverse)
library(ggrepel)
library(ggridges)
library(cowplot)
library(svglite)
library(rstatix)
library(modelsummary)

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

# number of users who don't follow any elites
length(unique(following_tbl$user)) - length(unique(elite_following_tbl$user))


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

user_elite_genre_count2 <- user_elite_genre_count %>%
  mutate(genre2 = ifelse(genre %in% c("hard news", "media outlet", "political figure", "political pundit"), "p", "np")) %>%
  group_by(user, genre2) %>%
  summarize(n2 = sum(n)) %>%
  pivot_wider(names_from = genre2, values_from = n2) %>%
  mutate(p = ifelse(is.na(p), 0, p),
         np = ifelse(is.na(np), 0, np)) %>%
  mutate(np_p_ratio = np/p) %>%
  mutate(inf_or_no = (np_p_ratio == Inf))

user_elite_genre_count2 %>%
  pull(inf_or_no) %>%
  table()

# FALSE  TRUE 
# 6246  2403 

# of the users who followed at least 1 opinion leader, 2403/(2403+6246) = ~28% didn't follow a single political or news / media opinion leader

user_elite_genre_count2 %>%
  filter(np_p_ratio < Inf) %>%
  pull(np_p_ratio) %>%
  median()

#4.5. those who followed at least 1 political (including news and media) opinion leader and at least 1 non-political opinion leader, 
# were more likely to follow, on average, 8.4 times (median 4.5) as many non-political opinion leader compared to political opinion leaders

# do individuals follow non-political elites less than political elites?
user_genre_count <- ggplot(user_elite_genre_count_full) +
  geom_boxplot(aes(x=n, y=reorder(genre,n))) +
  labs(x="# opinion leaders followed by ordinary users", y = "genre") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) # yes, they do

# for those users who don't follow any elites of certain genres, insert 0 for those combinations
user_elite_genre_count_full <- expand.grid(unique(user_elite_genre_count$user), unique(user_elite_genre_count$genre)) %>%
  as_tibble() %>% 
  rename("user" = 1, "genre" = 2) %>%
  left_join(user_elite_genre_count) %>%
  mutate(n = ifelse(is.na(n), 0, n))

# wilcox test
wilcox_results_less <- user_elite_genre_count_full %>%
  ungroup() %>%
  wilcox_test(n ~ genre, p.adjust.method = "holm", paired = T, alternative = "l")

wilcox_results_less2 <- wilcox_results_less %>%
  select(group1, group2, statistic, p.adj, p.adj.signif) %>%
  mutate(alt = "lesser") %>%
  filter(p.adj.signif != "ns") %>%
  select(group1, group2, statistic, alt, p.adj, p.adj.signif)

wilcox_results_greater <- user_elite_genre_count_full %>%
  ungroup() %>%
  wilcox_test(n ~ genre, p.adjust.method = "holm", paired = T, alternative = "g")

wilcox_results_greater2 <- wilcox_results_greater %>%
  select(group1, group2, statistic, p.adj, p.adj.signif) %>%
  mutate(alt = "greater") %>%
  filter(p.adj.signif != "ns") %>%
  select(group1, group2, statistic, alt, p.adj, p.adj.signif)

wilcox_results_unequal <- user_elite_genre_count_full %>%
  ungroup() %>%
  wilcox_test(n ~ genre, p.adjust.method = "holm", paired = T)

wilcox_results_unequal2 <- wilcox_results_unequal %>%
  select(group1, group2, statistic, p.adj, p.adj.signif) %>%
  mutate(alt = "unequal") %>%
  filter(p.adj.signif == "ns") %>%
  select(group1, group2, statistic, alt, p.adj, p.adj.signif)

sig_wilcox_results <- wilcox_results_greater2 %>%
  rbind(wilcox_results_less2) %>%
  rbind(wilcox_results_unequal2) %>%
  arrange(group1, group2)

user_elite_genre_count_full %>% 
  group_by(genre) %>%
  summarise(median_n = median(n),
            mean_n = mean(n)) %>% 
  arrange(desc(median_n))

# # A tibble: 10 x 2
# genre            median_n
# <chr>               <dbl>
# 1 entertainment          10
# 2 brand                   1
# 3 media outlet            1
# 4 political figure        1
# 5 public figure           1
# 6 sports                  1
# 7 hard news               0
# 8 meme                    0
# 9 organization            0
# 10 political pundit       0

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
  geom_boxplot(aes(x=count, y=reorder(type, count))) +
  labs(x="# of accounts followed by ordinary users", y = "type") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

o_user_friend_long_tbl %>%
  wilcox_test(count ~ type, paired = T, alternative = "greater") %>%
  add_significance() %>%
  mutate(alternative = "greater") %>%
  select(group1, group2, alternative, statistic, p, p.signif) %>%
  rename(genre1 = 1, genre2 = 2) %>%
  datasummary_df(output = "results/per_user_elite_vs_nonelite.tex")

rnr_plot1 <- plot_grid(plotlist = list(user_genre_count, user_elite_nonelite),
          nrow = 2,
          align = T,
          rel_heights = c(3.5, 1),
          labels = LETTERS[1:2])

ggsave(file="figures/svg/fig2.svg", plot=rnr_plot1, width=10, height=7)
ggsave(file="figures/jpg/fig2.jpg", device = "jpeg", plot=rnr_plot1, width=10, height=7)

sig_wilcox_results %>%
  rename(genre1 = 1, genre2 = 2, alternative = "alt") %>%
  datasummary_df(output = "results/per_user_genre_wilcox.tex", fmt = "%.5f")
