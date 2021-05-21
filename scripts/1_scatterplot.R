setwd("C:/Users/Subhayan/Work/twitter-landscape/")

library(tidyverse)
library(ggrepel)
library(ggridges)
library(cowplot)
library(svglite)

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

#top 20 elites
top_20 <- elite_following_tbl %>%
  pull(following) %>%
  table() %>%
  data.frame() %>%
  as_tibble() %>%
  arrange(-Freq) %>%
  head(20)

elite_following_tbl <- elite_following_tbl %>%
  filter(user != "Tip")

elite_genres <- read_csv("data/elite_classification.csv")

genres <- c("hard news", "meme", "organization", "political pundit", 
            "political figure",  "brand", "media outlet", "public figure",
            "sports", "entertainment")


elite_activity <- read_csv("data/elites_activity.csv")
relevance_df <- read_csv("../echo-chamber-exp/important_results/rnr/oct 9/all_relevance_ideologies.csv")
relevance_df <- relevance_df %>%
  select(userhandle, relevance_kw, relevance_delib) %>%
  mutate(handle = tolower(userhandle)) %>%
  select(-userhandle)

relevance_df %>%
  filter(handle %in% tolower(top_20$.))

all_genre_info <- NULL

for(genre in genres) {
  
  genre_elites <- elite_genres %>% 
    filter(sector1 == genre | sector2 == genre | sector3 == genre) %>%
    pull(handle) %>% 
    unique()
  
  n_elites <- length(genre_elites)
  
  unique_followers_genre <- elite_following_tbl %>%
    filter(following %in% genre_elites) %>%
    pull(user) %>%
    unique() %>%
    length()
  
  genre_activity <- elite_activity %>% 
    filter(handle %in% tolower(genre_elites)) %>%
    pull(numberoftweets) %>%
    sum()
  
  genre_median_relevance <- relevance_df %>%
    filter(handle %in% tolower(genre_elites)) %>%
    pull(relevance_kw) %>%
    median()
  
  all_genre_info <- all_genre_info %>%
    rbind(c(genre = genre,
            elite_count = n_elites,
            unique_followers = unique_followers_genre,
            number_of_tweets = genre_activity,
            median_relevance = genre_median_relevance)) %>%
    as_tibble()
}

all_genre_info <- all_genre_info %>%
  mutate(elite_count = as.numeric(elite_count),
         unique_followers = as.numeric(unique_followers),
         number_of_tweets = as.numeric(number_of_tweets)) %>%
  mutate(unique_followers_per_elite = unique_followers / elite_count,
         number_of_tweets_per_elite = number_of_tweets / elite_count)

fig1a_plot <- all_genre_info %>%
  ggplot(aes(x=unique_followers, y=number_of_tweets)) +
  geom_point(shape = 21, aes(size=elite_count, fill=as.numeric(median_relevance))) +
  scale_fill_gradient(low="white", high="red") +
  scale_size_continuous(range = c(4, 16.4)) +
  geom_text_repel(aes(label = genre),
                  size = 4.6) +
  labs(x="# unique followers", y="# tweets", size="# elites", fill = "political relevance") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.position = c(0.23, 0.74),
        legend.box = "horizontal",
        legend.text = element_text(size=8),
        legend.title = element_text(size = 9), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key.size = unit(1, "lines")) +
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 10))


all_class_elites_relevance <- NULL
for(class in genres) {
  message(class)
  class_elites <- elite_genres %>%
    filter(sector1 %in% class | sector2 %in% class | sector3 %in% class) %>%
    pull(handle) %>% tolower()
  
  all_class_elites_relevance <- relevance_df %>%
    filter(handle %in% class_elites) %>%
    mutate(class = class) %>%
    rbind(all_class_elites_relevance)
  
}

fig1b_plot <- ggplot(all_class_elites_relevance) +
  stat_density_ridges(aes(x = relevance_kw, y = class), fill = "skyblue", quantile_lines = TRUE, quantiles = 2) +
  labs(x="political relevance score", y="genre") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))


fig1 <- plot_grid(plotlist = list(fig1a_plot, fig1b_plot), ncol = 2,
                  align = "h", axis = "b", rel_widths = c(1.5, 1),
                  labels = "AUTO")

ggsave(file="figures/fig1.svg", plot=fig1, width=10, height=5.2)
ggsave(file="figures/fig1x.jpg", device = "jpeg", plot=fig1, width=10, height=5.2)
