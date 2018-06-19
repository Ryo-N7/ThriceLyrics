library(dplyr)
library(ggplot2)
library(scales)
library(gghighlight)

df <- read.csv("thrice_p1_df.csv")


df %>% glimpse()

df_bg <- df %>% select(-album)


ggplot(df, aes(x = as.numeric(lengthS)/60)) +
  geom_histogram(data = df_bg, fill = "grey", binwidth = 0.25) +
  geom_histogram(color = "black", binwidth = 0.25, fill = "darkgreen") +
  scale_y_continuous(breaks = pretty_breaks(), expand = c(0, 0)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  xlab('Minutes') +
  ylab('# of Songs') +
  labs(title = 'Distribution of Thrice Songs by Length') +
  theme_bw() +
  theme(axis.text = element_text(size = 8, color = "#252525"),
        axis.title = element_text(size = 8)) +
  facet_wrap(~album)


## use gghighlight for specific album?

ggplot(df, aes(x = as.numeric(lengthS)/60)) +
  geom_histogram(color = "black", binwidth = 0.25, fill = "darkgreen") +
  scale_y_continuous(breaks = pretty_breaks(), expand = c(0, 0)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  xlab('Minutes') +
  ylab('# of Songs') +
  labs(title = 'Distribution of Thrice Songs by Length') +
  theme_bw() +
  theme(axis.text = element_text(size = 8, color = "#252525"),
        axis.title = element_text(size = 8)) +
  gghighlight(stringr::str_detect(album, "Vheissu"))

# all Alchemy Index 
ggplot(df, aes(x = as.numeric(lengthS)/60)) +
  geom_histogram(color = "black", binwidth = 0.25, fill = "darkgreen") +
  scale_y_continuous(breaks = pretty_breaks(), expand = c(0, 0)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  xlab('Minutes') +
  ylab('# of Songs') +
  labs(title = 'Distribution of Thrice Songs by Length') +
  theme_bw() +
  theme(axis.text = element_text(size = 8, color = "#252525"),
        axis.title = element_text(size = 8)) +
  gghighlight(stringr::str_detect(album, "The Alchemy*"))

# album names starting with "T"
ggplot(df, aes(x = as.numeric(lengthS)/60)) +
  geom_histogram(color = "black", binwidth = 0.25, fill = "darkgreen") +
  scale_y_continuous(breaks = pretty_breaks(), expand = c(0, 0)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  xlab('Minutes') +
  ylab('# of Songs') +
  labs(title = 'Distribution of Thrice Songs by Length') +
  theme_bw() +
  theme(axis.text = element_text(size = 8, color = "#252525"),
        axis.title = element_text(size = 8)) +
  gghighlight(stringr::str_detect(album, "^T"))



