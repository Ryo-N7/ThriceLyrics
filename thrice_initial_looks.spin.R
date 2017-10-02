# Packages:
library(stringr)
library(lubridate)
library(gridExtra)
library(tidyverse)
library(scales)
library(ggplot2)

# load and tidy  ----------------------------------------------------------

df <- read.csv('thrice.df.csv', header = TRUE, stringsAsFactors = FALSE)
str(df, list.len = 3)

df2 <- read.csv('thrice.df.csv', header = FALSE, stringsAsFactors = FALSE)
str(df2, list.len = 3)
# very important to set header = TRUE if have variable names already
# or else your column names will appear on their own in the first row!

# Examining dataframes:
str(df)
str(df, list.len = 4)
str(df, vec.len = 3, nchar.max = 20)
glimpse(df)
# song ID, year, track num = integer
# all else chr


# use lubridate pkg to tranform length and lengthS
# turn album var into a factor variable for each unique album!
?ms()  # transforms chr/num vector into period object
?seconds()  # create period object in seconds

df <- df %>% 
  mutate(album = factor(album, levels = unique(album)),
         year = factor(year, levels = unique(year)),
         length = ms(length),
         lengthS = seconds(length))

df2 <- df %>% mutate(
  length = as.duration(length),
  lengthS = duration(second = length))


?ms()
str(df$length)
str(df$lengthS)
str(df, list.len = 8, max.level = 2)
glimpse(df)
# both length and lengthS are now Period/S4 variables!
# album is a factor!

# Explore our data -----------------------------------------------


length(unique(df$album))  # How many total albums have Thrice released?? (Counting each element from AlchemyIndex...)
df %>% select(album) %>% n_distinct()  # number of Thrice albums (so far..!)

df %>% select(album, year) %>% unique()  # list of all Thrice albums (so far...!)

albums_lengths <- df %>% 
  group_by(album, year) %>% 
  summarise(SongNum = n(),
            duration = as.duration(seconds_to_period(sum(lengthS))))  

str(albums_lengths)

albums_lengths %>% 
  arrange(desc(duration))


df %>% 
  group_by(year, album) %>% 
  summarise(SongNum = n(),
            durationInMinutes = sum(lengthS)/60) %>%    # lengthS in seconds / 60 to get in minutes!
  arrange(desc(durationInMinutes))

# song lengths
song_lengths <- df %>% 
  group_by(title, album) %>% 
  summarise(duration = as.duration(sum(lengthS)))

song_lengths %>% arrange(desc(duration))
  
# works if NOT use seconds_to_period(), but minutes still in base-10... not very elegant but w/e
# Major/Minor and Vheissu are longest albums both totalling up to a bit over 49 mins!
# Although if we took The Alchemy Index in its entirety, then W/F/A/E totalled up would be 
df %>% 
  filter(grepl("Index", album)) %>% 
  summarise(duration_minutes = sum(lengthS)/60)
# or use stringr pkg's str_detect()
df %>% 
  filter(str_detect(album, "Index")) %>% 
  summarise(duration_minutes = seconds_to_period(sum(lengthS)))

df %>% 
  filter(album == "Vheissu") %>% 
  summarise(duration_minutes = as.duration(sum(lengthS)))

df %>% 
  group_by(album) %>% 
  summarise(duration_minutes = as.duration(sum(lengthS)))

df %>% 
  group_by(title) %>% 
  summarise(duration_song = as.duration(sum(lengthS)))


# Plotting! ---------------------------------------------------------------

df %>% ggplot(aes(x = as.numeric(lengthS))) + 
  geom_histogram(binwidth = 10, 
                 color = 'white',
                 fill = 'darkgreen') +
  scale_y_continuous(breaks = pretty_breaks(), 
                      limits = c(0, 13), expand = c(0, 0)) +   # expand 0,0 to reduce space
  scale_x_continuous(breaks = pretty_breaks(10), 
                     limits = c(0, 420), expand = c(0, 0)) +  # set limits manually
  xlab('Seconds') +
  ylab('# of Songs') +
  labs(title = 'Distr. of Songs by Length') +
  theme_bw() +
  theme(axis.text = element_text(size = 14, face = "bold", color = "#252525"))

# by minutes along x-axis
df %>% ggplot(aes(x = as.numeric(lengthS)/60)) + 
  geom_histogram(binwidth = 0.5, 
                 color = 'white',
                 fill = 'darkgreen') +
  scale_y_continuous(breaks = pretty_breaks(10), 
                     expand = c(0,0), limits = c(0, 30)) +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  xlab('Minutes') +
  ylab('# of Songs') +
  labs(title = 'Distr. of Songs by Length') +
  theme_bw() +
  theme(axis.text = element_text(size = 14, color = "#252525"),
        axis.title = element_text(size = 14))

# facet by album? facet_wrap() vs facet_grid()
histogram <- df %>% 
  ggplot(aes(x = as.numeric(lengthS)/60)) + 
  geom_histogram(binwidth = 0.5, 
                 color = "#FFFFFF",
                 fill = "#006400") +
  scale_y_continuous(breaks = pretty_breaks(), expand = c(0, 0), limits = c(0, 7)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  xlab('Minutes') +
  ylab('# of Songs') +
  labs(title = 'Distr. of Songs by Length') +
  theme_bw() +
  theme(axis.text = element_text(size = 8, color = "#252525"),
        axis.title = element_text(size = 8)) 

histogram

histogram + facet_wrap(~album)
# hard to interpret

# grid
histogram + facet_grid(~album)
# better but hard to understand the differences between albums

# the other way around?
histogram + facet_grid(album ~.)
# can compare each of the histograms, but the bars make it hard to discern differences...
# try adding in trend lines for each?

histogram + facet_grid(album ~.) + 
  geom_smooth(se = FALSE, stat = "bin", bins = 10, col = "red")

# let's try alternating the colors for each album 
# also, reorder the albums in order of song lengths
# histogram of joy plot

hist <- df %>% 
  mutate(group = reorder(album, lengthS)) %>%
  arrange(group) %>% 
  ggplot(aes(x = as.numeric(lengthS)/60, fill = group)) + 
  geom_histogram(binwidth = 0.5, 
                 color = "#FFFFFF") +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  xlab('Minutes') +
  ylab('# of Songs') +
  labs(title = 'Distr. of Songs by Length') +
  facet_grid(group ~.) +
  scale_fill_manual(values = rep(c("#006400", "#404040"), length(unique(df$album)))) +
  theme_bw() +
  theme(legend.position = "none")

hist
# can somewhat see that Illusion of Safety has the least songs in 


# Joy Plots ---------------------------------------------------------------

# JoyPlot
library(ggjoy)
?arrange()

df %>% 
  ggplot(aes(x = as.numeric(lengthS)/60, y = album)) +
  geom_joy() +
  xlab('Minutes') +
  scale_x_continuous(breaks = pretty_breaks(7))

# attempt 1
df %>% 
  mutate(group = reorder(album, desc(lengthS))) %>%     # reorder based on lengthS
  ggplot(aes(x = as.numeric(lengthS), y = group, fill = album)) +  # fill on group as they are ordered instead of just album! worked! :D
  geom_joy(scale = 2) +     # scale to set overlap between ridges
  xlab('Seconds') +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  scale_fill_manual(values = rep(c("#006400", "#404040"), length(unique(df$album)))) +
  theme_bw() +
  theme(legend.position = "none")

# attempt 2 and success!
joyplot <- df %>% 
  mutate(group = reorder(album, desc(lengthS))) %>%     # reorder based on lengthS
  ggplot(aes(x = as.numeric(lengthS)/60, y = group, fill = group)) +  # fill on group as they are ordered instead of just album! worked! :D
  geom_joy() +
  xlab('Minutes') +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = rep(c("#006400", "#404040"), length(unique(df$album)))) +
  theme_bw() +
  theme(legend.position = "none")

joyplot
# near the longer albums, what really sets them apart is the small number of songs that are 
# 6 minutes or longer... otherwise mainly max out at around 4 minute long songs
# AI-Water should not really count as it's position is due to a 6 min long instrumental!
# the two shortest, seen in "TBEATBN" and "Identity Crisis" are also ~minute long instrumentals!


# add vertical mean song length line? geom_vline

# vertical line for each lengthS by album?

library(grid)
pushViewport(viewport(layout = grid.layout(1,2)))
print(joyplot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(hist, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

gridExtra::grid.arrange(joyplot, hist, nrow = 1)
# doesn't look very good in this instance...
?grid.arrange


albums_lengths %>% mutate(group = reorder(album, desc(duration))) %>% 
  ggplot(aes(x = as.numeric(duration), y = group, fill = group)) +  # fill on group as they are ordered instead of just album! worked! :D
  geom_joy() +
  xlab('seconds') +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = rep(c("#006400", "#404040"), length(unique(song_lengths$album)))) +
  theme_bw() +
  theme(legend.position = "none")

albums_lengths %>% str()
albums_lengths %>% select(duration) %>% class()
str(df)
class(albums_lengths$duration)
class(df$length)
albums_lengths$duration %>% 
  seconds_to_period() %>% 
  as.numeric()  # this converts back into seconds though...
