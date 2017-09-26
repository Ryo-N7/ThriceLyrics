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

df %>% select(album) %>% unique()  # list of all Thrice albums (so far...!)


albums_lengths <- df %>% 
  group_by(album, year) %>% 
  summarise(SongNum = n(),
            duration = as.duration(seconds_to_period(sum(lengthS))))   # this isnt even correct...??
str(albums_lengths)


df %>% 
  group_by(year, album) %>% 
  summarise(SongNum = n(),
            durationInMinutes = sum(lengthS)/60) %>%    # lengthS in seconds / 60 to get in minutes!
  arrange(desc(durationInMinutes))

  
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
  summarise(duration_minutes = seconds_to_period(sum(lengthS)))

df %>% 
  group_by(album) %>% 
  summarise(duration_in_mins = sum(length))
str(df, max.level = 2)




# Plotting! ---------------------------------------------------------------

df %>% ggplot(aes(x = as.numeric(lengthS))) + 
  geom_histogram(binwidth = 10, 
                 color = 'white',
                 fill = 'darkgreen') +
  scale_y_continuous(breaks = pretty_breaks(), expand = c(0, 0), limits = c(0, 13)) +
  scale_x_continuous(breaks = pretty_breaks(10), expand = c(0, 0), limits = c(0, 420)) +
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
  scale_y_continuous(breaks = pretty_breaks(10), expand = c(0,0), limits = c(0, 30)) +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  xlab('Minutes') +
  ylab('# of Songs') +
  labs(title = 'Distr. of Songs by Length') +
  theme_bw() +
  theme(axis.text = element_text(size = 14, color = "#252525"),
        axis.title = element_text(size = 14))

# facet by album? facet_wrap() vs facet_grid()
df %>% ggplot(aes(x = as.numeric(lengthS)/60)) + 
  geom_histogram(binwidth = 0.5, 
                 color = 'white',
                 fill = 'darkgreen') +
  scale_y_continuous(breaks = pretty_breaks(), expand = c(0, 0), limits = c(0, 7)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  xlab('Minutes') +
  ylab('# of Songs') +
  labs(title = 'Distr. of Songs by Length') +
  theme_bw() +
  theme(axis.text = element_text(size = 8, color = "#252525"),
        axis.title = element_text(size = 8)) +
  facet_wrap(~album)

# histogram of joy plot
scale_x_reordered <- function(..., sep = "___") {    # from David Robinson's github.
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

hist <- df %>% 
  arrange(desc(as.numeric(lengthS)/60)) %>%
  ggplot(aes(x = as.numeric(lengthS)/60)) + 
  geom_histogram(binwidth = 0.5, 
                 color = 'white',
                 fill = 'darkgreen') +
  scale_y_continuous(breaks = pretty_breaks(10)) +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  xlab('Minutes') +
  ylab('# of Songs') +
  labs(title = 'Distr. of Songs by Length') +
  facet_grid(album ~ ., scale = "free_x") +
  scale_x_reordered()

hist

# Joy Plots ---------------------------------------------------------------

# JoyPlot
library(ggjoy)
?arrange()

joyplot <- df %>% 
  ggplot(aes(x = as.numeric(lengthS)/60, y = album)) +
  geom_joy() +
  xlab('Minutes') +
  scale_x_continuous(breaks = pretty_breaks(10))
joyplot

df %>% mutate(group = reorder(album, lengthS)) %>%     # reorder based on lengthS
  ggplot(aes(x = as.numeric(lengthS)/60, y = group)) +
  geom_joy() +
  xlab('Minutes') +
  scale_x_continuous(breaks = pretty_breaks(10))
# vertical line for each lengthS by album?

library(grid)
pushViewport(viewport(layout = grid.layout(1,2)))
print(joyplot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(hist, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

gridExtra::grid.arrange(joyplot, hist)
# doesn't look very good in this instance...



# Song writers for Thrice  ------------------------------------------------

# not much meaning for this data as 99.9999% of songs written by Dustin, maybe for some other band.

writersAll <- paste(df$writers, collapse=', ')        # turn all artists into one list. each separated by commas (collapse = ', ')
writersAll <- str_replace_all(writersAll, ',,', ',')  # fix any double-commas typos
glimpse(writersAll)  # still one gigantic chr list

writersAll <- unlist(strsplit(writersAll, ', ')) # unlist writers. separated by the commas. (?), each appear "individually".
glimpse(writersAll)   # now a character vector of length 104!


writersList <- sort(unique(writersAll))     # List of all UNIQUE writers.
writersList    # Dustin, Eddie, Ian Stift, Riley     (99% DUstin tho)

library(stringr)
writersListLabel <- str_replace_all(writersList, ' ', '_') # label of writers, replace " " with "_" for calling purposes
writersListLabel   # now Dustin_Kensrue, Ian_Stift   etc.


dfWriters <- df # copy df into new dfWriters dataframe

for(i in 1:length(writersList)){
  dfWriters[,writersListLabel[i]] <- str_detect(dfWriters$writers, writersList[i]) # detect per each row (song) which writer in WriterList appears T/F
}

buffer <- dfWriters[, 10:13]   # all rows with writers as T/F. 

writers <-  data.frame(writer = writersList,           # name of writer from writerlist
                       nSongs = apply(buffer, 2, sum), # number of songs written by writer from writerlist
                       row.names = NULL,
                       stringsAsFactors = F) 

writers   # Dustin: 101, Eddie: 1, Ian Stift: 1, Riley: 1

writers <- writers %>%
  arrange(desc(nSongs))     # not much to see here as 99.999% of Thrice songs written by Dustin...LOL.

writers

str(writers)   # writer = chr
writers %>%
  mutate(writer = as.factor(writer)) %>%
  mutate(writer = reorder(writer, nSongs)) %>%
  top_n(10)           # ... i mean it's mainly just Dustin writing the songs...
