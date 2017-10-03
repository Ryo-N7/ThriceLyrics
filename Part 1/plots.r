library(ggplot2)
library(scales)

# 1
df %>% ggplot(aes(x = as.numeric(lengthS))) + 
  geom_histogram(binwidth = 10, 
                 color = 'white',
                 fill = 'darkgreen') +
  scale_y_continuous(breaks = pretty_breaks(), 
                     limits = c(0, 13), expand = c(0, 0)) +  # expand 0,0 to reduce space
  scale_x_continuous(breaks = pretty_breaks(10), 
                     limits = c(0, 420), expand = c(0, 0)) +  # set limits manually
  xlab('Seconds') +
  ylab('# of Songs') +
  labs(title = 'Distribution of Thrice Songs by Length') +
  theme_bw() +
  theme(axis.text = element_text(size = 14, face = "bold", color = "#252525"))

# 2
df %>% ggplot(aes(x = as.numeric(lengthS)/60)) + 
  geom_histogram(binwidth = 0.5, 
                 color = 'white',
                 fill = 'darkgreen') +
  scale_y_continuous(breaks = pretty_breaks(10), 
                     expand = c(0,0), limits = c(0, 30)) +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  xlab('Minutes') +
  ylab('# of Songs') +
  labs(title = 'Distribution of Thrice Songs by Length') +
  theme_bw() +
  theme(axis.text = element_text(size = 14, color = "#252525"),
        axis.title = element_text(size = 14))

#
histogram <- df %>% 
  ggplot(aes(x = as.numeric(lengthS)/60)) + 
  geom_histogram(binwidth = 0.5, 
                 color = "#FFFFFF",
                 fill = "#006400") +
  scale_y_continuous(breaks = pretty_breaks(), expand = c(0, 0), limits = c(0, 7)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  xlab('Minutes') +
  ylab('# of Songs') +
  labs(title = 'Distribution of Thrice Songs by Length') +
  theme_bw() +
  theme(axis.text = element_text(size = 8, color = "#252525"),
        axis.title = element_text(size = 8)) 

# 3

histogram + facet_wrap(~album)



# 4

histogram + facet_grid(album ~.) + 
  geom_smooth(se = FALSE, stat = "bin", bins = 10, col = "#FF3030")

# 5

library(ggjoy)

df %>% 
  ggplot(aes(x = as.numeric(lengthS)/60, y = album)) +
  geom_joy() +
  xlab('Minutes') +
  scale_x_continuous(breaks = pretty_breaks(7))

# 6

joyplot <- df %>% 
  mutate(group = reorder(album, desc(lengthS))) %>%   # reorder based on lengthS (descending)
  ggplot(aes(x = as.numeric(lengthS)/60, y = group, fill = group)) +   
  geom_joy(scale = 2) +                       # scale to set amount of overlap between ridges
  xlab('Minutes') +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = rep(c("#006400", "#404040"), n_distinct(df$album))) +
  theme_bw() +
  theme(legend.position = "none")

joyplot

# 7
hist <- df %>% 
  mutate(group = reorder(album, lengthS)) %>%
  arrange(group) %>% 
  ggplot(aes(x = as.numeric(lengthS)/60, fill = group)) + 
  geom_histogram(binwidth = 0.5, 
                 color = "#FFFFFF") +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  xlab('Minutes') +
  facet_grid(group ~.) +
  scale_fill_manual(values = rep(c("#006400", "#404040"), n_distinct(df$album))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_blank())

library(grid)

pushViewport(viewport(layout = grid.layout(1,2)))

print(joyplot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(hist, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

joyplot2 <- df %>% 
  mutate(group = reorder(album, desc(lengthS))) %>%     
  ggplot(aes(x = as.numeric(lengthS)/60, y = group, fill = group)) +  
  geom_joy(scale = 2) +
  xlab('Minutes') +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = rep(c("#006400", "#404040"), n_distinct(df$album))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

library(gridExtra)

grid.arrange(joyplot2, hist, nrow = 1)
