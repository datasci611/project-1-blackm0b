library(tidyverse)
library(grid)
library(gridExtra)
rm(list=ls()) 

data(rock)
g.top <- rock %>% 
  ggplot(mapping = aes( x= area,  y=  peri)) + 
    geom_point(color = 'red') +
    geom_smooth(se = FALSE,method='lm') +
    theme_classic() +
    theme(plot.margin = unit(c(-1,6,-25,1),units="points"))
g.bottom <- rock %>% 
  ggplot(mapping = aes(x= area, y=perm)) + 
    geom_point(color = 'blue') +
    geom_smooth(se = FALSE,method='lm') +
    theme_classic() +
    theme(plot.margin = unit(c(0,5,1,1),units="points")) 
grid.arrange(g.top,g.bottom, heights = c(1/5, 4/5)) 

data(occupationalStatus)
occupationalStatus %>%
  as_tibble() %>%
  ggplot(aes(origin,destination)) + 
    geom_tile(aes(fill=n))  +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme_grey() + 
    labs(x = "Father occupation status", y = "Subject's occupation status") + 
    scale_x_discrete(expand = c(0, 0))
    scale_y_discrete(expand = c(0, 0)) 

data(quakes)  
quakes %>%
  as_tibble() %>%
  ggplot() +
    geom_point(mapping = aes(depth,mag, alpha = stations, color = lat), position = "jitter")
    

data(infert)
infert %>% 
  as_tibble() %>%
  gather(key = 'type', 'count',induced,spontaneous,factor_key=TRUE) %>%
  mutate(case = case_when( case == 0 ~ "control", case == 1 ~ "treatment")) %>% 
  ggplot () +
    geom_point(alpha = 0.2, mapping = aes(age,as.factor(count), size = stratum, color = as.factor(type)), 
               stat = 'identity',position =  position_jitter(width = 0, height = 0.2)) +
    facet_grid(case ~ education) + 
    theme_linedraw()