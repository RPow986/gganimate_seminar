library(tidyverse)
library(gganimate)
library(ggforce)
library(cowplot)
library(hrbrthemes)
library(lubridate)

plotgrid <- tibble(x = 1:30) %>% 
	tidyr::expand(x,y = 1:10,step = 1:3, r = 0.3, a = 0) %>% 
	add_column(id = rep(1:100, each = 3, times = 3))

grid_colours <- plotgrid %>% 
	mutate(r = ifelse(y == 1 & step > 1 & x == 27, 0.6, r)) %>% 
	mutate(r = ifelse(y == 3 & step > 1 & x == 4, 0.6, r)) %>% 
	mutate(r = ifelse(y == 8 & step > 1 & x == 16, 0.6, r)) %>% 
	mutate(a = ifelse(y == 1 & step > 2 & x == 27, 2, a)) %>%
	mutate(a = ifelse(y == 3 & step > 2 & x == 4, 2, a)) %>%
	mutate(a = ifelse(y == 8 & step > 2 & x == 16, 2, a)) %>%
	ggplot(aes(x0=x,y0=y, group = id)) +
	ggforce::geom_regon(aes(sides = 4, r = r, angle = a, fill = as.factor(r>0.3))) +
	coord_fixed() +
	theme_nothing() +
	transition_states(step) +
	scale_fill_manual(values = c("grey20", "red")) +
	ease_aes("sine-in-out")

grid_movement <- plotgrid %>% 
	mutate(y = ifelse(y > 5 & step > 1, y+3, y)) %>% 
	mutate(x = ifelse(y>5 & step >2 & x < 15, x - 3, x)) %>% 
	mutate(x = ifelse(y<=5 & step >2 & x > 15, x + 3, x)) %>% 
	ggplot(aes(x0=x,y0=y, group = id)) +
	ggforce::geom_regon(aes(sides = 4, r = r, angle = a)) +
	coord_fixed() +
	theme_nothing() +
	transition_states(step) +
	ease_aes("cubic-in-out")

line_wave <- tibble(x = rep(1:20, times = 24), step = rep(-1:22, each = 20)) %>% 
	mutate(y = as.numeric(x==step)) %>% 
	group_by(step) %>% 
	mutate(y = ifelse(lead(y, default = 0) == 1 | lag(y, default = 0) == 1 , 0.5, y)) 

# a few manual fixups at the margins 
line_wave[21,3] <- 0.5
line_wave[nrow(d)-20, 3] <- 0.5
	
line_wave_anim <- ggplot(line_wave, aes(x,y, group = x)) + 
	geom_point() + 
	transition_time(step) +
	theme_nothing()
	
animate(a, detail = 3, width = 7, height = 3, units = "cm", res = 300)
