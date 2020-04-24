library(tidyverse)
library(gganimate)
library(ggforce)
library(cowplot)
library(hrbrthemes)

# Intro to animation -----

## Grid of x/y coords 30 wide, 10 high repeated for 3 animation steps
## Have a (r)adius and (a)ngle value for plotting
## And a separate ID for each of the 300 points to animate correctly
plotgrid <- tibble(x = 1:30) %>% 
	tidyr::expand(x,y = 1:10,step = 1:3, r = 0.3, a = 0) %>% 
	add_column(id = rep(1:300, each = 3))

## Make three points grow and rotate and change colour
## Points are at (27, 1), (4, 3), and (16, 8)

x_coords <- c(27, 4, 16)
y_coords <- c( 1, 3,  8)

grid_colours <- plotgrid %>% 
	# change radius for steps 2 & 3
	magrittr::inset(
		.$step > 1 & (.$y %in% y_coords & .$x %in% x_coords ), "r", 0.6
	) %>% 
	# change angle for step 3
	magrittr::inset(
		.$step == 3 & (.$y %in% y_coords & .$x %in% x_coords ), "a", 2
	) %>% 
	ggplot(aes(x0 = x, y0 = y, group = id)) +
	ggforce::geom_regon(aes(sides = 4, r = r, angle = a, fill = as.factor(r>0.3))) +
	coord_fixed() +
	cowplot::theme_nothing() +
	transition_states(step) +
	scale_fill_manual(values = c("grey20", "red")) +
	ease_aes("sine-in-out")

anim_save(
	filename = "animations/01_grid_colours.gif", 
	animation = grid_colours, 
	nframes = 300, 
	duration = 10, 
	detail = 3, 
	width = 1600, 
	height = 500, 
	type = "cairo-png", 
	res = 300
)

## Move points around in blocks instead
grid_movement <- plotgrid %>% 
	# Move top half up after step 1
	mutate(y = ifelse(y > 5 & step > 1, y + 3L, y)) %>% 
	# Shift two blocks left/right at step 3
	mutate(x = case_when(
		y > 5  & step == 3 & x < 15  ~ (x - 3L),
		y <= 5 & step == 3 & x >= 15 ~ (x + 3L),
		TRUE ~ x
	)) %>% 
	ggplot(aes(x0=x,y0=y, group = id)) +
	ggforce::geom_regon(aes(sides = 4, r = r, angle = a)) +
	coord_fixed() +
	cowplot::theme_nothing() +
	transition_states(step) +
	ease_aes("cubic-in-out")

anim_save(
	filename = "animations/02_grid_movement.gif", 
	animation = grid_movement, 
	nframes = 300, 
	duration = 10, 
	detail = 3, 
	width = 1600, 
	height = 700, 
	type = "cairo-png", 
	res = 300
)

# Make a line of points rise in a wave pattern
line_wave <- tibble(x = rep(1:20, times = 24), step = rep(-1:22, each = 20)) %>% 
	mutate(y = as.numeric(x == step)) %>% 
	group_by(step) %>% 
	mutate(y = ifelse(lead(y, default = 0) == 1 | lag(y, default = 0) == 1 , 0.5, y)) 

# a few manual fixups at the margins of the animation 
line_wave[21,3] <- 0.5
line_wave[nrow(d)-20, 3] <- 0.5
	
line_wave_anim <- ggplot(line_wave, aes(x,y, group = x)) + 
	geom_point() + 
	transition_time(step) +
	cowplot::theme_nothing()
	
anim_save(
	filename = "animations/03_line_wave.gif", 
	animation = line_wave_anim, 
	nframes = 300, 
	duration = 10, 
	detail = 3, 
	width = 1600, 
	height = 500, 
	type = "cairo-png", 
	res = 300
)

weather <- read_csv("data/weather_data.csv") %>% 
	mutate(name = fct_reorder(name, state, .fun = unique))

bars <- ggplot(weather, aes(x = month, y = rainfall, fill = rainfall, group = 1L)) +
	scale_fill_viridis_c() +
	scale_x_continuous(breaks = 1:12, labels = month.abb) + 
	scale_y_continuous(breaks = c(2,4,6,8,10)) +
	theme_ipsum_tw() +
	theme(
		panel.grid.major.x = element_blank(), 
		panel.grid.minor.x = element_blank(),
		legend.position = "none"
	) +
	labs(
		title = "Average monthly rainfall per station",
		x = NULL,
		y = "Average rainfall (mm)"
		) 

# Need a little cheat to get the static bar plot looking nice but
# the animation working correctly
bars_for_static <- bars +
	geom_col(aes(group = name),position = position_dodge())

ggsave(filename = "figures/01-bars.png", bars_for_static, width = 5, height = 5, dpi = 300)

# Now can add the bars to the animatable version
bars <- bars + geom_col()
	
lines <- ggplot(weather, aes(x = month, y = rainfall, colour = state, group = name)) +
	geom_line() +
	scale_color_ipsum() +
	scale_x_continuous(breaks = 1:12, labels = month.abb) +
	scale_y_continuous(breaks = c(2,4,6,8,10)) +
	theme_ipsum_tw() +
	theme(
		panel.grid.major.x = element_blank(), 
		panel.grid.minor.x = element_blank(),
		legend.position = "bottom"
	) +
	labs(
		title = "Average monthly rainfall per station",
		x = NULL,
		y = "Average rainfall (mm)",
		colour = NULL
	) 

ggsave(filename = "figures/02-lines.png", lines, width = 5, height = 5.5, dpi = 300)

points <- ggplot(weather, aes(x = temp_min, y = temp_max, size = solar, colour = state)) +
	geom_point() +
	scale_colour_ipsum() +
	coord_fixed() +
	theme_ipsum_tw() +
	labs(
		title = "Temperature relationship",
		x = "Minimum temperature",
		y = "Maximum temperature",
		colour = NULL,
		size = "Solar exposure"
	) 
	
ggsave(filename = "figures/03-points.png", points, width = 6, height = 5, dpi = 300)

# What to animate
map2(1:10, 1:10,  
		 ~ggsave(filename = glue::glue("figures/04-transitions_{.x}.png"), plot = ggplot(tibble(x = .x, y = .y), aes(x,y)) +
			 	geom_point(size = 3) +
			 	cowplot::theme_half_open() +
			 	scale_x_continuous(breaks = 1:10, limits = c(1,10)) +
			 	scale_y_continuous(breaks = 1:10, limits = c(1,10)),
			 	width = 3, height = 3
		 )
)
	

# Transitions
ggsave("figures/05-bars_facet.png", bars + facet_wrap(~name), width = 14, height = 7)

anim_save("animations/04_bars_transition.gif", animation = bars +
	transition_states(name) + labs(subtitle = "{closest_state}"), fps = 20, duration = 15, detail = 3, width = 1200, height = 1200, type = "cairo-png", res = 200)

anim_save("animations/05-points_transition.gif", animation = points + transition_time(month) + labs(subtitle = "{month.name[{frame_time}]}"), fps = 20, duration = 10, detail = 3, width = 1200, height = 1000, res = 200, type = "cairo-png")

#Context
ggsave("figures/06-points_facet.png", points + facet_wrap(~month, nrow = 2), width = 10, height = 5)

anim_save("animations/06-points_shadow.gif", animation = points + transition_time(month) + shadow_wake(wake_length = 0.2, wrap = F) + labs(subtitle = "{month.name[{frame_time}]}"), fps = 20, duration = 10, detail = 3, width = 1200, height = 1000, res = 200, type = "cairo-png")

anim_save("animations/07_bars_shadow.gif", animation = bars +
						transition_states(name) + shadow_mark(past = TRUE, future = TRUE, fill = "grey70") + labs(subtitle = "{closest_state}"), fps = 20, duration = 15, detail = 3, width = 1200, height = 1200, type = "cairo-png", res = 200)

# Easings
anim_save("animations/08-points_easing.gif", animation = points + transition_time(month) + ease_aes("cubic-in-out") + labs(subtitle = "{month.name[{frame_time}]}"), fps = 20, duration = 10, detail = 3, width = 1200, height = 1000, res = 200, type = "cairo-png")

anim_save("animations/09_bars_easing.gif", animation = bars +
						transition_states(name) + ease_aes("back-in") + labs(subtitle = "{closest_state}"), fps = 30, duration = 15, detail = 3, width = 1200, height = 1200, type = "cairo-png", res = 200)
