#=========================================================#
# My attempt to recreate U.S.A flag with R.               #
#                                                         # 
# source: https://www.ushistory.org/betsy/flagetiq3.html  #
#                                                         # 
# ONLY EDUCATIONAL PURPOSE                                #
#=========================================================#


library(ggplot2)
library(dplyr)
library(fontawesome) # Icons display in ggplot2
library(ggtext) # Markdown text in ggplot2
library(showtext) # Display fonts in ggplot2
library(ggtext) # formatting icons like X, LinkedIn, github


sysfonts::font_add_google("Roboto Condensed", "body_font")
sysfonts::font_add_google("Oswald", "title_font")
# Caption stuff for the plot
sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = here::here("docs", "Font Awesome 6 Brands-Regular-400.otf")
)
showtext::showtext_auto()

# Colour for the text

social_link_text_color <- "grey45"

github <- "&#xf09b"
github_username <- "yihunzeleke"
linkedin <- "&#xf08c"
linkedin_usrename <- "linkedin.com/in/yihun-zeleke"
xtwitter <- "&#xe61b"
xtwitter_username <- "@yihunzeleke"
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin};</span> <span style='color: {social_link_text_color}'>{linkedin_usrename}  </span>")
social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{xtwitter};</span> <span style='color: {social_link_text_color}'>{xtwitter_username}</span>")
social_caption_git <- glue::glue("<span style='font-family:\"Font Awesome 6 Brands\";'>{github};</span> <span style='color: {social_link_text_color}'>{github_username}</span>")

plot_caption <- paste0(
  "**50 Stars 13 Stripes**",
  "  |  **Code:** ",
  social_caption_git,
  "  ",
  social_caption_1,
  " |  **Graphics:** ",
  social_caption_2
)

# creating a function that will create star and stored as a data-frame
generate_star <- function(cx, cy, size = 0.03) {
  r_outer <- size
  r_inner <- r_outer * 0.4
  angles <- seq(0, 2 * pi, length.out = 11)[-1] - pi / 2 # 10 points for 5-pointed star
  radii <- rep(c(r_outer, r_inner), length.out = 10)

  data.frame(
    x = cx + radii * cos(angles),
    y = cy + radii * sin(angles)
  )
}

# U.S.A flag color code

us_red <- rgb(red = 179, green = 25, blue = 66, maxColorValue = 255)
us_white <- rgb(red = 255, green = 255, blue = 255, maxColorValue = 255)
us_blue <- rgb(red = 10, green = 49, blue = 97, maxColorValue = 255)

# Flag proportions (official 10:19 ratio)
flag_width <- 1.9
flag_height <- 1

# Create stripes (13 alternating red/white)
stripes <- data.frame(
  ymin = seq(flag_height, 0, length.out = 14)[-1],
  ymax = seq(flag_height, 0, length.out = 14)[-14],
  fill = rep(c(us_red, us_white), length.out = 13)
)

# Create canton (blue field)
canton_width <- 0.76 # 0.4 × flag width
canton_height <- 7 / 13 # 7 stripes

# Generate correct star positions (50 stars in 9 rows) 
# Manual star positions: maybe someone will generate by vectorizing operation: but for some reason 
# - that method doesn't work for me and I use trying the manual method.
star_positions <- data.frame(
  x = c(
    0.063, 0.189, 0.315, 0.441, 0.567, 0.693, 0.126, 0.252, 0.378, 0.504, 0.630,
    0.063, 0.189, 0.315, 0.441, 0.567, 0.693, 0.126, 0.252, 0.378, 0.504, 0.630,
    0.063, 0.189, 0.315, 0.441, 0.567, 0.693, 0.126, 0.252, 0.378, 0.504, 0.630,
    0.063, 0.189, 0.315, 0.441, 0.567, 0.693, 0.126, 0.252, 0.378, 0.504, 0.630,
    0.063, 0.189, 0.315, 0.441, 0.567, 0.693),
  y = c(
    rep(0.960, 6), rep(0.906, 5), rep(0.852, 6), rep(0.798, 5),
    rep(0.744, 6), rep(0.690, 5), rep(0.636, 6), rep(0.582, 5),
    rep(0.528, 6)
  )
)

# Generate all star polygons
stars_df <- data.frame()
for (i in 1:nrow(star_positions)) {
  star <- generate_star(star_positions$x[i], star_positions$y[i])
  star$id <- i
  stars_df <- rbind(stars_df, star)
}

# Create plot

##  13 Stripes 
flag_stripe <- ggplot() +
  geom_rect(
    data = stripes,
    aes(xmin = 0, xmax = flag_width, ymin = ymin, ymax = ymax, fill = fill),
    color = NA
  ) 
## Adding Canton(Blue Union)

flag_blue_canton <- flag_stripe+
  geom_rect(
    aes(
      xmin = 0, xmax = canton_width,
      ymin = flag_height - canton_height, ymax = flag_height
    ),
    fill = us_blue, color = NA
  )

# 50 Stars and positions
flag_stars <- flag_blue_canton +
  geom_polygon(
    data = stars_df,
    aes(x, y, group = id),
    fill = us_white, color = NA
  )

# Flag aesthetics and scaling 

flag_scaling <- flag_stars +
  scale_fill_identity() + # set colors 
  coord_fixed(xlim = c(0, flag_width), ylim = c(0, flag_height))

# Final flag
final_us_flag <- flag_scaling + 
  labs(caption = plot_caption) +
  theme_void() +
  theme(
    # plot.margin = unit(c(0, 30, 0, 0), "mm"),
    plot.caption = element_textbox_simple(halign = 0.95)
  )  
# 
ggsave("imgs/us_flag.png", final_us_flag,
       width = 19, # Width in inches
       height = 10, # Height in inches
       dpi = 300, # High resolution
       units = "in", bg = us_white ) # Unit specification


