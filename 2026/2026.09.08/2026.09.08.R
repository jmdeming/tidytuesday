### Setup ---------------------------------------------------------------------

# Packages
pacman::p_load(
  tidyverse,
  tidytext,
  showtext,
  sysfonts,
  fontawesome,
  ggtext
)

# Utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))

# Register fonts
setup_fonts()

# Fonts
title_font    <- "raleway"
subtitle_font <- "raleway"
body_font     <- "raleway"

base_text_size <- 18

# Colors
title_color      <- "black"
subtitle_color   <- "#4A4340"
body_color       <- "black"
caption_color    <- "#4A4340"
background_color <- "#F4F3EE"


### Data ----------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2026, week = 36)
cafe <- tuesdata$cafe
cappuccino_index <- tuesdata$cappuccino_index


### Titles --------------------------------------------------------------------

plot_title <- "How Long for a Cappuccino?"

plot_subtitle <- glue::glue(
  "Minutes of work at a local café wage needed to afford a cappuccino, by country"
)


# Social caption
social_caption <- create_social_caption(
  tt_year = 2026,
  tt_week = 35,
  source_text = "The Cappuccino Index"
)


# Full caption
plot_caption <- glue::glue(
  "**Note:** The Cappuccino Index estimates the minutes a café worker ",
  "must work to afford a cappuccino, based on average café wages and ",
  "cappuccino prices in each country.",
  "<br><br>",
  social_caption
)

 
### Theme ---------------------------------------------------------------------

my_theme <- theme_minimal(
  base_size = base_text_size,
  base_family = body_font
) +
  theme(
    
    # Background
    plot.background = element_rect(
      fill = background_color,
      color = NA
    ),
    
    panel.background = element_rect(
      fill = background_color,
      color = NA
    ),
    
    # Grid
    panel.grid.major.y = element_blank(),
    
    panel.grid.major.x = element_line(
      linewidth = 0.4,
      color = "white"
    ),
    
    panel.grid.minor = element_blank(),
    
    
    # Axis titles
    axis.title = element_text(
      family = body_font,
      color = body_color,
      size = base_text_size * 1.75
    ),
    
    axis.title.x = element_text(
      margin = margin(t = 10,
                      b = 10)
    ),
    
    axis.title.y = element_text(
      margin = margin(r = 10)
    ),
    
    
    # Axis labels
    axis.text = element_text(
      family = body_font,
      color = body_color,
      size = base_text_size * 1.5
    ),
    
    
    # Plot title
    plot.title = element_text(
      family = title_font,
      size = base_text_size * 2.2,
      face = "bold",
      color = title_color,
      margin = margin(b = 6)
    ),
    
    
    # Subtitle
    plot.subtitle = ggtext::element_markdown(
      family = subtitle_font,
      size = base_text_size * 2,
      color = subtitle_color,
      margin = margin(b = 18)
    ),
    
    
    # Caption
    plot.caption = ggtext::element_textbox_simple(
      family = body_font,
      size = base_text_size * 1.3,
      color = caption_color,
      lineheight = .5,
      halign = 0,
      width = grid::unit(1, "npc"),
      fill = NA,
      box.color = NA,
      padding = margin(0),
      margin = margin(t = 15)
    ),
    
    
    # Margins
    plot.margin = margin(
      t = 20,
      r = 40,
      b = 20,
      l = 20
    )
  )


### Plot ----------------------------------------------------------------------

p <-
ggplot(cappuccino_index,
       aes(x = index,
           y = fct_reorder(country, -index))) +
  geom_point(color = "#6F4E37", alpha = 0.7) +
  geom_text(x = 274, y = 2.5, label = "277 minutes!", size = 10) +
  scale_x_continuous(limits = c(0,300)) +
  labs(title = plot_title,
       subtitle = plot_subtitle,
       caption = plot_caption,
       x = "Minutes",
       y = NULL) +
 
  my_theme

p <- p +
  ggview::canvas(
    width = 8,
    height = 13,
    units = "in",
    dpi = 300
  )

p

### Save ----------------------------------------------------------------------

ggview::save_ggplot(
  plot = p,
  file = here::here(
    "2026/2026.09.08/2026.09.08.png"
  )
)