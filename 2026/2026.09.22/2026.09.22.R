pacman::p_load(
  tidyverse,
  patchwork
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

#usethis::create_github_token()
#gitcreds::gitcreds_set()

tuesdata <- tidytuesdayR::tt_load(2026, week = 38)
urban <- tuesdata$urban

# Prepare data
plot_data <- urban |> 
  filter(countryOrTerritoryName == "United States of America",
         year == 2020) |> 
  arrange(averageShareOfGreenAreaInCityUrbanAreaPct) |> 
  mutate(my_city = str_replace_all(cityName, ",\\s*", ", ")) |> 
  mutate(my_city = case_when(my_city == "Los Angeles-Long Beach-Santa Ana" ~ "Los Angeles",
                             my_city == "Springfield, Massachusett, Connecticut" ~ "Springfield, MA",
                             my_city == "Columbia, South Carolina" ~ "Columbia, SC",
                             .default = my_city))

### Titles --------------------------------------------------------------------

# Plot titles
top20_title <- glue::glue(
  "Top 20 Cities)"
)

bottom20_title <- glue::glue(
  "Bottom 20 Cities"
)

# Patchwork titles
title_text <- "Best and Worst US Cities for Green Space"
subtitle_text <- "Some cities are nearly 40% green space. Some are as low as 1%."

# Social caption
social_caption <- create_social_caption(
  tt_year = 2026,
  tt_week = 38,
  source_text = "UN Habitat Urban Indicators Dataset"
)

### Theme ---------------------------------------------------------------------

# Theme applied to individual plots
my_theme <- theme_minimal(
  base_size = base_text_size,
  base_family = body_font
) +
  
  theme(
    plot.title = element_text(
      family = title_font,
      size = rel(1.1), 
      hjust = .5,
      face = "bold",
      color = title_color,
      lineheight = 0.35
    ),
    
    plot.title.position = "panel",
    
    plot.subtitle = ggtext::element_markdown(
      family = subtitle_font, 
      face = "bold",
      size = rel(1.0), 
      margin(b = 14), 
      lineheight = 1.2, 
      color = subtitle_color
    ),
    
    axis.text.x = element_text(
      family = body_font,
      size = rel(1),
      hjust = 0.5,
      face = "bold",
      color = body_color
    ),
    
    axis.text.y = element_text(
      family = body_font,
      size = rel(1.1),
      hjust = 0,
      face = "bold",
      color = body_color,
      lineheight = 0.35
    ),
    
    plot.caption = ggtext::element_markdown(
      family = body_font,
      size = rel(0.8),
      hjust = 0,
      color = caption_color
    ),
    
    panel.grid.major.x = element_line(linewidth = .4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.background = element_rect(fill = background_color)
  )

# Annotation theme for patchwork
annotation_theme <- theme(
  
  plot.background = element_rect(fill = background_color),
  
  plot.title = element_text(
    family = title_font,
    size = base_text_size * 1.65,
    face = "bold",
    color = title_color,
    margin = margin(b = 6)
  ),
  
  plot.subtitle = ggtext::element_markdown(
    family = subtitle_font,
    size = base_text_size * 1.15,
    face = "bold",
    color = subtitle_color,
    margin = margin(b = 18)
  ),
  
  plot.caption = ggtext::element_textbox_simple(
    family = body_font,
    size = base_text_size * 0.85,
    color = caption_color,
    lineheight = .45,
    
    # Align text within the box
    halign = 0,
    
    # Make the box occupy available caption width
    width = grid::unit(1, "npc"),
    
    # No visible box styling
    fill = NA,
    box.color = NA,
    padding = margin(0),
    margin = margin(t = 15)
  ),
  
  plot.margin = margin(
    t = 20,
    r = 40,
    b = 20,
    l = 20
  )
)

### Plot ----------------------------------------------------------------------

# Top 20
p <- 
ggplot(plot_data |> slice_tail(n = 20),
       aes(
         x = averageShareOfGreenAreaInCityUrbanAreaPct,
         y = reorder(my_city, averageShareOfGreenAreaInCityUrbanAreaPct),
       )) +
  geom_col(fill = "#4F7942") +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,5),
                     expand = c(0,0)) +
  my_theme +
  labs(title = top20_title,
       x = "Green Space Percent",
       y = NULL)

# Bottom 20
p2 <- 
ggplot(plot_data |> slice_head(n = 20),
       aes(
         x = averageShareOfGreenAreaInCityUrbanAreaPct,
         y = reorder(my_city, averageShareOfGreenAreaInCityUrbanAreaPct),
       )) +
  scale_x_continuous(limits = c(0,10),
                     breaks = seq(0,10,5),
                     expand = c(0,0)) +
  geom_col(fill = "#8B7D6B") +
  my_theme +
  labs(title = bottom20_title,
       x = "Green Space Percent",
       y = NULL)

### Patchwork -----------------------------------------------------------------

patch <- p + p2 + 
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = social_caption,
    theme = annotation_theme
  )

patch <- patch + 
  ggview::canvas(
    width = 9, 
    height = 7, 
    units = "in", 
    dpi = 320)

### Save -------------------------------------------------------------------------

ggview::save_ggplot(
  plot = patch,
  file = here::here("2026/2026.09.22/2026.09.22.png")
  
