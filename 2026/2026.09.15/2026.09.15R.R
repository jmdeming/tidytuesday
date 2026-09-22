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

tuesdata <- tidytuesdayR::tt_load(2026, week = 37)
scrolls <- tuesdata$dead_sea_scrolls


### Titles --------------------------------------------------------------------

plot_title <- "Most Copied Dead Sea Scriptures"

plot_subtitle <- glue::glue(
  "Many non-canonical scriptures were copied, demonstrating <br> their importance to the Qumran community"
)


# Social caption
social_caption <- create_social_caption(
  tt_year = 2026,
  tt_week = 36,
  source_text = "The Dead Sea Scrolls"
)


# Full caption
plot_caption <- glue::glue(
  "**Note:** The plot depicts scriptures with at least 10 copies.",
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
      margin = margin(b = 18),
      lineheight = .4
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
      r = 60,
      b = 20,
      l = 20
    )
  )


### Plot ----------------------------------------------------------------------

p <- 
scrolls |> 
  group_by(biblical_book, canon_status) |> 
  summarize(
            n = n()) |> 
  filter(!is.na(biblical_book),
         n >= 10) |>  
  ggplot() +
  geom_point(aes(x = n,
               y = reorder(biblical_book, n),
               color = canon_status),
             alpha = .7) +
  scale_color_manual(values = c("#003f5c", "#008c54"),
                     labels = c("Not Canonized", "Canonized"),
                     name = NULL) +
  labs(title = plot_title,
       subtitle = plot_subtitle,
       caption = plot_caption,
       x = "Number of copies",
       y = NULL) +
  my_theme +
  theme(legend.position = "top",
        legend.title = NULL,
        legend.text = element_text(size = base_text_size * 1.5)
        )

p <- p +
  ggview::canvas(
    width = 7,
    height = 9,
    units = "in",
    dpi = 300
  )

p

### Save ----------------------------------------------------------------------

ggview::save_ggplot(
  plot = p,
  file = here::here(
    "2026/2026.09.15/2026.09.15.png"
  )
)

