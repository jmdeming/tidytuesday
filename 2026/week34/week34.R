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
title_font    <- "roboto"
subtitle_font <- "roboto"
body_font     <- "roboto"

base_text_size <- 18

# Colors
title_color      <- "black"
subtitle_color   <- "#4A4340"
body_color       <- "black"
caption_color    <- "#4A4340"
background_color <- "#F4F3EE"


### Data ----------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(
  year = 2026,
  week = 34
)

country_lyrics <- tuesdata$country_lyrics


### Data preparation ----------------------------------------------------------

# Tokenize lyrics and remove stop words
lyrics <- country_lyrics |>
  select(
    song,
    artist,
    entered_top_30_in,
    lyrics
  ) |>
  unnest_tokens(
    output = word,
    input = lyrics
  ) |>
  anti_join(
    stop_words,
    by = "word"
  ) |>
  filter(
    str_detect(word, "^[a-z]+$")
  )


# Number of songs represented in each year
songs_by_year <- lyrics |>
  distinct(
    entered_top_30_in,
    artist,
    song
  ) |>
  count(
    entered_top_30_in,
    name = "total_songs"
  )


# Count each word at most once per song
# and calculate annual prevalence
word_by_year <- lyrics |>
  distinct(
    entered_top_30_in,
    artist,
    song,
    word
  ) |>
  count(
    entered_top_30_in,
    word,
    name = "n_songs"
  ) |>
  complete(
    word,
    entered_top_30_in = 2013:2019,
    fill = list(n_songs = 0)
  ) |>
  left_join(
    songs_by_year,
    by = "entered_top_30_in"
  ) |>
  mutate(
    prop_songs = n_songs / total_songs
  )


# Estimate change in prevalence over time
word_trends <- word_by_year |>
  group_by(word) |>
  summarise(
    slope = coef(
      lm(prop_songs ~ entered_top_30_in)
    )[2],
    mean_prop = mean(prop_songs),
    total_songs = sum(n_songs),
    .groups = "drop"
  ) |>
  filter(
    total_songs >= 20
  )


### Titles --------------------------------------------------------------------

plot_title <- "The Changing Lyrics of Country Music"

plot_subtitle <- glue::glue(
  "Once-ubiquitous words like *baby* and *night* became less common from ",
  "2013 to 2019, while *yeah* became even more ubiquitous."
)


# Social caption
social_caption <- create_social_caption(
  tt_year = 2026,
  tt_week = 34,
  source_text = "Billboard Country Airplay"
)


# Full caption
caption_text <- glue::glue(
  "**Note:** Each word is counted at most once per song. ",
  "Average annual change is estimated from the share of songs containing each word ",
  "from 2013–2019. Only words appearing in at least 20 songs are shown.",
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
    panel.grid.major.y = element_line(
      linewidth = 0.4,
      color = "white"
    ),
    
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

p <- ggplot(
  word_trends,
  aes(
    x = mean_prop,
    y = slope,
    label = word,
    size = mean_prop,
    alpha = mean_prop
  )
) +
  
  # Zero-change reference line
  geom_hline(
    yintercept = 0,
    linewidth = 0.25,
    linetype = "dashed",
    color = body_color
  ) +
  
  # Words
  geom_text(
    family = body_font
  ) +
  
  # Word size represents average prevalence
  scale_size_continuous(
    range = c(7, 10.5)
  ) +
  
  # Alpha also represents average prevalence
  scale_alpha_continuous(
    range = c(0.60, 0.95)
  ) +
  
  # Axes
  scale_x_continuous(
    labels = scales::label_percent(
      accuracy = 1
    )
  ) +
  
  scale_y_continuous(
    labels = scales::label_percent(
      accuracy = 1
    )
  ) +
  
  labs(
    x = "Average share of songs containing word",
    y = "Average annual change in share of songs",
    title = plot_title,
    subtitle = plot_subtitle,
    caption = caption_text
  ) +
  
  # Suppress legends
  guides(
    size = "none",
    alpha = "none"
  ) +
  
  my_theme


### Canvas --------------------------------------------------------------------

p <- p +
  ggview::canvas(
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
  )

p


### Save ----------------------------------------------------------------------

ggview::save_ggplot(
  plot = p,
  file = here::here(
    "2026/week34/week34.png"
  )
)
