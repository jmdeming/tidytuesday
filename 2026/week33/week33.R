### Setup -- ---------------------------------------------------------------------

# Packages
pacman::p_load(tidyverse,
               showtext,
               sysfonts,
               fixest,
               broom,
               patchwork,
               fontawesome)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R")) # TT caption

# Fonts
setup_fonts()

title_font    <- "atkinson"
subtitle_font <- "atkinson"
body_font     <- "atkinson"

base_text_size <- 12

# Color palette
title_color    <- "black"
subtitle_color <- "#4A4340"
body_color     <- "black"
caption_color  <- "#4A4340"

background_color <- "#F4F3EE"


### Data -------------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2026, week = 33)

demo_by_first_language <- tuesdata$demo_by_first_language
#demo_by_nationality <- tuesdata$demo_by_nationality
#demo_by_reasons <- tuesdata$demo_by_reasons
#performance_by_first_language <- tuesdata$performance_by_first_language
#performance_by_nationality <- tuesdata$performance_by_nationality


### Data preparation -------------------------------------------------------------

demo_by_first_language <- demo_by_first_language |>
  mutate(
    band_num = case_when(
      band == "<4" ~ 3.5,
      TRUE ~ as.numeric(band)
    )
  )

ielts_mean <- demo_by_first_language |>
  group_by(language, year) |>
  summarize(
    annual_mean = weighted.mean(band_num, percent, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(language) |>
  mutate(
    mean_score = mean(annual_mean, na.rm = TRUE),
    is_english = factor(ifelse(language == "English", 1, 0))
  ) |>
  ungroup()

ielts_plot <- ielts_mean |>
  group_by(language) |>
  mutate(
    min_score = min(annual_mean),
    max_score = max(annual_mean)
  ) |>
  ungroup()


### Titles ---------------------------- ------------------------------------------

# Plot titles
plot_title <- glue::glue(
  "English Speakers Are Third-Best on English-Language Tests"
)

plot_subtitle <- glue::glue(
  "German and Greek speakers outperform English speakers on the IELTS exam"
)

# Social caption
social_caption <- create_social_caption(
  tt_year = 2026,
  tt_week = 33,
  source_text = "IELTS research website"
)

# Caption
caption_text <- glue::glue(
  "**Note:** Dots show the weighted average test score for 2022-2024. Lines show the range of scores for 2022-2024.","<br><br>", social_caption
)


### Theme ------------------------------------------------------------------------

my_theme <- theme_minimal(
  base_size = base_text_size,
  base_family = body_font
) +
  theme(plot.background = element_rect(background_color),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.4, color = "white"),
        panel.grid.minor = element_blank(),
        
        axis.title.x = element_text(
          family = body_font,
          color = body_color,
          margin = margin(t = 10, b = 10),
          size = base_text_size * 0.4
        ),
        
        axis.text.x = element_text(
          family = body_font,
          color = body_color,
          margin = margin(t = 0),
          vjust = 5,
          size = base_text_size * .8
        ),
        
        axis.text.y = element_text(family = body_font, 
                                   color = body_color,
                                   hjust = 0,
                                   margin = margin(r = 0),
                                   size = base_text_size * .8
        ),
  
        plot.title = element_text(
          family = title_font,
          size = base_text_size * 1.2,
          face = "bold",
          color = title_color,
          margin = margin(b = 6)
        ),
        
        plot.subtitle = ggtext::element_markdown(
          family = subtitle_font,
          size = base_text_size * 1.1,
          face = "bold",
          color = subtitle_color,
          margin = margin(b = 18)
        ),
        
        plot.caption = ggtext::element_textbox_simple(
          family = body_font,
          size = base_text_size * 0.8,
          color = caption_color,
          lineheight = .5,
          
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

p <-
ggplot(
  ielts_plot,
  aes(
    y = fct_reorder(language, mean_score),
    color = is_english
  )
) +
  geom_segment(
    aes(
      x = min_score,
      xend = max_score,
      y = fct_reorder(language, mean_score)
    ),
    linewidth = 0.6
  ) +
  #  geom_point(
  #    aes(x = annual_mean),
  #    size = 1.5,
  #    alpha = 0.35
  #  ) +
  geom_point(
    aes(x = mean_score),
    size = 2.5
  ) +
  scale_color_manual(values = c("#B8B8B8",
                                "#007C83"),
                     guide = "none") +
  labs(x = "Mean IELTS score",
       y = NULL,
       title = plot_title,
       subtitle = plot_subtitle,
       caption = caption_text) +
  my_theme

p <- p + 
  ggview::canvas(
    width = 8, 
    height = 12, 
    units = "in", 
    dpi = 300)

p

### Save ----------------------------------------------------------------------

ggview::save_ggplot(
  plot = p,
  file = here::here("2026/week32/week32.png")
)