### Setup ---------------------------------------------------------------------

# Packages
pacman::p_load(tidyverse,
               showtext,
               sysfonts,
               fontawesome)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R")) # TT caption

# Fonts
setup_fonts()

title_font    <- "Roboto"
subtitle_font <- "Roboto"
body_font     <- "Roboto"

base_text_size <- 28

# Color palette
title_color    <- "black"
subtitle_color <- "#4A4340"
body_color     <- "black"
caption_color  <- "#4A4340"

background_color <- "#F4F3EE"


### Data ----------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2026-08-04')
basotho_wool <- tuesdata$basotho_wool

# Cleaning
basotho_wool <- basotho_wool |> 
  filter(cmd_code != 5103)

# Annual dat
annual_dat <- basotho_wool |> 
  group_by(ref_year, reporter_desc) |> 
  summarize_if(is.numeric, sum, na.rm = TRUE)

# Seasonal data
seasonal_dat <- basotho_wool |> 
  mutate(season = case_when(ref_month %in% 6:8        ~ "Win",
                            ref_month %in% 9:11       ~ "Spr",
                            ref_month %in% c(12, 1:2) ~ "Sum",
                            ref_month %in% 3:5        ~ "Aut"),
         year_season = paste0(ref_year, "-", season)) |> 
  group_by(year_season, season) |> 
  summarize_if(is.numeric, sum, na.rm = T) |> 
  
  # Add detrend
  ungroup() |> 
  mutate(detrended = pracma::detrend(qty))

# Mean seasonal data
seasonal_avg_dat <- seasonal_dat |> 
  group_by(season) |> 
  summarize(
    mean = mean(detrended, na.rm = T),
    sd = sd(detrended, na.rm = T),
    n = sum(!is.na(detrended)),
    se = sd / sqrt(n),
    t = qt(.975, df = n - 1),
    lower = mean - t * se,
    upper = mean + t * se,
    .groups = "drop"
  )

# Mean monthly data
monthly_avg_dat <- basotho_wool |> 
  group_by(ref_year, ref_month) |> 
  summarize_if(is.numeric, sum, na.rm = T) |> 
  ungroup() |> 
  mutate(detrended = pracma::detrend(qty)) |> 
  group_by(ref_month) |> 
  summarize(
    mean = mean(detrended, na.rm = T),
    sd = sd(detrended, na.rm = T),
    n = sum(!is.na(detrended)),
    se = sd / sqrt(n),
    t = qt(.975, df = n - 1),
    lower = mean - t * se,
    upper = mean + t * se,
    .groups = "drop"
  )

# Add seasons to monthly data
monthly_avg_dat <- monthly_avg_dat |>
  mutate(
    season = case_when(
      ref_month %in% 3:5        ~ "Autumn",
      ref_month %in% 6:8        ~ "Winter",
      ref_month %in% 9:11       ~ "Spring",
      ref_month %in% c(12, 1:2) ~ "Summer"
    ),
    month_order = match(
      ref_month,
      c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2)
    ),
    month_label = factor(
      month.abb[ref_month],
      levels = month.abb[c(3:12, 1:2)]
    )
  )

### Theme ---------------------------------------------------------------------

my_theme <- theme_minimal(
  base_size = base_text_size,
  base_family = body_font
  ) +
  theme(plot.background = element_rect(background_color),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(family = body_font,
                                  face = "bold",
                                  color = body_color),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "white"),
        panel.grid.minor.y = element_line(linewidth = 0.5, color = "white"),
        panel.spacing.x = unit(0, "pt"),
        
        axis.text = element_text(family = body_font, 
                                 color = body_color),
        
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

# Titles ----------------------------------------------------------------------

# Plot titles
plot_title <- glue::glue(
  "Seasonal Patterns in Lesotho Wool Exports"
)

plot_subtitle <- glue::glue(
  "Average monthly deviations from the long-run trend reveal a pronounced spring peak and winter lull in wool imports."
)

social_caption <- create_social_caption(
    tt_year = 2026,
    tt_week = 31,
    source_text = "comtradr R package (UN Comtrade Database)"
  )

caption_text <- glue::glue("Points show the average deviation from the long-run 
trend in monthly wool imports; positive values indicate above-trend imports, negative 
values below trend.", "<br>", social_caption
)

### Plot ----------------------------------------------------------------------

p <- monthly_avg_dat |>
  mutate(
    season = factor(
      season,
      levels = c("Autumn", "Winter", "Spring", "Summer")
    )
  ) |>
  ggplot(aes(x = month_label, y = mean)) +
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    linewidth = 0.6,
    alpha = 0.5
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.1,
    linewidth = 0.5,
    color = "#3E4A59"
  ) +
  geom_point(size = .85,
             color = "#5C6F7B") +
  scale_y_continuous(breaks = seq(-0.5e6, 1.0e6, by = 0.25e6),
    labels = scales::label_number(scale = 1e-6,
                                  suffix = "M",
                                  accuracy = 0.1)
  ) +
  facet_grid(
    ~ season,
    scales = "free_x",
    space = "free_x",
    switch = "x"
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = caption_text,
    x = NULL,
    y = "Mean detrended wool imports (kg)"
  ) +
  my_theme

p <- p + 
  ggview::canvas(
    width = 11, 
    height = 7, 
    units = "in", 
    dpi = 320)

### Save ----------------------------------------------------------------------

ggview::save_ggplot(
  plot = p,
  file = here::here("2026/2026.08.04/2026.08.04.png")
)
