# title: "Effect of weather on tourism"
# author: "Mark Deming"
# date: "07/28/2026"
# description: "A Tidy Tuesday analysis of weather impact on quarterly holiday and business travel in Australia."


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



### Data -------------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2026, week = 30)
tourism <- tuesdata$tourism
weather <- tuesdata$weather


### Data Preparation -------------------------------------------------------------

my_weather <- weather |> 
  mutate(
    quarter = case_when(
      month <= 3  ~ 1L,
      month <= 6  ~ 2L,
      month <= 9  ~ 3L,
      month <= 12 ~ 4L
    ),
    year_quarter = paste(
      year, quarter, sep = "-"
      )
    ) |> 
  group_by(year_quarter, ws_id) |> 
  summarize(
    avg_temp = mean(temp, na.rm = T),
    avg_hi = mean(max, na.rm = T),
    avg_lo = mean(min, na.rm = T),
    avg_rh = mean(rh, na.rm = T),
    avg_prcp = mean(prcp, na.rm = T),
    rainy_days = sum(rainy, na.rm = T),
    avg_wind_speed = mean(wind_speed, na.rm = T),
    avg_max_speed = mean(max_speed, na.rm = T),
    .groups = "drop")

my_tourism <- tourism |> 
  mutate(
    year_quarter = paste(year, quarter, sep = "-")
    ) |> 
  group_by(year_quarter, ws_id, purpose) |> 
  summarize(
    total_trips = sum(trips, na.rm = T), .groups = "drop"
    )

merged_dat <- my_weather |> 
  left_join(my_tourism, by = c("ws_id", "year_quarter")) |> 
  ungroup()


### Models -----------------------------------------------------------------------

holiday_dat <- merged_dat |> 
  filter(purpose == "Holiday",
         total_trips > 0)
  
business_dat <- merged_dat |> 
  filter(purpose == "Business",
         total_trips > 0)

mod_holiday <- fixest::feols(log(total_trips) ~
                             avg_temp +
                             avg_prcp +
                             rainy_days |
                             ws_id +
                             year_quarter,
                             data = holiday_dat,
                             vcov = ~ws_id)

# Warmer temps linked to more holiday trips. 
# Rainy days linked to fewer holiday trips.

mod_business <- fixest::feols(log(total_trips) ~
                              avg_temp +
                              avg_prcp +
                              rainy_days |
                              ws_id +
                              year_quarter,
                              data = business_dat,
                              vcov = ~ws_id)

# Coefficients are in expected direction for business trips.
# But coefficients do not reach statistical significance.

# Prepare model output for plotting
coef_labels <- c(
  avg_temp   = "Average\nTemperature (°C)",
  avg_prcp   = "Average\nPrecipitation (mm)",
  rainy_days = "Number of\nRainy Days"
)

holiday_coef <- broom::tidy(
  mod_holiday,
  conf.int = TRUE
) |>
  mutate(
    travel_type = "Holiday",
    coef_label = recode(term, !!!coef_labels)
  )

business_coef <- broom::tidy(
  mod_business,
  conf.int = TRUE
) |>
  mutate(
    travel_type = "Business",
    coef_label = recode(term, !!!coef_labels)
  )

# Combine coefficients
all_coef <- bind_rows(holiday_coef, business_coef)

x_limit <- max(
  abs(c(all_coef$conf.low, all_coef$conf.high)),
  na.rm = TRUE
)


### Interpretation ---------------------------------------------------------------

# Calculate percentages from model
holiday_effects <- holiday_coef |>
  mutate(percent_change = round(100 * (exp(estimate) - 1)), 2)

# Extract percentages
temp_effect <- holiday_effects |>
  filter(term == "avg_temp") |>
  pull(percent_change)

rain_effect <- holiday_effects |>
  filter(term == "rainy_days") |>
  pull(percent_change)

# Sample sizes
holiday_n <- nobs(mod_holiday)
business_n <- nobs(mod_business)


### Titles ---------------------------- ------------------------------------------

# Plot titles
holiday_title <- glue::glue(
  "Quarterly Holiday Trips\n(n = {holiday_n})"
)

business_title <- glue::glue(
  "Quarterly Business Trips\n(n = {business_n})"
)

# Patchwork titles
title_text <- "Effect of Weather on Domestic Travel in Australia"
subtitle_text <- "Holiday travel is much more weather-sensitive than business travel."

# Social caption
social_caption <- create_social_caption(
  tt_year = 2026,
  tt_week = 30,
  source_text = "ecotourism R package (occurrences, weather, and tourism data)"
)

# Caption
caption_text <- glue::glue(
  "**Interpretation.** A 1°C increase in average quarterly temperature is associated with approximately 
  {temp_effect}% more holiday trips, while each additional rainy day is associated with roughly {rain_effect}% fewer holiday 
  trips. Comparable effects are not evident for business travel. **Models.** Coefficients are estimated 
  using fixed-effects panel regressions with weather station and year-quarter fixed effects. Standard 
  errors are clustered at the weather-station level. Horizontal lines denote 95% confidence intervals. 
  The dependent variable is the natural log of quarterly domestic trips. **Note:** Differences in statistical
  significance between the two models may be due to differences in sample size.","<br><br>", social_caption
)


### Theme ------------------------------------------------------------------------

# Theme applied to individual plots
tt_theme <- theme_minimal(
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

# Plot ------------------------------------------------------------------------

holiday_plot <- ggplot(holiday_coef, 
                       aes(x = estimate,
                           y = reorder(coef_labels, estimate),
                           xmin = conf.low,
                           xmax = conf.high)
                       ) +
  geom_vline(
    xintercept = 0, 
    linewidth = 0.65,
    linetype = "dotted", 
    color = "#77454A"
    ) +
  geom_pointrange(
    size = 0.1,
    linewidth = 0.4
  ) +
  scale_x_continuous(
    limits = c(-x_limit, x_limit)
  ) +
  labs(
    title = holiday_title,
    x = NULL, 
    y = NULL
    ) +
  tt_theme

p2 <- ggplot(business_coef, 
            aes(x = estimate,
                y = reorder(coef_labels, estimate),
                xmin = conf.low,
                xmax = conf.high)
) +
  geom_vline(
    xintercept = 0, 
    linewidth = 0.65,
    linetype = "dotted", 
    color = "#77454A"
  ) +
  geom_pointrange(
    size = 0.1,
    linewidth = 0.4
  ) +
  scale_x_continuous(
    limits = c(-x_limit, x_limit)
  ) +
  labs(
    title = business_title,
    x = NULL, 
    y = NULL
  ) +
  tt_theme


# Patchwork -------------------------------------------------------------------

patch <- p + p2 + 
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = annotation_theme
  )

patch <- patch + 
  ggview::canvas(
    width = 11, 
    height = 7, 
    units = "in", 
    dpi = 320)

patch


### Save -------------------------------------------------------------------------

ggview::save_ggplot(
  plot = patch,
  file = here::here("2026/2026.07.28/2026.07.28.png")
)