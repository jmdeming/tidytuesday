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

tuesdata <- tidytuesdayR::tt_load(2026, week = 32)
palomar_emission_lines <- tuesdata$palomar_emission_lines
palomar_survey <- tuesdata$palomar_survey


### Data preparation -------------------------------------------------------------

# Recode galaxy morphology
palomar_survey <- palomar_survey |>
  mutate(
    hubble_clean =
      hubble_type |>
      str_remove("\\?.*") |>
      str_remove("\\(.*\\)") |>
      str_remove("pec") |>
      str_remove("tides") |>
      str_squish()
  )

palomar_survey <- palomar_survey |>
  mutate(
    morphology = case_when(
      # Special cases
      str_detect(hubble_type, "^\\(R\\)SB") ~ "Barred spiral",
      str_detect(hubble_type, "^IB") ~ "Irregular",
      str_detect(hubble_type, "^\\[S0") ~ "Lenticular",
      str_detect(hubble_type, "^dS0") ~ "Lenticular",
      
      # Ellipticals
      str_detect(hubble_type, "^d?E") ~ "Elliptical",
      
      # Lenticulars (including barred lenticulars)
      str_detect(hubble_type, "^SB0|^S0|^RS0") ~ "Lenticular",
      
      # Barred spirals
      str_detect(hubble_type, "^SB|^RSB") ~ "Barred spiral",
      
      # Unbarred spirals
      str_detect(hubble_type, "^S|^RS") ~ "Spiral",
      
      # Irregulars
      str_detect(hubble_type, "Im|Sm|Irr") ~ "Irregular",
      
      TRUE ~ "Other"
    ),
    
    morphology = factor(
      morphology,
      levels = c(
        "Elliptical",
        "Lenticular",
        "Spiral",
        "Barred spiral",
        "Irregular",
        "Other"
      )
    )
  )

# Filter out Irregular and Other
palomar_survey <- palomar_survey |> 
  filter(!morphology %in% c("Irregular", "Other"))

# Recode nuclear activity
palomar_survey <- palomar_survey |>
  mutate(
    activity_type = na_if(activity_type, "Absorption")
  )

analysis_dat <- palomar_survey |>
  filter(!is.na(activity_type),
         !is.na(morphology))

plot_dat <- analysis_dat |>
  count(morphology, activity_type) |>
  group_by(morphology) |>
  mutate(prop = n / sum(n),
         activity_type = factor(
  activity_type,
  levels = c(
    "LINER",
    "Seyfert",
    "Transition",
    "H II"
  )
))

facet_labels <- analysis_dat |>
  count(morphology) |>
  mutate(
    label = paste0(
      morphology,
      "\nn = ", n
    )
  ) |>
  select(morphology, label) |>
  tibble::deframe()


### Titles ---------------------------- ------------------------------------------

# Plot titles
plot_title <- glue::glue(
  "Nuclear activity differs by galaxy morphology"
)

plot_subtitle <- glue::glue(
  "Distribution of nuclear activity within each morphological class"
)

# Social caption
social_caption <- create_social_caption(
  tt_year = 2026,
  tt_week = 32,
  source_text = "Palomar spectroscopic survey of nearby galaxies"
)

# Caption
caption_text <- glue::glue(
  "**Note:** Bars show the percentage of galaxies within each broad morphological class 
  classified as H II, Transition, Seyfert, or LINER nuclei. H II nuclei are powered primarily 
  by star formation, whereas Seyfert and LINER nuclei are forms of active galactic nuclei (AGN). 
  Transition nuclei exhibit characteristics of both. Morphological classifications are collapsed 
  from the original Hubble types.","<br><br>", social_caption
)


### Theme ------------------------------------------------------------------------

my_theme <- theme_minimal(
  base_size = base_text_size,
  base_family = body_font
) +
  theme(plot.background = element_rect(background_color),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(family = body_font,
                                  face = "bold",
                                  color = body_color,
                                  lineheight = 0.35,
                                  margin = margin(b = 5),
                                  size = base_text_size * 0.9),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.4, color = "white"),
        panel.grid.minor.x = element_blank(),
        panel.spacing.x = unit(28, "pt"), 
        panel.spacing.y = unit(24, "pt"),
        
        panel.background = element_rect(
          fill = "#FAF9F6",
          color = NA
        ),
        
        axis.ticks.y = element_blank(),
        axis.ticks.length.y.left = unit(-8, "pt"),
        
        axis.title.x = element_text(
          family = body_font,
          color = body_color,
          margin = margin(t = 10, b = 10)
        ),
        
        axis.text.x = element_text(
          family = body_font,
          color = body_color,
          margin = margin(t = 0),
          vjust = 5
        ),
        
        axis.text.y = element_text(family = body_font, 
                                   color = body_color,
                                   hjust = 1,
                                   margin = margin(r = 0)),
  
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


p <- ggplot(
  plot_dat,
  aes(
    x = prop,
    y = activity_type
  )
) +
  geom_segment(
    aes(
      x = 0,
      xend = prop,
      yend = activity_type
    ),
    linewidth = 0.8,
    color = "grey75"
  ) +
  geom_col(fill = "#44546A") +
  geom_text(
    aes(
      label = scales::percent(prop, accuracy = 1)
    ),
    hjust = -0.2,
    size =7.5
  ) +
  facet_wrap(
    ~ morphology,
    ncol = 2,
    labeller = labeller(morphology = facet_labels)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.2))
  ) +
  scale_y_discrete(
    expand = expansion(add = 0)
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = caption_text,
    x = "Share of galaxies",
    y = NULL
  ) +
  my_theme

p <- p + 
  ggview::canvas(
    width = 10, 
    height = 8, 
    units = "in", 
    dpi = 320)

p

### Save ----------------------------------------------------------------------

ggview::save_ggplot(
  plot = p,
  file = here::here("2026/week32/week32.png")
)


