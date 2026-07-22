# Packages --------------------------------------------------------------------
pacman::p_load(usethis,
               gitcreds,
               tidyverse, 
               ggthemes,
               ggrepel,
               glue,
               showtext,
               FactoMineR)

# Reusable caption function ---------------------------------------------------
viz_caption <- function(source,
                        date = format(Sys.Date(), "%B %Y"),
                        author = "Mark Deming",
                        website = "github.com/jmdeming",
                        note = NULL) {
  
  footer <- glue(
    "<span style='color:#777777;'>",
    "<b>Source:</b><span style='letter-spacing:2px;'> </span>{source}",
    "<span style='letter-spacing:6px;'> • </span>",
    "<b>Created:</b><span style='letter-spacing:2px;'> </span>{date}",
    "</span><br>",
    "<span style='color:#999999;'>",
    "<b>{author}</b>",
    " • ",      # Unicode EM SPACE around bullet
    "{website}",
    "</span>"
  )
  
  if (!is.null(note)) {
    footer <- paste0(
      footer,
      "<br><span style='font-size:8.5pt;color:#999999;'><i>",
      note,
      "</i></span>"
    )
  }
  
  footer
}

# Theming ---------------------------------------------------------------------
font_add_google("Raleway", 
                "Raleway",
                regular.wt = 400,
                bold.wt = 800)

title_font <- "Raleway"
subtitle_font <- "Raleway"
body_font <- "Raleway"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

set_theme(theme_bw(base_family = body_font, base_size = 10))
update_theme(
  text = element_text(color = "#333333"),
  plot.title = element_text(
    size = 14,
    face = "bold",
    family = title_font,
    margin = margin(t = 5, r = 0, b = 5, l = 0)
  ),
  plot.subtitle = element_text(
    size = 11,
    family = subtitle_font,
    margin = margin(t = 2, r = 0, b = 10, l = 0),
    lineheight = 1.3
  ),
  plot.caption = ggtext::element_markdown(
    size = rel(0.8),
    hjust = 1,
    lineheight = 1.15,
    margin = margin(t = 18)
  ),
  plot.title.position = "plot",
  plot.background = element_rect(fill = "#FAFAF8"),
  panel.background = element_rect(fill = "#FAFAF8"),
  panel.grid = element_blank(),
  legend.position = "none")


# Data ------------------------------------------------------------------------
#usethis::create_github_token()
#gitcreds::gitcreds_set()

tuesdata <- tidytuesdayR::tt_load(2026, week = 29)
nde_experiences <- tuesdata$nde_experiences


# Exploring co-occurrence -----------------------------------------------------
features <- nde_experiences |> 
  select(starts_with("ai_"))

assoc <- features |>
  cor(use = "pairwise.complete.obs")
assoc


# Multiple Components Analysis (MCA) --------------------------------------------

# Convert logical variables to categorical factors for MCA
features_mca <- features |>
  mutate(across(everything(), as.factor))

# Run multiple correspondence analysis
mca <- MCA(
  features_mca,
  graph = FALSE
)

# See how much variation each dimension explains
mca$eig

# Contribution of each TRUE/FALSE category to the dimensions
mca$var$contrib

# Plot the NDE categories on the first two MCA dimensions
fviz_mca_var(
  mca,
  repel = TRUE
)

# Extract MCA coordinates for each category
mca_coords <- as.data.frame(mca$var$coord) |>
  rownames_to_column("feature") |>
  filter(str_detect(feature, "TRUE"))              # keep presence of each feature only

# Inspect before plotting
mca_coords

# Create readable feature names
mca_coords <- mca_coords |>
  mutate(
    feature = recode(
      feature,
      ai_obe_TRUE = "Out-of-body",
      ai_unity_TRUE = "Unity",
      ai_hellish_TRUE = "Hellish",
      ai_clinical_TRUE = "Clinical death",
      ai_esp_TRUE = "ESP",
      ai_past_lives_TRUE = "Past lives",
      ai_world_future_TRUE = "World / future",
      ai_aliens_TRUE = "Aliens"
    )
  )


# Calculate prevalence of each NDE feature
prevalence <- features |>
  summarise(across(everything(), mean)) |>
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "prevalence"
  ) |>
  mutate(
    feature = recode(
      variable,
      ai_obe = "Out-of-body",
      ai_unity = "Unity",
      ai_hellish = "Hellish",
      ai_clinical = "Clinical death",
      ai_esp = "ESP",
      ai_past_lives = "Past lives",
      ai_world_future = "World / future",
      ai_aliens = "Aliens"
    )
  )

# Add prevalence to MCA coordinates
mca_coords <- mca_coords |>
  left_join(
    prevalence |> select(feature, prevalence),
    by = "feature"
  )

# MCA Scatterplot -------------------------------------------------------------

ggplot(
  mca_coords,
  aes(
    x = `Dim 1`,
    y = `Dim 2`
  )
) +
  
  # Light reference lines mark the MCA origin
  geom_hline(
    yintercept = 0,
    linewidth = 0.3,
    linetype = "dashed",
    color = "grey75"
  ) +
  
  geom_vline(
    xintercept = 0,
    linewidth = 0.3,
    linetype = "dashed",
    color = "grey75"
  ) +
  
  # Plot each NDE feature
  geom_point(
    aes(size = prevalence),
    color = "#C65D32"
  ) +
  
  scale_size_area(
    max_size = 12,
    labels = scales::percent,
    name = "Share of accounts"
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  
  # Add readable labels without overlap
  geom_text(
    aes(label = feature),
    size = 3.5,
    hjust = 0,                # label ends at its x position
    vjust = 0.5,              # vertically centered on point
    nudge_x = .2           # shift label left of point
  ) +
  
  # Preserve the geometry of the MCA solution
  coord_equal(clip = "off") +
  
#  coord_cartesian(clip = "off") +
  
  labs(
    title = "Which near-death experiences travel together?",
    subtitle = "Experiences closer together tend to appear in similar accounts. Out-of-body experiences\ntrack closely with ESP, while unity/oneness with the universe shares a surprisingly similar\npattern with hellish experiences.",
    x = "Dimension 1\n(17.4% of variation)",
    y = "Dimension 2\n(15.7% of variation)",
    caption = viz_caption(
      source = "Near Death Experience Research Foundation",
      note = NULL
      )
    ) +
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 10)) +
  ggview::canvas(width = 2000, height = 2000, units = "px") -> p

p

# Save ------------------------------------------------------------------------
ggview::save_ggplot(p, here::here("2026", "2026.07.21", "2026.07.21.png"))

