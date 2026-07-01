# Packages --------------------------------------------------------------------
remotes::install_github("ropensci/rnaturalearthhires")

pacman::p_load(tidyverse, 
               ggmap, 
               ggthemes,
               glue,
               showtext,
               sf,
               rnaturalearth,
               rnaturalearthdata)

# Reusable caption function ---------------------------------------------------

library(glue)

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
# 1. Tuesdata
tuesdata <- tidytuesdayR::tt_load(2026, week = 26)
wreck_inventory <- tuesdata$wreck_inventory

# 2. Stadia map
StadiaMaps_apikey <- "API key here"
register_stadiamaps(key = StadiaMaps_apikey)

# Use bboxfinder.com to determine coords
stadia_map_layer <- 
  ggmap(get_stadiamap(bbox = c(left = -15, 
                               bottom = 49, 
                               right = -5, 
                               top = 57),
                      zoom = 8,
                      maptype = "stamen_toner_background"))

# 3. Shapefile for masking
ireland_land <- ne_countries(
  scale = "large",
  country = c("Ireland", "United Kingdom"),
  returnclass = "sf"
) |> 
st_crop(
  xmin = -25,
  xmax = -5.2,
  ymin = 46,
  ymax = 57
)

# Plot 1: Density  ------------------------------------------------------------
# 1. Estimate density manually
wreck_no_nas <- wreck_inventory |> 
  filter(!is.na(longitude),
         !is.na(latitude))

dens <- MASS::kde2d(
  x = wreck_no_nas$longitude,
  y = wreck_no_nas$latitude,
  h = c(0.18, 0.18),
  n = 300,
  lims = c(-25, -5.2, 46, 57)
)

dens_df <- expand.grid(
  longitude = dens$x,
  latitude = dens$y
)

dens_df$density <- as.vector(dens$z)

# 2. Convert density grid to sf points
dens_sf <- st_as_sf(
  dens_df,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

# 3. Get land polygons
land <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>
  st_transform(4326)

# 4. Keep only density cells that are NOT on land
dens_ocean <- dens_sf |>
  filter(lengths(st_intersects(geometry, land)) == 0) |>
  st_drop_geometry()

dens_df$density <- as.vector(dens$z)

# 5. Plot
dens_breaks <- quantile(
  dens_ocean$density,
  probs = c(.87, .90, .94, .97, .99),
  na.rm = TRUE
)
dens_breaks <- c(dens_breaks, max(dens_ocean$density, na.rm = TRUE) * 1.01)

stadia_map_layer +
  geom_contour_filled(
    data = dens_ocean,
    aes(
      x = longitude,
      y = latitude,
      z = density
    ),
    breaks = dens_breaks,
    linewidth = 0.25,
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_d(option = "magma",
                       direction = 1,
                       name = "Shipwreck\nConcentration",
                       labels = c("Low", "", "", "", "High")) 

# This is probably best done using geom_density_2d_filled
# and then adding a geom_sf on top to mask density cloud
# portions that overlap landmass.

# Plot 2: Points --------------------------------------------------------------
stadia_map_layer +
  geom_point(
    data = wreck_inventory,
    aes(longitude, latitude),
    color = "white",
    size = 0.5,
    alpha = 0.3,
    inherit.aes = FALSE
  )

# Plot 3: Density cloud plus shapefile mask -----------------------------------
ggplot() +
  geom_density_2d_filled(
    data = wreck_inventory,
    aes(
      x = longitude,
      y = latitude,
      fill = after_stat(level)
    ),
    contour_var = "ndensity",
    h = c(.8, .8),
    breaks = seq(0.1, 1, .15),
    alpha = .6
  ) +
  geom_sf(
    data = ireland_land,
    fill = "gray95",
    color = "grey25",
    linewidth = .2
  ) +
  geom_point(
    data = wreck_inventory,
    aes(longitude, latitude),
    size = .5,
    alpha = .2
  ) +
  coord_sf(
    xlim = c(-20, -5.2),
    ylim = c(47, 57),
    expand = FALSE
  ) +
  scale_fill_viridis_d(option = "viridis",
                       direction = 1,
                       name = "Shipwreck\nconcentration",
                       labels = c("Low", "", "", "", "", "High")) +
  annotate(
    "text",
    x = -7.7,
    y = 53.3,
    label = "Ireland",
    #family = body_font,
    fontface = "bold",
    size = 5,
    color = "grey25"
  ) +
  labs(title = "Concentration of Irish shipwrecks",
       subtitle = "Recorded shipwrecks cluster along the island's western and southern coasts",
       caption = viz_caption(
         source = "Wreck Inventory of Ireland",
         note = NULL
       ),
       x = NULL,
       y = NULL) + 
  ggview::canvas(width = 2000, height = 2000, units = "px") -> p

p

# Save ------------------------------------------------------------------------
ggview::save_ggplot(p, here::here("2026.06.30/2026.06.30.png"))
