# Packages --------------------------------------------------------------------
library(tidyverse)
library(showtext)

# Theme -----------------------------------------------------------------------
font_add_google("Libre Caslon Text")
font_add_google("Baskervville")
font_add_google("Parisienne")

title_font = "Libre Caslon Text"
body_font = "Parisienne"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

set_theme(theme_minimal(base_family = body_font, base_size = 14))
update_theme(
  text = element_text(color = "#3C3A4D"),
  plot.title = ggtext::element_textbox_simple(
    size = rel(1.1),
    face = "bold",
    family = title_font,
    margin = margin(t = 5, r = 0, b = 5, l = 0),
    lineheight = 1
    ),
  plot.subtitle = ggtext::element_textbox_simple(
    size = rel(0.6),
    family = title_font,
    margin = margin(t = 2, r = 0, b = 10, l = 0),
    lineheight = 1.3
    ),
  plot.caption = ggtext::element_textbox_simple(
    size = rel(0.5),
    family = title_font,
    hjust = 0,
    margin = margin(t = 20, r = 0, b = 0, l = 0),
    lineheight = 1.2
  ),
  plot.title.position = "plot",
  plot.background = element_rect(fill = "#EAE2D9"),
  panel.background = element_rect(fill = "#EAE2D9"),
  panel.grid = element_blank(),
  legend.position = "none")

# Data ------------------------------------------------------------------------
tt <- tidytuesdayR::tt_load(2026, week = 24)
eng_names <- tt$england_wales_names
ni_names <- tt$ni_names
sct_names <- tt$scotland_names

# Add region
eng_names <- eng_names |> 
  mutate(Region = "England and Wales")

ni_names <- ni_names |> 
  mutate(Region = "Northern Ireland")

sct_names <- sct_names |> 
  mutate(Region = "Scotland")

# Determine date range
summary(eng_names$Year)
summary(ni_names$Year)
summary(sct_names$Year)

# Append
my_dat <- data.frame(rbind(eng_names, ni_names, sct_names))

# Summarize
summarized_dat <- my_dat |> 
  filter(Year >= 1997 & Year <= 2023) |> 
  group_by(Year, Sex, Name) |> 
  summarize(Number = sum(Number, na.rm = TRUE), .groups = "drop_last") |> 
  arrange(Year, Sex, -Number) |> 
  group_by(Year, Sex) |> 
  mutate(Rank = min_rank(-Number))

# Filter
da_dat <- summarized_dat |> 
  filter(Sex == "Girl" &
          Name %in% c("Edith", "Cora", "Sybil", "Mary", "Violet", "Marigold")) 

# Dotplot ---------------------------------------------------------------------
fig <- 
  ggplot(data = da_dat,
         aes(x = Year, y = Name, size = Number)) +
  geom_point(color = "#3E2C42", alpha = 0.7) +
  geom_text(data = subset(da_dat, Year == 2010 | Year == 2021),
            aes(x = Year,
                y = Name,
                label = scales::comma(Rank)),
            size = rel(2.5),
            color = "#3C3A4D",
            vjust = -2.5,
            family = title_font,
            inherit.aes = FALSE) +
  scale_size_continuous(range = c(1,8)) +
  labs(title = "The 'Downton Bump'",
       subtitle = "The hit historical drama *Downton Abbey* prompted a massive revival of vintage Edwardian and Victorian baby girl names across England, Wales, Scotland, and Ireland following its 2010 premiere. The most dramatic impact was seen in names tied to prominent, beloved, or tragic characters that had completely fallen out of modern favor prior to 2010. Sybil saw the largest rise, climbing from 4,750 to 1,415 in the rankings between 2010 and 2021.",
       caption = "Mark Deming | TidyTuesday | June 16th, 2026 <br> Labels denote ranking in 2010 and 2021. Circle size denotes the number of babies with each name.",
       x = NULL,
       y = NULL,
       size = "Number of babies") +
  ggview::canvas(width = 1600, height = 2200, units = "px") -> fig

fig

# Save ------------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026.06.16/2026.06.16.png"))