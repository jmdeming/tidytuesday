# Mark Deming
# Date created: 6/14/2026
# Date modified: 6/16/2026

# Packages --------------------------------------------------------------------
library(tidyverse)
library(stringr) 
library(showtext)

# Theme -----------------------------------------------------------------------
set_theme(theme_bw()) 
update_theme(text = element_text(family = "Raleway", size = 10),
             panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             plot.title.position = "plot")

# Data ------------------------------------------------------------------------
# gitcreds::gitcreds_set()
tt <- tidytuesdayR::tt_load('2026-06-09')
films <- tt$game_films

films <- films |> 
    filter_out(str_detect(title, "Untiled") | is.na(worldwide_box_office)) |> 
    mutate(
      
      # Grab franchise from title
      franchise = str_extract(title, "^[^:]+"), 
      franchise = str_remove(franchise, "\\d"),
      franchise = str_trim(franchise),
      
      # Manually edit some franchises
      franchise = case_when(
        str_detect(franchise, "Pokémon") ~ "Pokémon",
        str_detect(franchise, "Persona") ~ "Persona",
        str_detect(franchise, "Mario") ~ "Super Mario",
        str_detect(franchise, "Kombat") ~ "Mortal Kombat",
        str_detect(franchise, "Fighter") ~ "Street Fighter",
        str_detect(franchise, "Silent") ~ "Silent Hill",
        str_detect(franchise, "Lara Croft") ~ "Tomb Raider",
        str_detect(franchise, "Minecraft") ~ "Minecraft",
        str_detect(franchise, "Angry Birds") ~ "Angry Birds",
        str_detect(franchise, "Freddy's") ~ "FNAF",
        TRUE ~ franchise) 
    )
  
franchise_dat <- films |> 
  group_by(franchise) |> 
  summarize(n = n(),
            total_box_office = sum(worldwide_box_office, na.rm = T),
            avg_tomatoes = mean(rotten_tomatoes, na.rm = T),
            my_franchise_labels = ifelse(n > 1, paste(franchise, " (", n, ")", sep = ""), NA))


# Plot ------------------------------------------------------------------------
franchise_dat |> 
  ggplot(aes(x = avg_tomatoes, 
             y = total_box_office)) +
  geom_point(aes(size = n),
             alpha = .6,
             color = "steelblue2",
             show.legend = F) +
  ggrepel::geom_text_repel(aes(label = my_franchise_labels),
                           point.size = franchise_dat$n,
                           segment.color = "transparent",
                           point.padding = 5,
                           direction = "x",
                           hjust = 1,
                           size = 2.7,
                           family = "Raleway",
                         fontface = "bold") +
  scale_x_continuous(limits = c(0, 100),
                     breaks = seq(0, 100,  25),
                     labels = scales::label_percent(scale = 1)) +
  scale_y_log10(labels = scales::label_dollar(),
                expand = expansion(mult = c(0.05, 0.1))) +
  scale_size_continuous(range = c(3, 20)) +
  labs(title = "Movie Franchises Based on Video Games",
       subtitle = "Rotten Tomatoes Scores and Total Worldwide Box Office\n",
       caption = "\nMark Deming | TidyTuesday | June 9th, 2026\nNote: Number of films in franchise denoted by point size. Also shown in parentheses.",
       x = "\nAverage Rotten Tomatoes Score",
       y = "Total Worldwide Box Office\n") +
  ggview::canvas(width = 7.5, height = 7) -> fig

# Save ------------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026.06.09", "2026.06.09.png"))
                        