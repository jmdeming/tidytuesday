# Packages --------------------------------------------------------------------
pacman::p_load(tidyverse,
               tidytext,
               ggwordcloud,
               stopwords,
               showtext)

# Theming ---------------------------------------------------------------------


# Theme -----------------------------------------------------------------------
font_add_google("Raleway")

title_font <- "Raleway"
subtitle_font <- "Raleway"
body_font <- "Raleway"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

set_theme(theme_minimal(base_family = body_font, base_size = 10))
update_theme(
  text = element_text(color = "#333333"),
  plot.title = ggtext::element_textbox_simple(
    size = rel(1.1),
    face = "bold",
    family = title_font,
    margin = margin(t = 5, r = 0, b = 5, l = 0),
    lineheight = 1
  ),
  plot.subtitle = ggtext::element_textbox_simple(
    size = rel(0.8),
    family = title_font,
    margin = margin(t = 2, r = 0, b = 10, l = 0),
    lineheight = 1.3
  ),
  plot.caption = ggtext::element_textbox_simple(
    size = rel(0.5),
    family = title_font,
    hjust = 0,
    halign = 1,
    margin = margin(t = 20, r = 0, b = 0, l = 0),
    lineheight = 1.2
  ),
  plot.title.position = "plot",
  plot.background = element_rect(fill = "#FAFAF8"),
  panel.background = element_rect(fill = "#FAFAF8"),
  panel.grid = element_blank(),
  legend.position = "none")

# Data ------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2026, week = 25)

encyclicals <- tuesdata$encyclicals
papal_encyclicals <- tuesdata$papal_encyclicals
scripture_references <- tuesdata$scripture_references

# Comparative text analysis ---------------------------------------------------

# Common words in each encyclical
words <- encyclicals |>
  select(encyclical, pope, year, text) |>
  unnest_tokens(
    output = word,
    input = text
  ) |>
  anti_join(stop_words, by = "word") |> 
  filter(str_detect(word, "^[a-z]+$"))

word_counts <- words |>
  count(encyclical, word, sort = TRUE)

word_counts_wide <- word_counts |>
  pivot_wider(
    names_from = encyclical,
    values_from = n,
    values_fill = 0
  )

# Words common to both
common_words <- word_counts_wide |>
  filter(if_all(where(is.numeric), ~ .x > 0))

# Plot: Distinctive words -----------------------------------------------------
distinctive_words <- word_counts |>
  bind_tf_idf(word, encyclical, n) |>
  arrange(desc(tf_idf))

distinctive_words |>
  group_by(encyclical) |>
  slice_max(tf_idf, n = 20) |>
  ungroup() |>
  ggplot(aes(x = reorder_within(word, tf_idf, encyclical),
             y = tf_idf)) +
  geom_col() +
  facet_wrap(~ encyclical, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = "TF-IDF")

# Plot: Common phrases -- Bigrams ---------------------------------------------
bigrams <- encyclicals |>
  unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
  separate(bigram, c("word1", "word2"), sep = " ") |>
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word,
    str_detect(word1, "^[a-z]+$"),
    str_detect(word2, "^[a-z]+$")
  ) |>
  unite(bigram, word1, word2, sep = " ")

bigram_counts <- bigrams |>
  count(encyclical, bigram, sort = TRUE)

common_bigrams <- bigram_counts |>
  pivot_wider(
    names_from = encyclical,
    values_from = n,
    values_fill = 0
  ) |>
  filter(if_all(where(is.numeric), ~ .x > 0))

# Barplot
common_bigrams |> 
  mutate(total = `Magnifica Humanitas` + `Rerum Novarum`) |> 
  slice_max(total, n = 20) |> 
  ggplot(aes(x = reorder(bigram, total),
             y = total)) +
  geom_col() +
  coord_flip() +
  theme_minimal()

# Scatterplot
common_bigrams |>
  ggplot(aes(x = sqrt(`Magnifica Humanitas`),
             y = sqrt(`Rerum Novarum`),
             label = str_to_title(bigram))) +
  geom_abline(linetype = "dotted", color = "gray40") +
  ggrepel::geom_text_repel(
    size = 1.8,
    force = 1,
    max.overlaps = 22,
    box.padding = .6,
    point.padding = 0,
    segment.color = NA,
    family = body_font
  ) +
  scale_x_continuous(labels = scales::label_number()) +
  scale_y_continuous(breaks = seq(1, 3, 1),
                     labels = scales::label_number()) +
  labs(title = "Common phrases in landmark papal encyclicals",
       subtitle = "In papal encyclicals on the impacts of the industrial and AI revolutions, respectively, Popes Leo XII and Leo XIV emphasized common themes, including *private property*, *civil society* and references to Thomas Aquinas.",
       caption = "Mark Deming | TidyTuesday | June 23rd, 2026",
       x = "\nMentions in Magnifica Humanitas (2026)",
       y = "Mentions in Rerum Novarum (1891)\n") +
  ggview::canvas(width = 1600, height = 1600, units = "px") -> fig

# Save ------------------------------------------------------------------------
ggview::save_ggplot(fig, here::here("2026.06.23/2026.06.23.png"))
