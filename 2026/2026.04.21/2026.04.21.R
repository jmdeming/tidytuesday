################
### Packages ### 
################

# Packages
pacman::p_load(here, 
               tidyverse, 
               usethis, 
               gitcreds, 
               tidytuesdayR, 
               countrycode, 
               ggridges, 
               extrafont, 
               htmltools)
 
############
### Data ###
############

# TT Data
tuesdata <- tidytuesdayR::tt_load('2026-04-21', )
financing_schemes <- tuesdata$financing_schemes
health_spending <- tuesdata$health_spending
spending_purpose <- tuesdata$spending_purpose

# WDI Data
#WDI::WDIsearch("GDP")
my_wdi_vars <- c("SP.POP.TOTL", "NY.GDP.MKTP.KD", "NY.GDP.DEFL.ZS")
wdi <- WDI::WDI(country = "all",
                indicator = my_wdi_vars,
                extra = TRUE)

################
### Cleaning ###
################

my_spending_dat <- health_spending |> 
  # Grab gov health expenditure
  filter(indicator_code == "gghed_usd2023")

wdi <- wdi |> 
  # Remove aggregates
  filter(region != "Aggregates") |> 
  # Rename vars
  rename(pop = SP.POP.TOTL,
         gdp_2015 = NY.GDP.MKTP.KD,
         deflator = NY.GDP.DEFL.ZS)

# Merge data
merged <- my_spending_dat |> 
  left_join(wdi, by = c("year", "iso3_code" = "iso3c"), relationship = "many-to-many")

# Deflator is indexed to 2015 = 100. So, 
# rebase spending data from 2023 to 2015.

merged <- merged |> 
  group_by(iso3_code) |> 
  mutate(deflator_2023 = deflator[year == 2023][1],
         health_spending_2015_usd = value * (100 / deflator_2023)) |> 
  ungroup()

merged <- merged |> 
  mutate(health_pct_gdp = health_spending_2015_usd / gdp_2015,
         health_pc = health_spending_2015_usd / pop)

# Grab LatAm countries
latam <- c("MEX", "GTM", "HND", "SLV", "NIC", "CRI", "PAN", "COL", "VEN", "PER", 
           "ECU", "BRZ", "BOL", "CHL", "ARG", "PGY", "UGY", "CUB", "DOM", "HTI")

# Code LatAm countries
merged <- merged |> 
  mutate(is_latam = ifelse(iso3_code %in% latam, 1, 0))

############
### Plot ###
############

p <-
merged |> 
  filter(year == 2023) |> 
  
  
  ggplot(aes(x = health_pc, 
             y = health_pct_gdp, 
             size = pop*2, 
             color = as.factor(is_latam),
             text = paste(
                          "<b> Government health expenditure in ", country, "</b>",
                          "<b>\nPercent of GDP:</b> ", round(health_pct_gdp*100, 2), "%",
                          "<b>\nPer person:</b> ", "$", scales::comma(round(health_pc, 1)), " in $US 2015", sep = "")
             )) +
  
  geom_point(alpha = .7) +
  
  geom_vline(xintercept = 86, linetype = "dotted", color = "gray", linewidth = .5) +
  annotate("text", x = 87, y = .15, label = "$86 per person target →", size = 3.5, hjust = 0) +
  
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "gray", linewidth = .5) +
  annotate("text", x = .255, y = .052, label = "↑ 5% of GDP target", size = 3.5, hjust = 0) +
  
  scale_x_log10(limits = c(.25, 10000),
                breaks = c(0, 1, 10, 100, 1000, 10000),
                labels = scales::label_dollar()) +
  
  coord_cartesian(expand = TRUE,
                  clip = "off") +
 
  scale_y_continuous(limits = c(0, .15),
                     breaks = seq(0, .15, .05),
                     labels = scales::label_percent()) +


  scale_color_manual(values = c("gray80", "#69C5D1"),
                     guide = FALSE) +
  
  scale_size_continuous(range = c(3, 12),
                        guide = FALSE)  +
  
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend = element_blank()) +
  labs(title = "Domestic General Government Health Expenditure, 2023",
       subtitle = "Latin American Countries Shown in Blue\n",
       x = "\nUS$ per person",
       y = "Percent of GDP\n",
       color = "Latin America vs. Rest of World")

p

##############
### Plotly ###
##############

ply <-
ggplotly(p, tooltip = "text", width = 1200, height = 800) |> 
  style(textposition = "right")

htmlwidgets::saveWidget(ply, here("2026", "2026.04.21", "2026.04.21.html"))
