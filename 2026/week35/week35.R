### Setup ---------------------------------------------------------------------

pacman::p_load(
  tidyverse,
  plotly
)

# Colors
background_color <- "#F4F1EA"
land_color       <- "#E5DED0"
border_color     <- "#9B968D"
point_color      <- "#8C3F3F"
text_color       <- "#282522"


### Data ----------------------------------------------------------------------

world_castles <- tidytuesdayR::tt_load(2026, week = 35)$world_castles

europe_castles <- world_castles |>
  filter(
    between(lon, -12, 45),
    between(lat, 34, 72)
  ) |>
  mutate(
    category = str_to_title(category),
    tooltip = paste0(
      "<b>", name, "</b>",
      "<br>Country: ", country,
      "<br>Type: ", category,
      "<br>Built: ", century
    )
  )


### Plot ----------------------------------------------------------------------

europe_map <- plot_ly(
  data = europe_castles,
  type = "scattergeo",
  mode = "markers",
  lon = ~lon,
  lat = ~lat,
  text = ~tooltip,
  hoverinfo = "text",
  customdata = ~paste(
    image, name, country, category, century, wikipedia,
    sep = "|||"
  ),
  marker = list(
    size = 4,
    opacity = 0.65,
    color = point_color
  ),
  hoverlabel = list(
    bgcolor = background_color,
    bordercolor = border_color,
    font = list(
      family = "Roboto, Arial, sans-serif",
      size = 14,
      color = text_color
    )
  ),
  showlegend = FALSE
) |>
  layout(
    paper_bgcolor = background_color,
    plot_bgcolor = background_color,
    
    font = list(
      family = "Roboto, Arial, sans-serif",
      color = text_color
    ),
    
    title = list(
      text = paste0(
        "<span style='font-size:22px;'><b>A Continent of Castles</b></span>",
        "<br>",
        "<span style='font-size:16px; color:#4A4340;'>",
        "Explore castles and ruins across Europe. ",
        "Hover to identify a site. Click to discover its history.",
        "</span>"
      ),
      x = 0.075,
      xanchor = "left",
      y = 0.93,
      yanchor = "top"
    ),
    
    geo = list(
      domain = list(
        x = c(0.05, 0.66),
        y = c(0.05, 0.95)
      ),
      projection = list(type = "natural earth"),
      lonaxis = list(range = c(-12, 45)),
      lataxis = list(range = c(34, 72)),
      showland = TRUE,
      landcolor = land_color,
      showocean = TRUE,
      oceancolor = background_color,
      showcountries = TRUE,
      countrycolor = border_color,
      countrywidth = 0.6,
      showcoastlines = TRUE,
      coastlinecolor = border_color,
      coastlinewidth = 0.7,
      bgcolor = background_color
    ),
    
    margin = list(
      t = 85,
      r = 45,
      b = 75,
      l = 45
    )
  )


### Click interaction ---------------------------------------------------------

europe_map <- europe_map |>
  htmlwidgets::onRender(
    "
    function(el, x, data) {

      // Load Roboto
      var fontLink = document.createElement('link');
      fontLink.rel = 'stylesheet';
      fontLink.href = 'https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap';
      document.head.appendChild(fontLink);

      // Information panel
      var panel = document.createElement('div');

      Object.assign(panel.style, {
        position: 'absolute',
        top: '120px',
        right: '5%',
        width: '25%',
        padding: '16px',
        boxSizing: 'border-box',
        background: data.background_color,
        fontFamily: 'Roboto, Arial, sans-serif',
        color: data.text_color,
        zIndex: '1000'
      });

      panel.innerHTML = `
        <div style='
          padding: 30px 10px;
          text-align: center;
          font-size: 16px;
          line-height: 1.5;
        '>
          Click a castle on the map<br>
          to explore it.
        </div>
      `;

      el.appendChild(panel);

      // Caption
      var caption = document.createElement('div');

      Object.assign(caption.style, {
        position: 'absolute',
        left: '8%',
        bottom: '18px',
        fontFamily: 'Roboto, Arial, sans-serif',
        fontSize: '12px',
        color: data.text_color
      });

      caption.innerHTML = `
        <b>Data:</b> TidyTuesday &nbsp;|&nbsp;
        <b>Graphic:</b> Mark Deming &nbsp;|&nbsp;
        @jmdeming
      `;

      el.appendChild(caption);

      // Update panel on click
      el.on('plotly_click', function(d) {

        var castle = d.points[0].customdata.split('|||');

        var image     = castle[0];
        var name      = castle[1];
        var country   = castle[2];
        var category  = castle[3];
        var century   = castle[4];
        var wikipedia = castle[5];

        panel.innerHTML = `
          <img
            src='${image}'
            style='
              width: 100%;
              height: 240px;
              object-fit: contain;
              background: ${data.background_color};
              margin-bottom: 12px;
          '
        >

          <div style='
            font-size: 18px;
            font-weight: 600;
            margin-bottom: 6px;
          '>
            ${name}
          </div>

          <div style='
            font-size: 14px;
            line-height: 1.5;
            margin-bottom: 12px;
          '>
            ${country}<br>
            ${category} | ${century}
          </div>

          <a
            href='${wikipedia}'
            target='_blank'
            style='
              font-size: 13px;
              color: ${data.point_color};
              text-decoration: none;
            '
          >
            Learn more →
          </a>
        `;
      });
    }
    ",
    
    data = list(
      background_color = background_color,
      text_color = text_color,
      point_color = point_color
    )
  )


### Save ----------------------------------------------------------------------

htmlwidgets::saveWidget(
  europe_map,
  here::here("2026/week35/europe_castles.html"),
  selfcontained = TRUE
)

webshot2::webshot(
  url = here::here("2026/week35/europe_castles.html"),
  file = here::here("2026/week35/europe_castles.png"),
  vwidth = 1600,
  vheight = 1000
)
  