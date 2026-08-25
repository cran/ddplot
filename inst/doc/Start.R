## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ddplot)

## ----fig.align='center', message=FALSE, warning=FALSE-------------------------
library(ggplot2) # needed for the mpg data frame

scatter_plot(
  data = mpg,
  x = "hwy",
  y = "cty",
  xtitle = "hwy variable",
  ytitle = "cty variable",
  title = "cty and hwy relationship",
  titleFontSize = 20
)


## ----fig.align='center'-------------------------------------------------------
scatter_plot(
  data = mpg,
  x = "displ",
  y = "cty",
  col = "tomato",
  bgcol = "pink",
  size = 3,
  stroke = "royalblue",
  strokeWidth = 1,
  xtitle = "displ variable",
  ytitle = "cty variable",
  xticks = 3,
  yticks = 3)


## -----------------------------------------------------------------------------
histogram(
  x = mpg$hwy,
  bins = 20,
  fill = "crimson",
  stroke = "white",
  strokeWidth = 1,
  title = "Distribution of the hwy variable",
  width = "20",
  height = "10"
)

## -----------------------------------------------------------------------------
animated_histogram(
  x = mpg$hwy,
  duration = 2000,
  delay = 100,
  fill = "lime",
  stroke = "white",
  bgcol = "white"
  )

## -----------------------------------------------------------------------------
flower(
  petalCount = 7,
  petalColor = "plum",
  rotationSpeed = 0
)

## -----------------------------------------------------------------------------
flower(
  petalCount = 15,
  petalWidth = 20,
  petalColor = "plum",
  rotationSpeed = 1.5
)

## -----------------------------------------------------------------------------
heart_fill(fill_level = 0.8, titleText = NULL, renderFillLabel = FALSE)

## -----------------------------------------------------------------------------
glass_fill(fill_level = 0.8, titleText = "Glass of water", renderFillLabel = TRUE)

## -----------------------------------------------------------------------------
plant_growth(0.4)

## -----------------------------------------------------------------------------
plant_growth(1)

## -----------------------------------------------------------------------------
workload_grid <- data.frame(
  hour = c("09:00","12:00","15:00","18:00",
           "09:00","12:00","15:00","18:00",
           "09:00","12:00","15:00","18:00"),
  team = c("North","North","North","North",
           "South","South","South","South",
           "East","East","East","East"),
  load = c(0.35,0.72,0.56,0.91,
           0.41,0.67,0.83,0.58,
           0.28,0.49,0.77,0.63),
  stringsAsFactors = FALSE
)

pulse_grid(
  data = workload_grid,
  x = "hour",
  y = "team",
  value = "load",
  title = "Support Team Load Through the Day",
  low = "#D9F0FF",
  high = "red",
  bgcol = "#0B132B",
  stroke = "#1C2541",
  strokeWidth = 1.5,
  pulseStrength = 0.22
)

## -----------------------------------------------------------------------------
reliability_grid <- data.frame(
  day = c(
    "Mon","Tue","Wed","Thu","Fri",
    "Mon","Tue","Wed","Thu","Fri",
    "Mon","Tue","Wed","Thu","Fri",
    "Mon","Tue","Wed","Thu","Fri"
  ),
  service = c(
    "API","API","API","API","API",
    "Billing","Billing","Billing","Billing","Billing",
    "Search","Search","Search","Search","Search",
    "Auth","Auth","Auth","Auth","Auth"
  ),
  incident_score = c(
    0.62, 0.44, 0.57, 0.78, 0.69,
    0.31, 0.36, 0.40, 0.51, 0.47,
    0.73, 0.81, 0.76, 0.88, 0.67,
    0.22, 0.27, 0.25, 0.34, 0.29
  ),
  stringsAsFactors = FALSE
)

pulse_grid(
  data = reliability_grid,
  x = "day",
  y = "service",
  value = "incident_score",
  title = "Service Reliability Pressure Map",
  low = "lightgreen",
  high = "#B00020",
  bgcol = "#081C15",
  labelColor = "#F1FAEE",
  titleColor = "#F1FAEE",
  stroke = "#2D6A4F",
  showValues = TRUE,
  digits = 2
)

## -----------------------------------------------------------------------------
flame(
  intensity = 20, 
  flameGradientColors = c("yellow", "orange", "darkred"),
  flameOutline = "darkred",
  bgcol = "#fefefe"
)

## -----------------------------------------------------------------------------
flame(
  intensity = 80, 
  flameGradientColors = c("green", "darkgreen", "darkblue"),
  flameOutline = "#fefefe",
  bgcol = "black"
)

## -----------------------------------------------------------------------------
# The data is a toy example and does not reflect the reality
vote_results_germany <- data.frame(
    political_party = c("SDP", "CDU", "Linke", "Grüne"),
    number_of_seats = c(200, 40, 30, 20)
)
parliament_chart(
  data = vote_results_germany,
  categorical_column = "political_party",
  numerical_column = "number_of_seats",
  title = "German Bundestag",
  seatSize = 10,
  bgcol = "#fefefe"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(dplyr)

mpg %>% group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  bar_chart(
    x = "manufacturer",
    y = "mean_cty",
    xFontSize = 10,
    yFontSize = 10,
    fill = "orange",
    strokeWidth = 2,
    ytitle = "average cty value",
    title = "Average City Miles per Gallon by manufacturer"
  )

## ----message=FALSE, warning=FALSE---------------------------------------------
mpg %>% group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  bar_chart(
    x = "manufacturer",
    y = "mean_cty",
    sort = "ascending",
    xFontSize = 10,
    yFontSize = 10,
    fill = "orange",
    strokeWidth = 1,
    ytitle = "average cty value",
    title = "Average City Miles per Gallon by manufacturer",
    titleFontSize = 16
  )

## -----------------------------------------------------------------------------
mpg %>% group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  horz_bar_chart(
    label = "manufacturer",
    value = "mean_cty",
    sort = "ascending",
    labelFontSize  = 10,
    valueFontSize = 10,
    fill = "orange",
    stroke = "crimson",
    strokeWidth = 1,
    valueTitle  = "average cty value",
    title = "Average City Miles per Gallon by manufacturer",
    titleFontSize = 16
  )

## -----------------------------------------------------------------------------
mpg %>% group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  horz_bar_chart(
    label = "manufacturer",
    value = "mean_cty",
    sort = "descending",
    labelFontSize  = 10,
    valueFontSize = 10,
    bgcol = "black",
    axisCol = "white",
    fill = "white",
    stroke = "white",
    strokeWidth = 1,
    valueTitle  = "average cty value",
    labelTitle = "Manufacturers",
    title = "Average City Miles per Gallon by manufacturer",
    titleFontSize = 16
  )

## -----------------------------------------------------------------------------
mpg %>% group_by(drv) %>%
  summarise(median_cty = median(cty)) %>%
  lollipop_chart(
    x = "drv",
    y = "median_cty",
    sort = "ascending",
    xtitle = "drv variable",
    ytitle = "median cty",
    title = "Median cty per drv",
    xFontSize = 20
  )

## -----------------------------------------------------------------------------

mpg %>% filter(year == 2008) %>%
lollipop_chart(
    x = "manufacturer",
    y = "hwy",
    circleFill = 'red',
    circleStroke = 'orange',
    circleRadius = 5,
    sort = "none",
    xFontSize = 10
  )

## -----------------------------------------------------------------------------
mpg %>% group_by(manufacturer) %>%
  summarise(median_cty = median(cty)) %>%
  horz_lollipop(
    label = "manufacturer",
    value = "median_cty",
    sort = "descending")

## -----------------------------------------------------------------------------
mpg %>% filter(year == 2008) %>%
horz_lollipop(
    label = "manufacturer",
    value = "hwy",
    circleFill = 'red',
    circleStroke = 'orange',
    circleRadius = 5,
    sort = "none"
  )

## -----------------------------------------------------------------------------
# starwars is part of the dplyr data frame
mini_starwars <- starwars %>% tidyr::drop_na(mass) %>%
  sample_n(size = 5) # getting 5 random values

pie_chart(
  data = mini_starwars,
  value = "mass",
  label = "name"
)

## -----------------------------------------------------------------------------
pie_chart(
  data = mini_starwars,
  value = "mass",
  label = "name",
  padRadius = 200,
  padAngle = 0.1,
  cornerRadius = 50,
  innerRadius = 10
)

## -----------------------------------------------------------------------------
pie_chart(
  data = mini_starwars,
  value = "mass",
  label = "name",
  innerRadius = 120,
  cornerRadius = 20,
  title = "5 Starwars characters ranked by their mass",
  titleFontSize = 16,
  bgcol = "yellow"
)

## -----------------------------------------------------------------------------
# 1. converting AirPassengers to a tidy data frame
airpassengers <- data.frame(
  passengers = as.matrix(AirPassengers),
  date= zoo::as.Date(time(AirPassengers))
)

# 2. plotting the line chart
line_chart(
  data = airpassengers,
  x = "date",
  y = "passengers"
)

## -----------------------------------------------------------------------------
line_chart(
  data = airpassengers,
  x = "date",
  y = "passengers",
  curve = "curveStep"
)

## -----------------------------------------------------------------------------
line_chart(
  data = airpassengers,
  x = "date",
  y = "passengers",
  curve = "curveCardinal"
)

## -----------------------------------------------------------------------------
line_chart(
  data = airpassengers,
  x = "date",
  y = "passengers",
  curve = "curveBasis"
)

## -----------------------------------------------------------------------------
anim_line_chart(
  data = airpassengers,
  x = "date",
  y = "passengers",
  duration = 10000, # in milliseconds (10 seconds)
  curve = "curveCardinal"
  )

## -----------------------------------------------------------------------------
# 1. converting AirPassengers to a tidy data frame
airpassengers <- data.frame(
  passengers = as.matrix(AirPassengers),
  date= zoo::as.Date(time(AirPassengers))
)

# 2. plotting the area chart
area_chart(
  data = airpassengers,
  x = "date",
  y = "passengers",
  fill = "purple",
  bgcol = "white"
)

## -----------------------------------------------------------------------------
airpassengers <- data.frame(
  passengers_lower = as.matrix(AirPassengers),
  passengers_upper = as.matrix(AirPassengers) + 40,
  date= zoo::as.Date(time(AirPassengers))
)

area_band(
  data = airpassengers,
  x = "date",
  yLower = "passengers_lower",
  yUpper = "passengers_upper",
  fill = "yellow",
  stroke = "black"
)

## -----------------------------------------------------------------------------
data <- data.frame(
  date = c(
    "2000-01-01", "2000-02-01", "2000-03-01", "2000-04-01",
    "2000-05-01", "2000-06-01", "2000-07-01",
    "2000-08-01", "2000-09-01", "2000-10-01"
  ),
  Trade = c(
    2000,1023, 983, 2793, 1821, 1837, 1792, 1853, 791, 739
  ),
  Manufacturing = c(
    734, 694, 739, 736, 685, 621, 708, 685, 667, 693
  ),
  Leisure = c(
    1782, 1779, 1789, 658, 675, 833, 786, 675, 636, 691
  ),
  Agriculture = c(
    655, 587,623, 517, 561, 2545, 636, 584, 559, 2504
  )
)

data

## -----------------------------------------------------------------------------
stacked_area_chart(
  data = data,
  x = "date",
  legendTextSize = 14
  )

## -----------------------------------------------------------------------------
stacked_area_chart(
  data = data,
  x = "date",
  legendTextSize = 14,
  curve = "curveCardinal",
  colorCategory = "Accent",
  bgcol = "white",
  stroke = "black",
  strokeWidth = 1
  )

## -----------------------------------------------------------------------------
stacked_area_chart(
  data = data,
  x = "date",
  legendTextSize = 14,
  curve = "curveBasis",
  colorCategory = "Set3",
  bgcol = "black",
  axisCol = "white",
  xticks = 4,
  stroke = "black"
  )

## ----eval = FALSE-------------------------------------------------------------
# gapminder_subset <- gapminder::gapminder %>%
#   select(country, year, pop) %>%
#   filter(country %in% c("Japan", "Mexico", "Germany", "Brazil", "Philippines", "Vietnam")) %>%
#   mutate(pop = pop/1e6)
# 
# 
# gapminder_subset %>%
#   slice_sample(n = 10)
# 
# #>    year       pop     country
# #> 1  2007  91.07729 Philippines
# #> 2  1997  76.04900     Vietnam
# #> 3  1972 107.18827       Japan
# #> 4  1967  39.46391     Vietnam
# #> 5  1952  30.14432      Mexico
# #> 6  1987 142.93808      Brazil
# #> 7  1997 168.54672      Brazil
# #> 8  1962  41.12148      Mexico
# #> 9  1952  69.14595     Germany
# #> 10 1957  91.56301       Japan

## ----echo = FALSE-------------------------------------------------------------
gapminder_subset <- data.frame(
  year = c(
    1952L,1957L,1962L,1967L,1972L,1977L,
    1982L,1987L,1992L,1997L,2002L,2007L,1952L,1957L,1962L,
    1967L,1972L,1977L,1982L,1987L,1992L,1997L,2002L,2007L,
    1952L,1957L,1962L,1967L,1972L,1977L,1982L,1987L,1992L,
    1997L,2002L,2007L,1952L,1957L,1962L,1967L,1972L,1977L,
    1982L,1987L,1992L,1997L,2002L,2007L,1952L,1957L,1962L,
    1967L,1972L,1977L,1982L,1987L,1992L,1997L,2002L,2007L,
    1952L,1957L,1962L,1967L,1972L,1977L,1982L,1987L,1992L,
    1997L,2002L,2007L
  ),
  pop = c(
    56.60256,65.551171,76.03939,88.049823,
    100.840058,114.313951,128.962939,142.938076,155.975974,
    168.546719,179.914212,190.010647,69.145952,71.019069,73.739117,
    76.368453,78.717088,78.160773,78.335266,77.718298,
    80.597764,82.011073,82.350671,82.400996,86.459025,91.563009,
    95.831757,100.825279,107.188273,113.872473,118.454974,
    122.091325,124.329269,125.956499,127.065841,127.467972,30.144317,
    35.015548,41.121485,47.995559,55.984294,63.759976,
    71.640904,80.122492,88.11103,95.895146,102.479927,108.700891,
    22.438691,26.072194,30.325264,35.3566,40.850141,46.850962,
    53.456774,60.017788,67.185766,75.012988,82.995088,91.077287,
    26.246839,28.998543,33.79614,39.46391,44.655014,50.533506,
    56.142181,62.826491,69.940728,76.048996,80.908147,
    85.262356
  ),
  country = as.factor(c(
    "Brazil","Brazil",
    "Brazil","Brazil","Brazil","Brazil","Brazil",
    "Brazil","Brazil","Brazil","Brazil","Brazil","Germany",
    "Germany","Germany","Germany","Germany",
    "Germany","Germany","Germany","Germany","Germany",
    "Germany","Germany","Japan","Japan","Japan","Japan",
    "Japan","Japan","Japan","Japan","Japan","Japan",
    "Japan","Japan","Mexico","Mexico","Mexico",
    "Mexico","Mexico","Mexico","Mexico","Mexico",
    "Mexico","Mexico","Mexico","Mexico","Philippines",
    "Philippines","Philippines","Philippines","Philippines",
    "Philippines","Philippines","Philippines",
    "Philippines","Philippines","Philippines","Philippines",
    "Vietnam","Vietnam","Vietnam","Vietnam",
    "Vietnam","Vietnam","Vietnam","Vietnam","Vietnam",
    "Vietnam","Vietnam","Vietnam"
  ))
)

## -----------------------------------------------------------------------------
gapminder_subset %>%
  bar_chart_race(
    x = "pop",
    y = "country",
    time = "year",
    ytitle = "Country",
    xtitle = "Population (in millions)",
    title = "Bar chart race of country populations"
  )

## -----------------------------------------------------------------------------
gapminder_subset %>%
  bar_chart_race(
    x = "pop",
    y = "country",
    time = "year",
    transitionDur = 1000,
    frameDur = 0,
    ytitle = "Country",
    xtitle = "Population (in millions)",
    title = "Bar chart race of country populations"
  )

## -----------------------------------------------------------------------------
gapminder_subset %>%
  bar_chart_race(
    x = "pop",
    y = "country",
    time = "year",
    ease = "BackInOut",
    ytitle = "Country",
    xtitle = "Population (in millions)",
    title = "Bar chart race of country populations",
    timeLabelOpts = list(
      size = 40,
      prefix = "Year: ",
      xOffset = 0.2
    )
  )

## -----------------------------------------------------------------------------
liquid_chart(value = 0.55, title = "Memory")

## -----------------------------------------------------------------------------
liquid_chart(
  value         = 0.28,
  label         = "28%",
  fillColor     = "tomato",
  circleColor   = "tomato",
  textColor     = "tomato",
  waveTextColor = "white",
  title         = "Errors"
)

## -----------------------------------------------------------------------------
liquid_chart(
  value         = 0.72,
  label         = "72%",
  fillColor     = "#27ae60",
  waveAmplitude = 0.05,
  waveCount     = 3,
  waveSpeed     = 1.5,
  title         = "Battery"
)

## -----------------------------------------------------------------------------
pnl <- data.frame(
  label   = c("Revenue", "COGS", "Gross Profit",
              "R&D", "S&M", "G&A", "Operating Income"),
  value   = c(1200, -450, 750, -120, -90, -60, 480),
  measure = c("relative", "relative", "total",
              "relative", "relative", "relative", "total")
)

waterfall_chart(
  data   = pnl,
  x      = "label",
  y      = "value",
  measure = "measure",
  title  = "P&L bridge",
  ytitle = "USD thousands"
)

## -----------------------------------------------------------------------------
cashflow <- data.frame(
  month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"),
  delta = c(120, -30, 80, -60, 95, -20)
)

waterfall_chart(
  data          = cashflow,
  x             = "month",
  y             = "delta",
  title         = "Monthly cash flow",
  ytitle        = "USD thousands",
  positiveColor = "steelblue",
  negativeColor = "tomato"
)

## -----------------------------------------------------------------------------
life_exp <- data.frame(
  country   = c("Brazil", "China", "Egypt", "India",
                "Japan", "Mexico", "Nigeria", "Turkey"),
  year_1952 = c(50.9, 44.0, 41.9, 37.4, 63.0, 50.8, 36.3, 43.6),
  year_2007 = c(72.4, 72.9, 71.3, 64.7, 82.6, 76.2, 46.9, 71.8)
)

dumbbell_chart(
  data     = life_exp,
  x1       = "year_1952",
  x2       = "year_2007",
  y        = "country",
  x1Label  = "1952",
  x2Label  = "2007",
  title    = "Life expectancy: 1952 vs 2007",
  xtitle   = "Life expectancy (years)",
  sort     = "ascending"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(dplyr)

mpg_summary <- mpg |>
  group_by(class) |>
  summarise(city = mean(cty), highway = mean(hwy))

dumbbell_chart(
  data    = mpg_summary,
  x1      = "city",
  x2      = "highway",
  y       = "class",
  x1Label = "City",
  x2Label = "Highway",
  col1    = "steelblue",
  col2    = "darkorange",
  title   = "City vs highway fuel economy by class",
  xtitle  = "Miles per gallon",
  sort    = "ascending"
)

## ----fig.align='center', message=FALSE, warning=FALSE-------------------------
library(ggplot2) # for the mpg dataset

# Single swarm — full distribution of highway fuel economy
beeswarm_plot(
  data   = mpg,
  x      = "hwy",
  col    = "steelblue",
  xtitle = "Highway miles per gallon",
  title  = "Distribution of hwy"
)

## ----fig.align='center', message=FALSE, warning=FALSE-------------------------
beeswarm_plot(
  data   = mpg,
  x      = "hwy",
  group  = "class",
  xtitle = "Highway miles per gallon",
  title  = "Highway fuel economy by vehicle class", 
  tooltip = "manufacturer"
)

## ----fig.align='center'-------------------------------------------------------
beeswarm_plot(
  data         = iris,
  x            = "Sepal.Length",
  group        = "Species",
  colorPalette = "Set2",
  radius       = 5,
  opacity      = 0.8,
  xtitle       = "Sepal length (cm)",
  title        = "Sepal length by species"
)

## -----------------------------------------------------------------------------
bullet_chart(
  value    = 270,
  target   = 300,
  ranges   = c(150, 225, 350),
  title    = "Revenue",
  subtitle = "USD thousands"
)

## -----------------------------------------------------------------------------
bullet_chart(
  value      = 7.4,
  target     = 8.0,
  ranges     = c(4, 7, 10),
  title      = "Satisfaction",
  subtitle   = "out of 10",
  valueColor = "steelblue"
)

## -----------------------------------------------------------------------------
bullet_chart(
  value       = 320,
  target      = 250,
  ranges      = c(200, 500, 1000),
  rangeColors = c("#f0f0f0", "#d9d9d9", "#bdbdbd"),
  title       = "Response",
  subtitle    = "ms"
)

## -----------------------------------------------------------------------------
gauge_chart(
  value = 8,
  title = "Memory"
)

## -----------------------------------------------------------------------------
gauge_chart(
  value = 72,
  min = 0,
  max = 100,
  title = "CPU Load",
  warningZone  = 60,
  warningColor = "orange",
  dangerZone   = 80,
  dangerColor  = "red"
)

## -----------------------------------------------------------------------------
gauge_chart(
  value = 530,
  min = 0,
  max = 1000,
  title = "Response (ms)",
  warningZone  = 400,
  warningColor = "orange",
  dangerZone   = 700,
  dangerColor  = "red"
)

## ----eval=FALSE---------------------------------------------------------------
# library(shiny)
# library(ddplot)
# library(r2d3)
# 
# ui <- fluidPage(
# 
#   shiny::h2("Example ddplot application"),
# 
#   shiny::selectInput(
#     inputId = "colors",
#     label = NULL,
#     choices = colors(),
#     selected = "springgreen3"
#   ),
# 
#   shiny::br(),
# 
#   shiny::sliderInput(
#     inputId = "slider",
#     label = NULL,
#     min = 1,
#     max = 10,
#     value = 3
#   ),
#   shiny::br(),
#   mainPanel(
#     uiOutput("ddplot_ui")
#   )
# )
# 
# server <- function(input, output) {
#   output$ddplot_ui <- renderUI({
#     widget <- ddplot::scatter_plot(
#       data = iris,
#       x = "Sepal.Length",
#       y = "Sepal.Width",
#       size = input$slider,
#       col = input$colors
#     )
#     htmlwidgets::saveWidget(widget, "www/temp_ddplot.html", selfcontained = TRUE)
#     tags$iframe(src = "temp_ddplot.html", width = "100%", height = "400px", frameborder = 0)
#   })
# }
# 
# shinyApp(ui = ui, server = server)

