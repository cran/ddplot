## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ddplot)

## ----fig.align='center', message=FALSE, warning=FALSE-------------------------
library(ggplot2) # needed for the mpg data frame

scatterPlot(
  data = mpg,
  x = "hwy",
  y = "cty",
  xtitle = "hwy variable",
  ytitle = "cty variable",
  title = "cty and hwy relationship",
  titleFontSize = 20
)


## ---- fig.align='center'------------------------------------------------------
scatterPlot(
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
animatedHistogram(
  x = mpg$hwy,
  duration = 2000,
  delay = 100,
  fill = "lime",
  stroke = "white",
  bgcol = "white"
  )

## ----fig.align='center', message=FALSE, warning=FALSE-------------------------
library(dplyr)

mpg %>% group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  barChart(
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
  barChart(
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
  horzBarChart(
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
  horzBarChart(
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
  lollipopChart(
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
lollipopChart(
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
  horzLollipop(
    label = "manufacturer",
    value = "median_cty",
    sort = "descending")

## -----------------------------------------------------------------------------
mpg %>% filter(year == 2008) %>%
horzLollipop(
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

pieChart(
  data = mini_starwars,
  value = "mass",
  label = "name"
)

## -----------------------------------------------------------------------------
pieChart(
  data = mini_starwars,
  value = "mass",
  label = "name",
  padRadius = 200,
  padAngle = 0.1,
  cornerRadius = 50,
  innerRadius = 10
)

## -----------------------------------------------------------------------------
pieChart(
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
lineChart(
  data = airpassengers,
  x = "date",
  y = "passengers"
)

## -----------------------------------------------------------------------------
lineChart(
  data = airpassengers,
  x = "date",
  y = "passengers",
  curve = "curveStep"
)

## -----------------------------------------------------------------------------
lineChart(
  data = airpassengers,
  x = "date",
  y = "passengers",
  curve = "curveCardinal"
)

## -----------------------------------------------------------------------------
lineChart(
  data = airpassengers,
  x = "date",
  y = "passengers",
  curve = "curveBasis"
)

## -----------------------------------------------------------------------------
animLineChart(
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
areaChart(
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

areaBand(
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
stackedAreaChart(
  data = data,
  x = "date",
  legendTextSize = 14
  )

## -----------------------------------------------------------------------------
stackedAreaChart(
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
stackedAreaChart(
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

## ---- eval = FALSE------------------------------------------------------------
#  <<<<<<< HEAD
#  gapminder_subset <- gapminder::gapminder %>%
#    select(country, year, pop) %>%
#    filter(country %in% c("Japan", "Mexico", "Germany", "Brazil", "Philippines", "Vietnam")) %>%
#    mutate(pop = pop/1e6)
#  =======
#  gapminder_subset <- gapminder::gapminder %>% select(country, year, pop) %>%
#      filter(country %in% c("Japan", "Mexico", "Germany", "Brazil", "Mexico", "Philippines", "Vietnam")) %>%
#      mutate(pop = pop/1e6)
#  >>>>>>> 6bab1415a132b17bda7192e7e2e63758614d5161
#  
#  gapminder_subset %>%
#    slice_sample(n = 10)
#  
#  #>    year       pop     country
#  #> 1  2007  91.07729 Philippines
#  #> 2  1997  76.04900     Vietnam
#  #> 3  1972 107.18827       Japan
#  #> 4  1967  39.46391     Vietnam
#  #> 5  1952  30.14432      Mexico
#  #> 6  1987 142.93808      Brazil
#  #> 7  1997 168.54672      Brazil
#  #> 8  1962  41.12148      Mexico
#  #> 9  1952  69.14595     Germany
#  #> 10 1957  91.56301       Japan

## ---- echo = FALSE------------------------------------------------------------
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
  barChartRace(
    x = "pop",
    y = "country",
    time = "year",
    ytitle = "Country",
    xtitle = "Population (in millions)",
    title = "Bar chart race of country populations"
  )

## -----------------------------------------------------------------------------
gapminder_subset %>%
  barChartRace(
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
  barChartRace(
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

