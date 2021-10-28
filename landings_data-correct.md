Landings Data
================

``` r
library('readr')
library('ggplot2')
library('magrittr')
library('tidyverse')
library('lubridate')
library('dplyr')
```

Import data

``` r
landings_data <- read_csv("sample_landings_data_raw.csv")

landings_data
```

    ## # A tibble: 7,214 x 8
    ##       yy dat      trip effort gr    sp              l_cm  w_cm
    ##    <dbl> <chr>   <dbl>  <dbl> <chr> <chr>          <dbl> <dbl>
    ##  1  2003 4/30/03     1     10 Trap  Caesoi cunning    36 1089.
    ##  2  2003 4/30/03     1     10 trap  Caesio cuning     29  565.
    ##  3  2003 4/30/03     1     10 Trap  Caesio cuning     34  916.
    ##  4  2003 4/30/03     1     10 Trap  Caesio cuning     36 1089.
    ##  5  2003 4/30/03     1     10 Trap  Caesio cuning     34  916.
    ##  6  2003 4/30/03     1     10 Trap  Caesoi cunning    28  508.
    ##  7  2003 4/30/03     1     10 Trap  Caesio cuning     30  627.
    ##  8  2003 4/30/03     1     10 Trap  Caesio cuning     27  455.
    ##  9  2003 4/30/03     1     10 Trap  Caesio cuning     33  837.
    ## 10  2003 4/30/03     1     10 Trap  Caesio cuning     35 1000.
    ## # ... with 7,204 more rows

Start with the landings_data data frame and rename columns. Also turn
the date column into a date format that R recognises.

``` r
landings_data <- landings_data %>%
 rename( Year = yy,
         Date = dat,
         Trip_ID = trip,
         Effort_Hours = effort,
         Gear = gr,
         Species = sp,
         Length_cm = l_cm,
         Weight_g = w_cm) %>%
  mutate(Date = mdy(Date))
```

Print data.

``` r
landings_data
```

    ## # A tibble: 7,214 x 8
    ##     Year Date       Trip_ID Effort_Hours Gear  Species        Length_cm Weight_g
    ##    <dbl> <date>       <dbl>        <dbl> <chr> <chr>              <dbl>    <dbl>
    ##  1  2003 2003-04-30       1           10 Trap  Caesoi cunning        36    1089.
    ##  2  2003 2003-04-30       1           10 trap  Caesio cuning         29     565.
    ##  3  2003 2003-04-30       1           10 Trap  Caesio cuning         34     916.
    ##  4  2003 2003-04-30       1           10 Trap  Caesio cuning         36    1089.
    ##  5  2003 2003-04-30       1           10 Trap  Caesio cuning         34     916.
    ##  6  2003 2003-04-30       1           10 Trap  Caesoi cunning        28     508.
    ##  7  2003 2003-04-30       1           10 Trap  Caesio cuning         30     627.
    ##  8  2003 2003-04-30       1           10 Trap  Caesio cuning         27     455.
    ##  9  2003 2003-04-30       1           10 Trap  Caesio cuning         33     837.
    ## 10  2003 2003-04-30       1           10 Trap  Caesio cuning         35    1000.
    ## # ... with 7,204 more rows

Check for missing values

``` r
landings_data[!complete.cases(landings_data),]
```

    ## # A tibble: 3 x 8
    ##    Year Date       Trip_ID Effort_Hours Gear     Species      Length_cm Weight_g
    ##   <dbl> <date>       <dbl>        <dbl> <chr>    <chr>            <dbl>    <dbl>
    ## 1  2003 2003-05-01      10           10 <NA>     Caesio cuni~      19       157.
    ## 2  2003 2003-05-01      10           10 Handline Caesio cuni~      19        NA 
    ## 3  2004 2004-12-18      NA            9 Trap     Caesio cuni~      20.1     186.

Remove rows with missing values

``` r
landings_data <- na.omit(landings_data)
```

Print edited data set

``` r
landings_data
```

    ## # A tibble: 7,211 x 8
    ##     Year Date       Trip_ID Effort_Hours Gear  Species        Length_cm Weight_g
    ##    <dbl> <date>       <dbl>        <dbl> <chr> <chr>              <dbl>    <dbl>
    ##  1  2003 2003-04-30       1           10 Trap  Caesoi cunning        36    1089.
    ##  2  2003 2003-04-30       1           10 trap  Caesio cuning         29     565.
    ##  3  2003 2003-04-30       1           10 Trap  Caesio cuning         34     916.
    ##  4  2003 2003-04-30       1           10 Trap  Caesio cuning         36    1089.
    ##  5  2003 2003-04-30       1           10 Trap  Caesio cuning         34     916.
    ##  6  2003 2003-04-30       1           10 Trap  Caesoi cunning        28     508.
    ##  7  2003 2003-04-30       1           10 Trap  Caesio cuning         30     627.
    ##  8  2003 2003-04-30       1           10 Trap  Caesio cuning         27     455.
    ##  9  2003 2003-04-30       1           10 Trap  Caesio cuning         33     837.
    ## 10  2003 2003-04-30       1           10 Trap  Caesio cuning         35    1000.
    ## # ... with 7,201 more rows

Check for typos in the gear variable

``` r
unique(landings_data$Gear)
```

    ## [1] "Trap"     "trap"     "Muroami"  "Handline" "Gillnet"  "Trolling" "Speargun"

Trap appears twice - fix this

``` r
landings_data <- landings_data %>%
  mutate(Gear = tolower(Gear))
unique(landings_data$Gear)
```

    ## [1] "trap"     "muroami"  "handline" "gillnet"  "trolling" "speargun"

Check for typos in species variable

``` r
unique(landings_data$Species)
```

    ## [1] "Caesoi cunning" "Caesio cuning"

Check how many times each spelling occurs

``` r
landings_data %>%
  filter(Species == "Caesoi cunning") %>%
  nrow()
```

    ## [1] 2

``` r
landings_data %>%
  filter(Species == "Caesio cuning") %>%
  nrow()
```

    ## [1] 7209

Caesio cuning is typo as it appears only twice, replace with correct
spelling

``` r
landings_data <- landings_data %>%
  mutate(Species = replace(Species,Species == "Caesoi cunning", "Caesio cuning"))

unique(landings_data$Species)
```

    ## [1] "Caesio cuning"

Look at the distribution of length

``` r
summary(landings_data$Length_cm)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    2.00   23.00   25.00   25.81   27.00 2400.00

The max value is much larger than the mean/median values. Make a plot to
check for outliers.

``` r
plot(landings_data$Length_cm)
```

![](landings_data-correct_files/figure-gfm/length%20plot-1.png)<!-- -->

We see there is an outlier. Remove this error and make a new plot.

``` r
landings_data <- landings_data %>%
  filter(Length_cm < 100)
```

``` r
plot(landings_data$Length_cm)
```

![](landings_data-correct_files/figure-gfm/corrected%20lenth%20plot-1.png)<!-- -->

Examine data frame again

``` r
landings_data
```

    ## # A tibble: 7,208 x 8
    ##     Year Date       Trip_ID Effort_Hours Gear  Species       Length_cm Weight_g
    ##    <dbl> <date>       <dbl>        <dbl> <chr> <chr>             <dbl>    <dbl>
    ##  1  2003 2003-04-30       1           10 trap  Caesio cuning        36    1089.
    ##  2  2003 2003-04-30       1           10 trap  Caesio cuning        29     565.
    ##  3  2003 2003-04-30       1           10 trap  Caesio cuning        34     916.
    ##  4  2003 2003-04-30       1           10 trap  Caesio cuning        36    1089.
    ##  5  2003 2003-04-30       1           10 trap  Caesio cuning        34     916.
    ##  6  2003 2003-04-30       1           10 trap  Caesio cuning        28     508.
    ##  7  2003 2003-04-30       1           10 trap  Caesio cuning        30     627.
    ##  8  2003 2003-04-30       1           10 trap  Caesio cuning        27     455.
    ##  9  2003 2003-04-30       1           10 trap  Caesio cuning        33     837.
    ## 10  2003 2003-04-30       1           10 trap  Caesio cuning        35    1000.
    ## # ... with 7,198 more rows

Save clean data

``` r
write_csv(landings_data,"sample_landings_data_clean.csv")
```
