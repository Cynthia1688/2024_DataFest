Main Data Analysis
================
Yangyang Chen
2024-01-11

## Importing dataframe

``` r
load("~/Desktop/2024_DataFest/main_data/nhanes_data.rda")
main_df = nhanes_data
df_99 = 
  readxl::read_xlsx("Dietary_data/1999-2000daysmean.xlsx") |> 
  rename(MEAN_FIBER = DR_MEAN) |> 
  select(SEQN, MEAN_FIBER, YEAR)
df_01 = 
  readxl::read_xlsx("Dietary_data/2001-2002daysmean.xlsx") |> 
  rename(MEAN_FIBER = DR_MEAN) |> 
  select(SEQN, MEAN_FIBER, YEAR)
df_03 = 
  readxl::read_xlsx("Dietary_data/2003-2004daysmean.xlsx") |> 
  rename(MEAN_FIBER = DR_MEAN) |> 
  select(SEQN, MEAN_FIBER, YEAR)
df_05 = 
  readxl::read_xlsx("Dietary_data/2005-2006daysmean.xlsx") |> 
  rename(MEAN_FIBER = DR_MEAN) |> 
  select(SEQN, MEAN_FIBER, YEAR)
df_07 = 
  readxl::read_xlsx("Dietary_data/2007-2008daysmean.xlsx") |> 
  rename(MEAN_FIBER = DR_MEAN) |> 
  select(SEQN, MEAN_FIBER, YEAR)
df_09 = 
  readxl::read_xlsx("Dietary_data/2009-2010daysmean.xlsx") |> 
  rename(MEAN_FIBER = DR_MEAN) |> 
  select(SEQN, MEAN_FIBER, YEAR)
df_11 = 
  readxl::read_xlsx("Dietary_data/2011-2012daysmean.xlsx") |> 
  rename(MEAN_FIBER = DR_MEAN) |> 
  select(SEQN, MEAN_FIBER, YEAR)
df_13 = 
  readxl::read_xlsx("Dietary_data/2013-2014daysmean.xlsx") |> 
  rename(MEAN_FIBER = DR_MEAN) |> 
  select(SEQN, MEAN_FIBER, YEAR)
df_15 = 
  readxl::read_xlsx("Dietary_data/2015-2016daysmean.xlsx") |> 
  rename(MEAN_FIBER = DR_MEAN) |> 
  select(SEQN, MEAN_FIBER, YEAR)
df_17 = 
  readxl::read_xlsx("Dietary_data/2017-2020daysmean.xlsx") |> 
  rename(MEAN_FIBER = DR_MEAN) |> 
  select(SEQN, MEAN_FIBER, YEAR)
```

## Combining Fiber Data

``` r
fiber_df =   
  df_99 |> 
  rbind(df_01) |> 
  rbind(df_03) |> 
  rbind(df_05) |> 
  rbind(df_07) |> 
  rbind(df_09) |> 
  rbind(df_11) |> 
  rbind(df_13) |> 
  rbind(df_15) |> 
  rbind(df_17)
```

## Data Cleaning

``` r
fiber_tidy_df = 
  fiber_df |> 
  janitor::clean_names() |> 
  rename(
    svy_id = seqn,
    svy_year = year
         ) 

main_df =
  main_df |> 
  filter(demo_gender == "Women")
```

## Combining main data and fiber data, and remove NA

``` r
df = 
  left_join(fiber_tidy_df, main_df) |> 
  drop_na()
```

    ## Joining with `by = join_by(svy_id, svy_year)`
