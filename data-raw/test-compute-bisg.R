# R version of original Python code
# writes the precomputed BISG values to data-raw/precomputed-bisg.R
# these values were used to create the snapshot for testthat in tests/testthat/test-bisg.R

library(tidyverse)
library(glue)

path_root <- "/Users/jgabry/Desktop/tmp/caliBISG-data/bisg-test/"
df_agg <- read_csv(glue("{path_root}wa_2020.csv"))
df_cen_surs <- read_csv(glue("{path_root}df_surnames.csv"))
df_cen_counties  <- read_csv(glue("{path_root}county_census_wa2020.csv"))
df_cen_usa  <- read_csv(glue("{path_root}usa_census_2020.csv"))

CEN_R_GIVEN_SUR_COLS <- c(
  "cen_r_given_sur_nh_aian",  "cen_r_given_sur_nh_api",
  "cen_r_given_sur_nh_black", "cen_r_given_sur_hispanic",
  "cen_r_given_sur_nh_white", "cen_r_given_sur_other"
)

PROB_COLS <- c(
  "prob_nh_aian", "prob_nh_api", "prob_nh_black",
  "prob_hispanic", "prob_nh_white", "prob_other"
)

BISG_CEN_COUNTY_COLS <- c(
  "bisg_cen_county_nh_aian", "bisg_cen_county_nh_api", "bisg_cen_county_nh_black",
  "bisg_cen_county_hispanic", "bisg_cen_county_nh_white", "bisg_cen_county_other"
)

# draw 100 random (surname, county) pairs
set.seed(1001)
df_rows <- df_agg %>% slice_sample(n = 100)
df_name_county  <- df_rows %>% select(name, county)

# merge surname & county factors
df <- df_name_county %>%
  ## surname × race: pull the six columns that begin cen_r_given...
  left_join(select(df_cen_surs, name, all_of(CEN_R_GIVEN_SUR_COLS)), by = "name") %>%
  ## indicator: did we find this surname in the Census surname list?
  ## if not, overwrite with the “all other names” distribution
  mutate(in_cen_surs = name %in% df_cen_surs$name)  %>%
  {
    if (any(!.$in_cen_surs)) {
      other_vals <- df_cen_surs %>%
        filter(name == "all other names") %>%
        select(all_of(CEN_R_GIVEN_SUR_COLS)) %>%
        slice(1) # a 1-row tibble

      rows_bad <- which(!.$in_cen_surs)
      for (col in CEN_R_GIVEN_SUR_COLS) {
        .[rows_bad, col] <- other_vals[[col]]
      }
    }
    .
  } %>%
  ## county × race
  left_join(select(df_cen_counties, county, all_of(PROB_COLS)),
            by = "county")

# BISG calculation
bisg_mat <- as.matrix(df[ , CEN_R_GIVEN_SUR_COLS]) * as.matrix(df[ , PROB_COLS])
bisg_mat <- sweep(bisg_mat,
                  MARGIN = 2,
                  as.numeric(df_cen_usa[1, PROB_COLS]),
                  FUN = "/")
bisg_mat <- bisg_mat / rowSums(bisg_mat)
df[ , BISG_CEN_COUNTY_COLS] <- bisg_mat

# check max absolute error
error <- max(abs(
  as.matrix(df[, BISG_CEN_COUNTY_COLS]) -
  as.matrix(df_rows[ , BISG_CEN_COUNTY_COLS])
))

cat(sprintf("max error: %.4g\n", error))

df_rows$state <- "WA"
df_rows$year <- 2020
result <- df_rows[ , c("name", "year", "state", "county", BISG_CEN_COUNTY_COLS)]

# rename columns to match names that package uses
result <- result %>%
  rename(
    bisg_aian = bisg_cen_county_nh_aian,
    bisg_api = bisg_cen_county_nh_api,
    bisg_black_nh = bisg_cen_county_nh_black,
    bisg_hispanic = bisg_cen_county_hispanic,
    bisg_white_nh = bisg_cen_county_nh_white,
    bisg_other = bisg_cen_county_other
  )
dput(as.data.frame(result), file = "data-raw/precomputed-bisg.R")
rm(list = ls())

