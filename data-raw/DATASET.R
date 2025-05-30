library(readr)
library(glue)

path_root <- "/Users/jgabry/Desktop/tmp/caliBISG-data/"
.race_x_surname_df <- as.data.frame(caliBISG:::.rename_data(read_csv(glue("{path_root}df_surnames.csv"))))
.race_x_usa_df_2020  <- caliBISG:::.rename_data(read_csv(glue("{path_root}usa_census_2020.csv")))


.race_x_county_list_2020 <- list()
missing_states <- c()
for (st in tolower(state.abb)) {
  if (file.exists(glue("{path_root}county_census_{st}2020.csv"))) {
    dat <- caliBISG:::.rename_data(read_csv(glue("{path_root}county_census_{st}2020.csv")))
    dat$county <- tolower(dat$county)
    .race_x_county_list_2020[[st]] <- dat[, c("county", "prob_aian", "prob_api", "prob_black_nh", "prob_hispanic",
                                        "prob_white_nh", "prob_other")]
  } else
    missing_states <- c(missing_states, st)
}
print(missing_states)

usethis::use_data(
  .race_x_surname_df,
  .race_x_usa_df_2020,
  .race_x_county_list_2020,
  internal = TRUE,
  overwrite = TRUE,
  compress = "xz"
)


