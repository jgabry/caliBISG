library(readr)
library(glue)

path_root <- "/Users/jgabry/Desktop/tmp/voter_bisg/"
.race_x_surname <- as.data.frame(caliBISG:::.rename_data(read_csv(glue("{path_root}df_surnames.csv"))))
.race_x_usa_2020  <- caliBISG:::.rename_data(read_csv(glue("{path_root}usa_census_2020.csv")))
.race_x_county_wa_2020  <- caliBISG:::.rename_data(read_csv(glue("{path_root}county_census_wa2020.csv")))
.race_x_county_vt_2020  <- caliBISG:::.rename_data(read_csv(glue("{path_root}county_census_vt2020.csv")))

usethis::use_data(
  .race_x_surname,
  .race_x_usa_2020,
  .race_x_county_wa_2020,
  .race_x_county_vt_2020,
  internal = TRUE,
  overwrite = TRUE,
  compress = "xz"
)


