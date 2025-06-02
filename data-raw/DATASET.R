library(readr)
library(glue)

path_root <- "/Users/jgabry/Desktop/tmp/caliBISG-data/"


rename_data <- function(data) {
  colnames(data) <- gsub("nh_aian", "aian", colnames(data))
  colnames(data) <- gsub("nh_api", "api", colnames(data))
  colnames(data) <- gsub("nh_black", "black_nh", colnames(data))
  colnames(data) <- gsub("nh_white", "white_nh", colnames(data))
  colnames(data) <- gsub("in_cen_surs", "in_census", colnames(data))
  data
}
reorder_data <- function(data) {
  col_order <- c(
    .demographic_columns(),
    .calibisg_columns(),
    "in_census"
  )
  data[, col_order]
}

# For Florida only we need to convert abbreviations to full county names using
# the following:
florida_counties <- c(
  ALA = "Alachua",
  BAK = "Baker",
  BAY = "Bay",
  BRA = "Bradford",
  BRE = "Brevard",
  BRO = "Broward",
  CAL = "Calhoun",
  CHA = "Charlotte",
  CIT = "Citrus",
  CLA = "Clay",
  CLL = "Collier",
  CLM = "Columbia",
  DAD = "Miami-Dade",
  DES = "Desoto",
  DIX = "Dixie",
  DUV = "Duval",
  ESC = "Escambia",
  FLA = "Flagler",
  FRA = "Franklin",
  GAD = "Gadsden",
  GIL = "Gilchrist",
  GLA = "Glades",
  GUL = "Gulf",
  HAM = "Hamilton",
  HAR = "Hardee",
  HEN = "Hendry",
  HER = "Hernando",
  HIG = "Highlands",
  HIL = "Hillsborough",
  HOL = "Holmes",
  IND = "Indian River",
  JAC = "Jackson",
  JEF = "Jefferson",
  LAF = "Lafayette",
  LAK = "Lake",
  LEE = "Lee",
  LEO = "Leon",
  LEV = "Levy",
  LIB = "Liberty",
  MAD = "Madison",
  MAN = "Manatee",
  MRN = "Marion",
  MRT = "Martin",
  MON = "Monroe",
  NAS = "Nassau",
  OKA = "Okaloosa",
  OKE = "Okeechobee",
  ORA = "Orange",
  OSC = "Osceola",
  PAL = "Palm Beach",
  PAS = "Pasco",
  PIN = "Pinellas",
  POL = "Polk",
  PUT = "Putnam",
  SAN = "Santa Rosa",
  SAR = "Sarasota",
  SEM = "Seminole",
  STJ = "St. Johns",
  STL = "St. Lucie",
  SUM = "Sumter",
  SUW = "Suwannee",
  TAY = "Taylor",
  UNI = "Union",
  VOL = "Volusia",
  WAK = "Wakulla",
  WAL = "Walton",
  WAS = "Washington"
)


# internal package data ---------------------------------------------------

.race_x_county_list_2020 <- list()
missing_states <- c()
for (st in tolower(state.abb)) {
  if (file.exists(glue("{path_root}county_census_{st}2020.csv"))) {
    dat <- rename_data(read_csv(glue("{path_root}county_census_{st}2020.csv")))
    if (st == "fl") {
      dat$county <- unname(florida_counties[dat$county])
    }
    dat$county <- tolower(dat$county)
    .race_x_county_list_2020[[st]] <- dat[, c("county", "prob_aian", "prob_api", "prob_black_nh", "prob_hispanic",
                                        "prob_white_nh", "prob_other")]
  } else
    missing_states <- c(missing_states, st)
}
print(missing_states)

.race_x_surname_df <- as.data.frame(rename_data(read_csv(glue("{path_root}df_surnames.csv"))))
.race_x_usa_df_2020  <- rename_data(read_csv(glue("{path_root}usa_census_2020.csv")))

usethis::use_data(
  .race_x_surname_df,
  .race_x_usa_df_2020,
  .race_x_county_list_2020,
  internal = TRUE,
  overwrite = TRUE,
  compress = "xz"
)



# caliBISG files to upload as release assets  -----------------------------

for (st in tolower(caliBISG:::.all_calibisg_states())) {
  for (yr in caliBISG:::.all_calibisg_years()) {
    dat <- read_csv(glue("{path_root}calibisg_{st}{yr}.csv"))
    dat <- as.data.frame(dat)
    if (st == "fl") {
      dat$county <- unname(florida_counties[dat$county])
    }
    dat$year <- yr
    dat$state <- toupper(st)
    dat$county <- tolower(dat$county)
    dat$name <- tolower(dat$name)
    dat <- rename_data(dat)
    dat <- reorder_data(dat)
    readr::write_csv(
      dat,
      file = glue("{path_root}/to-upload/calibisg-{st}-{yr}.csv"),
      na = "NA",
      col_names = TRUE
    )
  }
}



