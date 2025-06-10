library(readr)
library(glue)
library(usethis)

path_root <- "/Users/jgabry/Desktop/tmp/caliBISG-data/"

csv_to_dataframe <- function(file_path) {
  as.data.frame(readr::read_csv(file_path, show_col_types = FALSE, progress = FALSE))
}
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
fix_spaces_in_county_name <- function(county, state) {
  orig <- county

  # replace any period not followed by a space with a period and a space
  # change cases like "st.lawrence" to "st. lawrence"
  step1 <- gsub("\\.(?! )", ". ", orig, perl = TRUE)

  # replace two or more consecutive spaces with a single space.
  modified <- gsub(" {2,}", " ", step1)

  # identify which entries actually changed
  changed_idx <- which(orig != modified & !is.na(orig))

  # print table of changes, if any
  if (length(changed_idx) > 0) {
    changes_df <- data.frame(
      original = orig[changed_idx],
      modified = modified[changed_idx],
      stringsAsFactors = FALSE
    )
    counts <- aggregate(
      x = changes_df$original,
      by = list(original = changes_df$original, modified = changes_df$modified),
      FUN = length
    )
    names(counts)[3] <- "count"
    message(glue("Changes made for state {state}:"))
    print(counts, row.names = FALSE)
  }

  modified
}

# For Florida only we need to convert abbreviations to full county names
fix_florida_county_names <- function(county) {
  florida_counties <- c(ALA = "Alachua", BAK = "Baker", BAY = "Bay", BRA = "Bradford",
                        BRE = "Brevard", BRO = "Broward", CAL = "Calhoun", CHA = "Charlotte",
                        CIT = "Citrus", CLA = "Clay", CLL = "Collier", CLM = "Columbia",
                        DAD = "Miami-Dade", DES = "Desoto", DIX = "Dixie", DUV = "Duval",
                        ESC = "Escambia", FLA = "Flagler", FRA = "Franklin", GAD = "Gadsden",
                        GIL = "Gilchrist", GLA = "Glades", GUL = "Gulf", HAM = "Hamilton",
                        HAR = "Hardee", HEN = "Hendry", HER = "Hernando", HIG = "Highlands",
                        HIL = "Hillsborough", HOL = "Holmes", IND = "Indian River", JAC = "Jackson",
                        JEF = "Jefferson", LAF = "Lafayette", LAK = "Lake", LEE = "Lee",
                        LEO = "Leon", LEV = "Levy", LIB = "Liberty", MAD = "Madison",
                        MAN = "Manatee", MRN = "Marion", MRT = "Martin", MON = "Monroe",
                        NAS = "Nassau", OKA = "Okaloosa", OKE = "Okeechobee", ORA = "Orange",
                        OSC = "Osceola", PAL = "Palm Beach", PAS = "Pasco", PIN = "Pinellas",
                        POL = "Polk", PUT = "Putnam", SAN = "Santa Rosa", SAR = "Sarasota",
                        SEM = "Seminole", STJ = "St. Johns", STL = "St. Lucie", SUM = "Sumter",
                        SUW = "Suwannee", TAY = "Taylor", UNI = "Union", VOL = "Volusia",
                        WAK = "Wakulla", WAL = "Walton", WAS = "Washington")
  unname(florida_counties[county])
}


# internal package data ---------------------------------------------------

.race_x_surname_df <- rename_data(csv_to_dataframe(glue("{path_root}df_surnames.csv")))
.race_x_usa_df_2020  <- rename_data(csv_to_dataframe(glue("{path_root}usa_census_2020.csv")))
.race_x_county_list_2020 <- list()

missing_county_data <- c()
for (st in tolower(state.abb)) {
  if (file.exists(glue("{path_root}county_census_{st}2020.csv"))) {
    dat <- rename_data(csv_to_dataframe(glue("{path_root}county_census_{st}2020.csv")))
    if (st == "fl") {
      dat$county <- fix_florida_county_names(dat$county)
    }
    dat$county <- tolower(dat$county)
    dat$county <- fix_spaces_in_county_name(dat$county, st)
    dat$state <- toupper(st)
    .race_x_county_list_2020[[st]] <- dat[, c(
      "state",
      "county",
      "prob_aian",
      "prob_api",
      "prob_black_nh",
      "prob_hispanic",
      "prob_white_nh",
      "prob_other"
    )]
  } else
    missing_county_data <- c(missing_county_data, st)
}
if (length(missing_county_data)) {
  stop(glue("Missing county data for states: {paste(missing_county_data, collapse = ', ')}"))
}

use_data(
  .race_x_surname_df,
  .race_x_usa_df_2020,
  .race_x_county_list_2020,
  internal = TRUE,
  overwrite = TRUE,
  compress = "xz"
)



# caliBISG files to upload as GitHub release assets  ---------------------------

for (st in tolower(caliBISG:::.all_calibisg_states())) {
  for (yr in caliBISG:::.all_calibisg_years()) {
    dat <- csv_to_dataframe(glue("{path_root}calibisg_{st}{yr}.csv"))
    dat <- as.data.frame(dat)
    if (st == "fl") {
      dat$county <- fix_florida_county_names(dat$county)
    }
    dat$year <- yr
    dat$state <- toupper(st)
    dat$county <- tolower(dat$county)
    dat$county <- fix_spaces_in_county_name(dat$county, st)
    dat$name <- tolower(dat$name)
    dat <- rename_data(dat)
    dat <- reorder_data(dat)
    dat <- unique(dat)
    readr::write_csv(
      dat,
      file = glue("{path_root}/to-upload/calibisg_{st}{yr}.csv"),
      na = "NA",
      col_names = TRUE
    )
  }
}




