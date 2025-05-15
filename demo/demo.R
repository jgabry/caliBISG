install.packages("pak")
pak::pak("jgabry/caliBISG")

library(caliBISG)

# replace PATH argument with the full path to the directory where
# you have calibisg_vt2020.csv and calibisg_wa2020.csv
# this will eventually be unnecessary since we will host them online
set_temporary_local_directory(PATH)

# reads the CSVs (eventually will download them too)
download_data(states = c("VT", "WA"), years = 2020)

most_probable_race("smith", "wa", "king")
most_probable_race(
  name = c("Lopez", "Jackson"),
  state = c("VT", "WA"),
  county = c("Chittenden", "King")
)

race_probabilities("smith", "wa", "king")
race_probabilities("lopez", "vt", "chittenden")
probs2 <- race_probabilities(
  name = c("Lopez", "Smith"),
  state = c("VT", "WA"),
  county = c("Chittenden", "King")
)
print_comparison_tables(race_probabilities("smith", "wa", "king"))
print_comparison_tables(probs2, digits = 2)

