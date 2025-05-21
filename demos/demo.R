# install from github
install.packages("pak")
pak::pak("jgabry/caliBISG")


library(caliBISG)

# replace PATH argument with the full path to the directory where
# you have calibisg_vt2020.csv and calibisg_wa2020.csv
# this will eventually be unnecessary since we will host them online
set_temporary_local_directory(PATH)

# reads the CSVs (eventually will download them too)
download_data(states = c("VT", "WA"), years = 2020)

# get most probable race according to calibisg and bisg
most_probable_race("smith", "wa", "king", year = 2020)

# can handle vectors of names, states, and counties
# year defaults to 2020 since that's the only one we have right now
most_probable_race(
  name = c("Lopez", "Jackson"),
  state = c("VT", "WA"),
  county = c("Chittenden", "King")
)

# if we don't have calibisg_race we can still get bisg_race by using the "all
# other names" distribution as long as we have a valid state/county/year
most_probable_race("no_name", "WA", "King")


# get the actual probabilities, not just most probable
# the print method prints comparison tables of calibisg vs bisg
race_probabilities("smith", "wa", "king")

probs1 <- race_probabilities("lopez", "vt", "chittenden")
print(probs1, digits = 4)

probs2 <- race_probabilities(
  name = c("Lopez", "Smith"),
  state = c("VT", "WA"),
  county = c("Chittenden", "King")
)
str(probs2) # still just a data frame but with subclass "compare_bisg"
print(probs2)



