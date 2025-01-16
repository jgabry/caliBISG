.onAttach <- function(...) {
  ver <- utils::packageVersion("pisg")
  packageStartupMessage("This is pisg version ", ver)
}

.internal_data_env <- new.env(parent = emptyenv())
