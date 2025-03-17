.onAttach <- function(...) {
  ver <- utils::packageVersion("caliBISG")
  packageStartupMessage("This is caliBISG version ", ver)
}

.internal_data_env <- new.env(parent = emptyenv())
