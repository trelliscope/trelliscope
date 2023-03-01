suppressPackageStartupMessages({
  library(trelliscope, warn.conflicts = FALSE)
  library(ggplot2, warn.conflicts = FALSE)
  library(tidyr, warn.conflicts = FALSE)
  library(dplyr, warn.conflicts = FALSE)
  library(tidyr, warn.conflicts = FALSE)
})

get_trobj <- function(x) attr(x, "trelliscope")

test_that2 <- function(desc, code) {
  invisible(cli::test_that_cli(desc, configs = "plain", code = code))
}
