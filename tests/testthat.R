# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

suppressPackageStartupMessages({
  library(testthat, warn.conflicts = FALSE)
  library(trelliscope, warn.conflicts = FALSE)
  library(ggplot2, warn.conflicts = FALSE)
  library(tidyr, warn.conflicts = FALSE)
  library(dplyr, warn.conflicts = FALSE)
})

test_that2 <- function(desc, code) {
  invisible(cli::test_that_cli(desc, configs = "plain", code = code))
}

test_check("trelliscope")
