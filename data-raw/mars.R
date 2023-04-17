library(dplyr)

cls <- readr::read_fwf(
  "_ignore/mars/msl-images/msl_synset_words-indexed.txt") %>%
  rename(classn = "X1", class = "X2")

x <- readr::read_delim("_ignore/mars/msl-images/test-calibrated-shuffled.txt",
  col_names = FALSE) %>%
  rename(src = "X1", classn = "X2") %>%
  mutate(substr = gsub(".*/(.*)_.*", "\\1", src)) %>%
  left_join(cls, by = "classn")

d <- readr::read_rds("_ignore/mars/all.rds") %>%
  mutate(substr = gsub(".*/(.*)_.*", "\\1", img_src))

res <- right_join(d, x, by = "substr")

mars_rover <- res %>%
  filter(!is.na(id)) %>%
  rename(camera = "camera_full_name")

mars_rover <- mars_rover[, c("camera", "sol", "earth_date", "class", "img_src")]

use_data(mars_rover, overwrite = TRUE)
