library(dplyr)

# library(magick)
# library(imager)
# library(scales)

# get_colorpal <- function(im, n = 8, cs = "RGB") {
#   #print(cs)
#   tmp <- im %>%
#     image_resize("100") %>%
#     image_quantize(max = n, colorspace = cs) %>%
#     magick2cimg() %>%
#     RGBtoHSV() %>%
#     as.data.frame(wide = "c") %>%
#     mutate(
#       hex = hsv(rescale(c.1, from = c(0, 360)), c.2, c.3),
#       hue = c.1,
#       sat = c.2,
#       value = c.3
#     ) %>%
#     count(hex, hue, sat, value, sort = TRUE) %>%
#     mutate(colorspace = cs)
#   return(select(tmp, colorspace, hex, hue, sat, value, n))
# }

# res <- bind_rows(lapply(mars_rover$img_src, function(x) {
#   a <- image_read(x)
#   tibble(
#     img_src = x,
#     select(image_info(a), -colorspace),
#     head(get_colorpal(a), 1),
#   )
# }))

# readr::write_rds(res, "_ignore//mars/stats.rds")

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

d2 <- readr::read_rds("_ignore/mars/stats.rds")

res <- right_join(d, x, by = "substr")
res <- left_join(res, d2, by = "img_src")

mars_rover <- res %>%
  filter(!is.na(id)) %>%
  rename(camera = "camera_full_name") %>%
  mutate(earth_date = as.Date(earth_date))

mars_rover <- mars_rover[, c("id", "camera", "sol", "earth_date", "class",
  "width", "height", "filesize", "hex", "hue", "img_src")]

use_data(mars_rover, overwrite = TRUE)
remotes::install_github("trelliscope/trelliscope@new-ui")