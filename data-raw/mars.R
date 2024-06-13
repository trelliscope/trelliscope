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

rover_icon_b64 <- "data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPCFET0NUWVBFIHN2ZyAgUFVCTElDICctLy9XM0MvL0RURCBTVkcgMS4xLy9FTicgICdodHRwOi8vd3d3LnczLm9yZy9HcmFwaGljcy9TVkcvMS4xL0RURC9zdmcxMS5kdGQnPgo8c3ZnIGNsaXAtcnVsZT0iZXZlbm9kZCIgZmlsbC1ydWxlPSJldmVub2RkIiBzdHJva2UtbGluZWpvaW49InJvdW5kIiBzdHJva2UtbWl0ZXJsaW1pdD0iMiIgdmVyc2lvbj0iMS4xIiB2aWV3Qm94PSIwIDAgMjUgMjQiIHhtbDpzcGFjZT0icHJlc2VydmUiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+CiAgICA8ZyB0cmFuc2Zvcm09InRyYW5zbGF0ZSgwIC0uMDAwNjUyODYpIj4KICAgICAgICA8cmVjdCB5PSIuMDAxIiB3aWR0aD0iMjQiIGhlaWdodD0iMjMuOTk5IiBmaWxsPSJub25lIi8+CiAgICAgICAgPGNsaXBQYXRoIGlkPSJhIj4KICAgICAgICAgICAgPHJlY3QgeT0iLjAwMSIgd2lkdGg9IjI0IiBoZWlnaHQ9IjIzLjk5OSIvPgogICAgICAgIDwvY2xpcFBhdGg+CiAgICAgICAgPGcgY2xpcC1wYXRoPSJ1cmwoI2EpIj4KICAgICAgICAgICAgPGcgdHJhbnNmb3JtPSJ0cmFuc2xhdGUoMCAuMDAwNjUyODYpIj4KICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgIDxwYXRoIGQ9Im0xNC41IDE2YzAuMjc2IDAgMC41LTAuMjI0IDAuNS0wLjV2LTFjMC0wLjI3Ni0wLjIyNC0wLjUtMC41LTAuNXMtMC41IDAuMjI0LTAuNSAwLjV2MWMwIDAuMjc2IDAuMjI0IDAuNSAwLjUgMC41em05LjM1NC0zLjg1NC00LTRjLTAuMDk0LTAuMDkzLTAuMjIxLTAuMTQ2LTAuMzU0LTAuMTQ2aC0xMGMtMC4yMDIgMC0wLjM4NSAwLjEyMi0wLjQ2MiAwLjMwOS0wLjA3OCAwLjE4Ny0wLjAzNSAwLjQwMiAwLjEwOCAwLjU0NWwyLjE0NiAyLjE0NmgtNC4yOTJ2LTdoMS41YzAuMjc2IDAgMC41LTAuMjI0IDAuNS0wLjV2LTJjMC0wLjI0NC0wLjE3Ny0wLjQ1My0wLjQxOC0wLjQ5M2wtNi0xYy0wLjE0My0wLjAyMy0wLjI5MyAwLjAxNy0wLjQwNSAwLjExMi0wLjExMyAwLjA5NC0wLjE3NyAwLjIzNC0wLjE3NyAwLjM4MXYzYzAgMC4yNzYgMC4yMjQgMC41IDAuNSAwLjVoMy41djdoLTIuNWMtMC44MjcgMC0xLjUgMC42NzMtMS41IDEuNXY0YzAgMC44MjcgMC42NzMgMS41IDEuNSAxLjVoMy43OTNsLTIuNTAzIDIuNTAzYy0wLjM4Ny0wLjg4My0xLjI2Ny0xLjUwMy0yLjI5LTEuNTAzLTEuMzc4IDAtMi41IDEuMTIyLTIuNSAyLjVzMS4xMjIgMi41IDIuNSAyLjVjMS4yOTkgMCAyLjM1OC0xLjAwMSAyLjQ3Ny0yLjI3bDMuNzMtMy43M2gyLjI5M3YxLjA1MWMtMS4xNCAwLjIzMi0yIDEuMjQyLTIgMi40NDkgMCAxLjM3OCAxLjEyMiAyLjUgMi41IDIuNXMyLjUtMS4xMjIgMi41LTIuNWMwLTEuMjA3LTAuODYxLTIuMjE3LTItMi40NDl2LTEuMDUxaDIuMjkybDMuNzMxIDMuNzMxYzAuMTE5IDEuMjY4IDEuMTc4IDIuMjY5IDIuNDc3IDIuMjY5IDEuMzc4IDAgMi41LTEuMTIyIDIuNS0yLjVzLTEuMTIyLTIuNS0yLjUtMi41Yy0xLjAyNCAwLTEuOTA0IDAuNjIxLTIuMjkgMS41MDRsLTIuNTA0LTIuNTA0aDMuNzk0YzAuODI3IDAgMS41LTAuNjczIDEuNS0xLjV2LTMuNWgyLjVjMC4yMDIgMCAwLjM4NS0wLjEyMiAwLjQ2Mi0wLjMwOSAwLjA3OC0wLjE4NyAwLjAzNS0wLjQwMi0wLjEwOC0wLjU0NXptLTE5Ljg1NCA5LjM1NWMtMWUtMyAwLjgyNi0wLjY3NCAxLjQ5OS0xLjUgMS40OTktMC44MjcgMC0xLjUtMC42NzMtMS41LTEuNXMwLjY3My0xLjUgMS41LTEuNSAxLjUgMC42NzMgMS41IDEuNXYxZS0zem0xNi41LTEuNTAxYzAuODI3IDAgMS41IDAuNjczIDEuNSAxLjVzLTAuNjczIDEuNS0xLjUgMS41LTEuNS0wLjY3My0xLjUtMS41IDAuNjczLTEuNSAxLjUtMS41em0tOS43OTMtMTFoMy41ODVsMyAzaC0zLjU4NWwtMy0zem0tNy43MDctNnYtMS45MWw1IDAuODMzdjEuMDc3aC01em0xMCAxOC41YzAgMC44MjctMC42NzMgMS41LTEuNSAxLjVzLTEuNS0wLjY3My0xLjUtMS41IDAuNjczLTEuNSAxLjUtMS41IDEuNSAwLjY3MyAxLjUgMS41em03LTVjMCAwLjI3Ni0wLjIyNCAwLjUtMC41IDAuNWgtMTZjLTAuMjc2IDAtMC41LTAuMjI0LTAuNS0wLjV2LTRjMC0wLjI3NiAwLjIyNC0wLjUgMC41LTAuNWg4Ljc5MmwwLjg1NCAwLjg1NGMwLjA5NCAwLjA5MyAwLjIyMSAwLjE0NiAwLjM1NCAwLjE0Nmg2LjV2My41em0tMS4yOTQtNC41LTMtM2gzLjU4N2wzIDNoLTMuNTg3em0tMi4yMDYgNGMwLjI3NiAwIDAuNS0wLjIyNCAwLjUtMC41di0xYzAtMC4yNzYtMC4yMjQtMC41LTAuNS0wLjVzLTAuNSAwLjIyNC0wLjUgMC41djFjMCAwLjI3NiAwLjIyNCAwLjUgMC41IDAuNXptLTktM2gtM2MtMC4yNzYgMC0wLjUgMC4yMjQtMC41IDAuNXYyYzAgMC4yNzYgMC4yMjQgMC41IDAuNSAwLjVoM2MwLjI3NiAwIDAuNS0wLjIyNCAwLjUtMC41di0yYzAtMC4yNzYtMC4yMjQtMC41LTAuNS0wLjV6bS0wLjUgMmgtMnYtMWgydjF6bTExLjUgMWMwLjI3NiAwIDAuNS0wLjIyNCAwLjUtMC41di0xYzAtMC4yNzYtMC4yMjQtMC41LTAuNS0wLjVzLTAuNSAwLjIyNC0wLjUgMC41djFjMCAwLjI3NiAwLjIyNCAwLjUgMC41IDAuNXoiIGZpbGw9IiM3NTc1NzUiIGZpbGwtcnVsZT0ibm9uemVybyIvPgogICAgICAgICAgICAgICAgCiAgICAgICAgICAgIDwvZz4KICAgICAgICA8L2c+CiAgICA8L2c+Cjwvc3ZnPgo="
use_data(rover_icon_b64, overwrite = TRUE)
