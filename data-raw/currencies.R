# https://www.six-group.com/en/products-services/financial-information/data-standards.html#scrollTo=maintenance-agency
# List One (XLS)
currencies <- readr::read_csv("extdata/currencies.csv")
currencies[[6]] <- NULL
currencies <- filter(currencies, !is.na(code_alpha))

valid_currencies <- unique(currencies$code_alpha)

tmp <- jsonlite::read_json("https://gist.githubusercontent.com/manishtiwari25/d3984385b1cb200b98bcde6902671599/raw/2bd8040867dc8d9f09fa84e6acfc9ce5dc163ab9/world_currency_symbols.json", simplifyVector = TRUE) |>
  rename_all(tolower)

tmp2 <- left_join(currencies, select(tmp, code, symbol),
  by = c("code_alpha" = "code"))


usethis::use_data(currencies, overwrite = TRUE)


# https://www.w3schools.com/tags/ref_language_codes.asp
# https://www.w3schools.com/tags/ref_country_codes.asp
# https://medium.com/@samanthaming/format-currency-in-es6-with-intl-numberformat-f07e9b6321f9
