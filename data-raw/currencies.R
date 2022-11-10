# https://www.six-group.com/en/products-services/financial-information/data-standards.html#scrollTo=maintenance-agency
# List One (XLS)
currencies <- readr::read_csv("extdata/currencies.csv")
currencies[[6]] <- NULL
currencies <- filter(currencies, !is.na(code_alpha))

valid_currencies <- unique(currencies$code_alpha)

usethis::use_data(currencies, overwrite = TRUE)


# https://www.w3schools.com/tags/ref_language_codes.asp
# https://www.w3schools.com/tags/ref_country_codes.asp
# https://medium.com/@samanthaming/format-currency-in-es6-with-intl-numberformat-f07e9b6321f9
