# download flag images to illustrate local image panel...
flgs <- paste0("https://raw.githubusercontent.com/hafen/countryflags/master/png/512/",
  unique(gap$iso_alpha2), ".png")
flgdir <- tempfile(pattern = "flags")
dir.create(flgdir)
lapply(flgs, function(x) download.file(x, file.path(flgdir, basename(x))))

system2("open", flgdir)
