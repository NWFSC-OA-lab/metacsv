## code to prepare `attrib_lists` dataset goes here

gen_attrib <- utils::read.csv("data-raw/gen_attrib_1.csv", stringsAsFactors = FALSE)
var_attrib <- utils::read.csv("data-raw/var_attrib_1.csv", stringsAsFactors = FALSE)

usethis::use_data(gen_attrib, var_attrib, internal = TRUE, overwrite = TRUE)
