################
# code for metadata template options - package internal data
###################
#Code to create the general metadata attribute lists

gen_format_1 <- c("file_name", "project_name", "creation_date",
                   "description", "created_by")

gen_format_2 <- c(gen_format_1, "data_owner", "version")


# TODO add more general format options

gen_attrib <- list(gen_format_1 = gen_format_1,
                   gen_format_2 = gen_format_2)

#########################3
#Code to create the variable metadata attribute lists
var_format_1 <- c("var_name", "units", "description")

var_format_2 <- c(var_format_1, "format", "min", "max")

# TODO add more variable format options

var_attrib <- list(var_format_1 = var_format_1,
                   var_format_2 = var_format_2)

usethis::use_data(gen_attrib, var_attrib,
                  internal = TRUE, overwrite = TRUE)

