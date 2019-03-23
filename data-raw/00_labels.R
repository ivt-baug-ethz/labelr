# this is the way to use it in another package to set defaults:
# label_list <- sapply(
#   X = readxl::excel_sheets(path = "data-raw/labels.xlsx"),
#   FUN = function (x) {
#     readxl::read_excel(path = "data-raw/labels.xlsx", sheet = x)
#   },
#   simplify = FALSE,
#   USE.NAMES = TRUE
# )

labels_list <- list()

# set default labels:
labels <- new_defaults(labels_list)


# save labels:
usethis::use_data(
  labels,
  overwrite = T
)
