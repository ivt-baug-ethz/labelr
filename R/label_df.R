#' Label data frame
#'
#' Use this to label your final data frame before you plot it.
#'
#' @param df A data frame
#' @param ... Pass further arguments to \code{label_var()}.
#'
#' @return Labelled data frame
#' @export
#' @author Tino Good, \email{onit.good+labelr@gmail.com}
label_df <- function(df, ...) {
  # should order df and label all relevant columns
  labels_available <- names(labelr::labels$get())
  vars_in_df <- names(df)


  vars_to_label <- vars_in_df[vars_in_df %in% labels_available]

  for (column_name in vars_to_label) {
    df[, column_name] <- label_var(x = df[[column_name]], variable = column_name, ...)
  }

  return(df)
}
