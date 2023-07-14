#' Label variable values
#'
#' @param x Vector
#' @param variable Denote which variable vector represents. If not changed from defaults, it will find out.
#' @param ... Other filters.
#'
#' @return Vector of labels
#' @export
#' @author Tino Good, \email{onit.good+labelr@gmail.com}
#'
#' @importFrom dplyr %>% do group_by distinct inner_join
label_var <- function(x, variable = NULL, ...) {

  # deparse variable name from x and search for labels in namespace:
  if (is.null(variable)) {
    variable <- deparse(expr = substitute(x))
  }

  stopifnot(labelr::labels$exists(variable))

  label_df <- labelr::labels$get(variable)

  dots <- list(...)

  # match list names with variables in labels:
  try_by <- intersect(names(dots), names(label_df))

  # if there are matches:
  filterfun <- function(.data, variable, value) {
    if (any(.data[[variable]] %in% value)) {
      return(.data[.data[[variable]] %in% value, ])
    } else {
      return(.data)
    }
  }
  label_df_sub <- label_df
  if (!is.null(try_by)) {
    for (nm in try_by) {
      label_df_sub <- label_df_sub %>% group_by(from) %>%
        do(filterfun(., variable = nm, value = dots[[nm]]))
    }
  }

  label_df <- inner_join(label_df, label_df_sub, by = intersect(names(label_df), names(label_df_sub)))
  label_df <- distinct(.data = label_df, from, .keep_all = TRUE)

  if (all(is.na(x))) {
    return(x)
  }

  # Option, nur benutzte Labels zu brauchen? Dann könnte man auch poolen... Bzw. müsste keine Unterscheidung gemacht werden...
  if (!any(as.character(x) %in% label_df$from) & !is.factor(x)) {
    msg <- paste("No match for variable", variable, "in the `from`, hence no labels. Do you have the correct variable?", sep = " ")
    stop(msg)
  }

  # if not all are in labels, fill missing with initial value an put at first place, because exceptions might be most important (custom/ad-hoc stuff)
  unique_x <- unique(as.character(sort(x)))
  if (is.factor(x)) {
    levels_x <- unique_x
  } else {
    unique_x_in_from <- as.character(label_df$from)[as.character(label_df$from) %in% unique_x]
    unique_x_not_in_from <- setdiff(unique_x, unique_x_in_from)
    levels_x <- c(unique_x_not_in_from, unique_x_in_from)
  }

  labels_x <- mapvalues(x = levels_x, from = as.character(label_df$from), to = as.character(label_df$to), missing_to_na = FALSE)

  return(factor(x = x, levels = levels_x, labels = labels_x))
}
