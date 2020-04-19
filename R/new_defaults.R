#' New label defaults
#'
#' @param value Named list of data frames with at least \code{from} and \code{to} values (columns).
#'
#' @return Label-container.
#' @export
#'
#' @author Yihui Xie (borrowed from knitr)
new_defaults <- function(value = list()) {

  defaults <- value

  exists <- function(name) {
    any(name %in% names(defaults))
  }

  get = function(name, default = FALSE, drop = TRUE) {

    if (default) defaults = value  # this is only a local version

    if (missing(name)) defaults else {

      if (drop && length(name) == 1) defaults[[name]] else {

        setNames(defaults[name], name)

      }

    }

  }

  resolve = function(...) {

    dots = list(...)

    if (length(dots) == 0) return()

    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))

      if (length(dots <- dots[[1]]) == 0) return()

    dots

  }

  set = function(...) {

    dots = resolve(...)

    lapply(dots, test_input)

    if (length(dots)) defaults <<- merge(dots)

    invisible(NULL)

  }

  merge = function(values) merge_list(defaults, values)

  restore = function(target = value) defaults <<- target

  append = function(...) {

    dots = resolve(...)

    tmp <- defaults

    for (i in names(dots)) tmp[[i]] <- rbind(dots[[i]], defaults[[i]])

    if (length(dots)) defaults <<- tmp

    invisible(NULL)

  }

  test_input <- function(x) {
    if (is.data.frame(x)) {
      stopifnot(all(c("from", "to") %in% names(x)))
    }
  }

  merge_list <- function (x, y)
  {
    x[names(y)] = y
    x
  }

  list(get = get, set = set, append = append, merge = merge, restore = restore, exists = exists)

}
