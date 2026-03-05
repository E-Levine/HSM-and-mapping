#' object_size_check
#' Returns a data frame of the largest objects (by memory size) found in
#' the specified environment.
#'
#' @param n Integer. Number of largest objects to return. Default is 10.
#' @param envir Environment to inspect. Default is the global environment.
#'
#' @return A data frame with object names and formatted object sizes,
#' ordered from largest to smallest.
#'
#' @examples
#' \dontrun{
#' top_largest_objects()
#' top_largest_objects(n = 5)
#' top_largest_objects(envir = environment())
#' }
#'
#' @export
object_size_check <- function(n = 10, envir = .GlobalEnv) {
  
  # Object names in environment
  objs <- ls(envir = envir)
  
  if (length(objs) == 0) {
    return(
      data.frame(
        object = character(),
        size = character(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Calculate sizes
  obj_sizes <- data.frame(
    object = objs,
    size = sapply(
      objs,
      function(x) object.size(get(x, envir = envir))
    ),
    stringsAsFactors = FALSE
  )
  
  # Order and format
  obj_sizes <- obj_sizes[order(-obj_sizes$size), , drop = FALSE]
  obj_sizes$size <- format(obj_sizes$size, units = "auto")
  
  head(obj_sizes, n)
}
