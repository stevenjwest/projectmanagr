#' Replace and insert a vector
#'
#' This function will find the FIRST INSTANCE of the exact match pattern in the passed vector, x,
#' and replace this element with all elements in the vector, values.
#'
replaceAndInsertVector <- function( pattern, values, x ) {

  index <- match(pattern, x)

  c(x[1:(index-1)], values, x[(index+1):length(x)])

}
