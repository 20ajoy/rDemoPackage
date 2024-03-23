
#' This Function creates string.
#' @param year A vector or single \code{integer} valued input, floating point values will be converted to \code{integer}.
#' @return A string.
#' @importFrom base as.integer sprintf
#' @examples
#' make_filename(2023)
#' make_filename(c(2022,2023,2024))
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
