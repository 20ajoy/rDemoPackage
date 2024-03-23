#' This Function reads the csv files corresponding to code{years} from the local directory and returns tibbles with two column MONTH and year for each file.
#' @param years A vector containing the years.
#' @return A list containing tibble for each csv file.
#' @details If there are no files specific to any of the years \code{"invalid year: "} warning will be shown in the console and the corresponding list element will #' be NULL
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years(c(2013,2014,2015))
#'
#' }
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

