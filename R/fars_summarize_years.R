
#' This Function creates summarization of the files corresponding to the \code{years}.
#' @param years A vector containing the years.
#' @return summarized table from the csv files.
#' @importFrom dplyr group_by, summarize
#' @importFrom tidyr spread
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013,2014,2015))
#'
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
