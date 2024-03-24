
#' This Function reads the csv file \code{filename} from the local directory and returns as \code{tibble}
#' @param filename The name or path of the csv file.
#' @return A tibble converted from the csv file.
#' @details If \code{filename} does not exist error will be shown as \code{Error in fars_read("filename") : file 'filename' does not exist}
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples
#' \dontrun{
#' # Read data from a CSV file and display the first few rows
#' data <- fars_read("path/to/fars_data.csv")
#' head(data)
#' }
#' @export
fars_read <- function(filename) {
  tryCatch(
    data <-  read.csv(system.file("extdata",
                                filename,
                                package = "farsTrialPackage")),
    error = function(err0){
      print("file does not exists")
    }
  )
  dplyr::as_tibble(data)
}


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


#' This Function creates visualization of accidents as points in United States Geographical Map.
#' @param state.num State ID from the file.
#' @param year Year corresponding to the file.
#' @return plot the accident data as points.
#' @details If the input \code{state.num} is not present will show the error \code{"invalid STATE number: state.num"}, If no data is present in the file with respect to the input \code{state.num} and \code{year} it will print the message \code{"no accidents to plot"}.
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' \dontrun{
#' fars_map_state(state.num = 1, year = 2015)
#' }
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
