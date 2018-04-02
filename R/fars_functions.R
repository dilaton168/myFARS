#' fars_read
#'
#' Read a csv-data file. If file exists, the data will be loaded and a fraction of it will be shown.
#'
#' @param filename File name of the csv-file.
#'
#' @return A dataframe.
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' \dontrun{
#' fars_read(filename = 'filedoesnotexist')
#' }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' make_filename
#'
#' Attach a prefix and suffice to a number (year) in a form of "accident_#.csv.bz2".
#'
#' @param year A number, presumably a 4-digit year.
#'
#' @return A string in the form "accident_#.csv.bz2".
#'
#' @examples
#' make_filename(year = '2013')
#'
#' \dontrun{
#' make_filename(year = 'two thousand eighteen') #  error
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' Read multiple CSV-files for the corresponding years of "accident_####.csv.bz2".
#'
#' @param years List of years.
#'
#' @return Display the dateframe representation of the input data.
#'
#' @examples
#' fars_read_years(c(2013, 2014, 2015))
#'
#' \dontrun{
#' fars_read_years(years = 2000) # error
#' }
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year)
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' fars_summarize_years
#'
#' Produce a summary of the FARS Files.
#'
#' @param years List of years.
#'
#' @return Display the data characteristics of the years of the corresponding CSV-file.
#'
#' @examples
#' fars_summarize_years(c(2013, 2014, 2015))
#'
#' \dontrun{
#' fars_summarize_years(years = 2018)
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list)
                dplyr::group_by(year, MONTH)
                dplyr::summarize(n = n())
                tidyr::spread(year, n)
}

#' fars_map_state
#'
#' Plot the fars data or the state (input) from the input data of a selecter year.
#'
#' @param state.num Integer representation of a state.
#' @param year 4-digit year.
#'
#' @return NULL
#'
#' @examples
#' fars_map_state(1, 2014)
#'
#' \dontrun{
#' fars_map_state(3, 2019)   # error
#' }
#'
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
