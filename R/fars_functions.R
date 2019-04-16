#' Read FARS data
#'
#' This is a function that reads in the FARS dataset.
#'
#' @param filename A character string for the filename, in (compressed) CSV format, of the dataset.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @return The function returns a dataframe tbl if the file exists.  If the file does not
#'      exist, then the function throws an error and halts execution. 
#'
#' @examples
#' fars_read("./data/accident_2013.csv.bz2")
#' fars_read(filename = "./data/accident_2014.csv.bz2)
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

#' Make a filename string for a given year
#'
#' This function makes a filename string given a year
#'
#' @param year A numeric value representing the calendar year
#'
#' @return This function returns a filename string of the form "accident_%d.csv.bz2", where %d is the integer value
#'      of the calendar year.
#'
#' @examples
#' make_filename(2019)
#' make_filename(year = 2018)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS data for a vector of years, creating a list of data frames with year column
#'
#' @param years A numeric vector of years for which to read FARS datasets.
#'
#' @importFrom dplyr mutate select
#' 
#' @return This function returns a list of FARS data frames, one for each year in the years list, checking if the data
#'      file exists for each year.  If the data file is sucessfully read, a year column is added to the corresponding 
#'      data frame, otherwise a warning is issued that the supplied year value is invalid, and the entry in the returned
#'      list becomes a NULL value.
#'
#' @examples
#' fars_read_years(2014)
#' fars_read_years(c(2013, 2014))
#'
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

#' Summarize data, grouped by year and month for a list of years of FARS data
#'
#' @param years A numeric vector of years of FARS datasets.
#'
#' @importFrom dplyr bind_rows group_by summarize spread
#' 
#' @return This function tabulates summary data for a one or more years of FARS datasets, grouped by year and month,
#'      providing total counts for each year.  It uses the error handling procedures from the *fars_read_years* 
#'      function.
#' @examples
#' fars_summarize_years(2014)
#' fars_summarize_years(c(2103, 2014))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plots fatalities from FARS data for a state in a given year
#'
#' @param state.num The numeric value representing the state
#' @param year The numeric value of the year
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @return This function plots the coordinates of fatalities for a statee in a given year.  This function checks the
#'      validity of the state.num and filters the data by state.num if valid.  If the state.num is not valid, the
#'      function throws an error and the program halts.  If there are no accidents for the state in the given year, then
#'      the function indicates so by a message.
#'
#' @examples
#' fars_map_state(25, 2019)
#' fars_map_state(state.num = 20, year = 2014)
#'
#'@export
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
