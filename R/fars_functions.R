#' Read a csv file into a dataframe
#'
#' This function takes a CSV file name as an argument, and attempts to read the specified
#' file into a data.frame with the default settings. It gives an error if the file does
#' not exist.
#'
#'
#' @param filename The name of the CSV file to be opened.
#' 
#' @return A data frame constructed from the contents of the CSV file passed.
#'
#' @examples
#' farsread(filename = "accident_2013.csv")
#' farsread(filename = "accident_2014.csv")
#' 
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


#' Create a standard filename for the FARS dataset given a year
#'
#' This function creates a standard filename for the FARS dataset from
#' \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#' given a year.  
#'
#'
#' @param year The year of the data of interest.
#' 
#' @return This function returns a filename for the given year's compressed dataset.
#'
#' @examples
#' makefilename(2014)
#' makefilename(2013)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Read the month and year of incidents from multiple years of FARS data.
#'
#' This function reads one or more years of FARS data into a data frame, and returns a data frame
#' containing only the month and the year of each incident. Will give errors for any specified
#' year that does not have a specified value, and if the dplyr::%>% function has not been imported.
#'
#'
#' @param years The year(s) of the data of interest in a vector or list.
#' 
#' @return This function returns a data frame with two columns, MONTH and year, with a row for each FARS incident
#'      in the specified years.
#'
#' @examples
#' fars_read_years(2014)
#' fars_read_years(c(2014,2015,2016)
#' 
#' @importFrom dplyr %>%
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

#' Gives the number of FARS incident per month in each specified year.
#'
#' This function creates a table of the month and year of each incident in the FARS database
#' for one or more specified years, then computes and returns a summary table of the number of incidents
#' with the months in rows and years in columns.  Will give an error for any specified year
#' that does not have an associated data file, as well as an additional error if none
#' of the specified years are valid.
#'
#'
#' @param years The year(s) of the data of interest in a vector or list.
#' 
#' @return This function returns a data frame with years as columns and month numbers as row numbers.
#'
#' @examples
#' fars_summarize_years(2014)
#' fars_summarize_years(c(2014,2015,2016)
#' 
#' @importFrom dplyr %>%
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Plots the locations of FARS incidents in a given state in a given year.
#'
#' This function loads the FARS data for the give year, and selects all
#' the incidents in the given state number.  States are nubmered in alphabetical order
#' (see \href{https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/812449}{2016 FARS / CRSS Coding and Validation Manual}, 
#' p.50), including District of Columbia, Peurto Rico and Virgin Islands, as entries, but skipping the numbers 3, 7 and 14.
#' Will generate an error if an invalid state number is given, or if there are no incidents in the
#' specified state in the specified year.
#'
#'
#' @param year The year of the data of interest.
#' @param state.num The number of the state of equivalent ot be plotted.
#' 
#' @return Returns a null value, but generates a plot.
#'
#' @examples
#' fars_map_state(36,2013)
#' fars_map_state(34,2014)
#' fars_map_state(34,2014)
#' 
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
