#' Loads a CSV file with FARS data
#'
#' It checks if the CSV file exists. 
#' If it exists, it reads the data, if it does not, it will return an error.
#'
#' @param filename A string containing the path that it is going to be used for reading the data.
#'
#' @return Returns a data.frame containing the data.  
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
#'
#' @examples
#' \dontrun{
#' FARSdf<-fars_read('data.csv.bz2')
#' }
#'
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}



#' Creates a filename for a given year
#'
#' It creates a filename taking into account the year.
#'
#' @param year An integer where the year is stored.
#'
#' @return Returns a string with the filename
#'
#' @export
#'
#' @examples
#' \dontrun{fn<-make_filename(2017)}
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Processes data from the wanted years
#'
#' It processes data for a given year to obtain the crashes from each month. If the years selected does not exist it returns an error.
#'
#' @param years An integer vector with the year(s) to use.
#'
#' @return A list of data.frames, each data.frame (tibble in this case) contains two columns month and year and a row for each fatal accident.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#' \dontrun{fry <- fars_read_years(2011:2015)}
#'
fars_read_years <- function(years) {
    MONTH = NULL
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


#' Summarizes number of accidents per month and year
#'
#' It summarize the number of observations by month and year in the given year(s)
#'
#' @param years An integer vector of years where it will search
#'
#' @return A data.frame with two columns, year and month. Each row the sum of observations in a year-month.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @export
#'
#' @examples
#' \dontrun{fsy<-fars_summarize_years(2011:2015)}
#'
fars_summarize_years <- function(years) {
        year = n = MONTH = NULL
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Plots the results on a map
#'
#' It plots the results on a map from a given year and state with the fatal crasesh.
#'
#' @param state.num An integer with the state id
#' @param year An integer with the year to use
#'
#' @return NULL
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
#'
#' @examples
#' \dontrun{fars_map_state(12, 2012)}
#'
fars_map_state <- function(state.num, year) {
        STATE = year = NULL
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