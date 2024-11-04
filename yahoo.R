
#' Yahoo Period
#' @description Calculates the period in seconds since 1 Jan 1970 which is the time specification
#' used by the Yahoo Finance API.
#' @param x A date or vector of dates
#' @return Numeric, the interval in seconds since 1 Jan 1970.
#' @export
#' @importFrom lubridate interval seconds
#' @examples yahoo_period(as.Date('1970-01-02'))

yahoo_period <- function(x) {
    x <- lubridate::interval(start=as.Date("1970-01-01"), end=x) |>
        lubridate::seconds() |>
        as.numeric() 
    return(sprintf("%0.0f", x))
}

#' @rdname yahoo_period
from_yahoo_period <- function(x) {
    as.Date(as.POSIXct(x, origin=as.Date("1970-01-01"), tz=Sys.timezone()),
            tz=Sys.timezone())
}

#' Parse Yahoo Dates
#' @description Converts dates received from Yahoo via the API to R date object
#' @param x Character vector of dates from Yahoo
#' @return Vector of R dates
#' @importFrom stringr str_match
#' @export
parse_yahoo_dates <- function(x) {
    x <- stringr::str_match(x, "(\\d{2})\\s(\\w{3}).+(\\d{4})")[,-1]
    x <- apply(x, 1, \(x) paste(x, collapse="-"))
    as.Date(x, format="%d-%b-%Y")
}

#' Read Yahoo
#' @description Downloads the price history of a ticker from the Yahoo API
#' @param ticker Character vector, Yahoo stock ticker
#' @param freq Optional string, the observation frequency: Monthly or Daily, defaults to Monthly
#' @param start_date Optional start date, if missing the earliest date is used
#' @param end_date Optional end date, if missing the latest date is used
#' @details The full list of support intervals by yahoo is: [1m, 2m, 5m, 15m, 30m, 60m, 90m, 1h, 1d, 5d, 1wk, 1mo, 3mo]
#' When requesting monthly data, the returned date for the month is the first day of the month, the returned value is
#' value for the last day of the month.
#' @return A tidy (ie long) data.frame containing:
#'  - Ticker: the stock ticker
#'  - Date: the observation date
#'  - Measure: the observation measure type (open, high, low, close, volume, adjclose)
#'  - Value: the observation value
#'  - Period: if monthly the observation period
#'  - Currency: the currency of the stock
#'  - Exchange: the stock exchange or data source
#'  - Fullname: the full company name or long name of the stock or instrument
#' @export
#' @importFrom httr build_url GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_cols mutate
#' @importFrom tidyr unnest pivot_longer
#' @examples read_yahoo("AAPL", start_date="2024-01-01", end_date="2024-03-01", freq="Monthly")

read_yahoo <- function(ticker,
                       freq=c("Monthly", "Daily", "Weekly"),
                       start_date=NULL,
                       end_date=NULL) {
    freq <- match.arg(freq)
    if (is.null(start_date)==TRUE) {
        start_date <- if_else(freq=="Monthly", 0, yahoo_period(Sys.Date() - years(1)))
    } else {
        start_date <- yahoo_period(as.Date(start_date))
    }
    if (is.null(end_date)==TRUE) {
        end_date <- yahoo_period(Sys.Date())
    } else {
        end_date <- yahoo_period(as.Date(end_date))
    }
    query <- list(period1=start_date,
                  period2=end_date,
                  interval=switch(freq, "Daily"="1d", "Weekly"="1wk", "Monthly"="1mo"))
    url  <- list(scheme ="https",
                 hostname ="query2.finance.yahoo.com",
                 path = file.path("v8", "finance", "chart", URLencode(ticker)))
    class(url) <- "url"
    url <- httr::build_url(url)
    data <- httr::GET(url, query=query)
    data <- httr::content(data, "text") |>
        jsonlite::fromJSON(data, simplifyDataFrame = TRUE)
    if (is.null(data$chart$error)) {
        result <- data$chart$result
        ydata <- dplyr::bind_cols(
            Ticker = result$meta$symbol,
            Currency = result$meta$currency,
            Exchange = result$meta$exchangeName,
            Fullname = result$meta$longName,
            Date = from_yahoo_period(result$timestamp[[1]]),
            tidyr::unnest(result$indicators$quote[[1]],
                          cols = c(open, high, low, close, volume)),
            tidyr::unnest(result$indicators$adjclose[[1]], cols=adjclose)) |>
            tidyr::pivot_longer(cols=c(open, high, low, close, volume, adjclose),
                                names_to="Measure", values_to="Value") 
        return(ydata)
    } else {
        return(data$chart$error)
    }}

