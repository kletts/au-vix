
source("yahoo.R")

data <- utils::read.csv("avix.csv") |> 
    dplyr::mutate(Date = as.Date(Date))
if (max(data$Date) < Sys.Date() - 1) { 
    vix <- read_yahoo("^AXVI", freq="Monthly")
    data <- vix |>      
        dplyr::filter(Measure=="adjclose") |>
        dplyr::mutate(Date=dplyr::if_else(dplyr::row_number() == c(1,2), eom(Date, 0), Date))  |> 
        dplyr::select(Date, VIX=Value) |> 
        dplyr::bind_rows(data) |> 
        dplyr::distinct(Date, .keep_all = TRUE) |> 
        dplyr::arrange(Date)
    utils::write.csv(data, "avix.csv", row.names =FALSE)
    } 

data <- utils::read.csv("asxaccum.csv") |> 
    dplyr::mutate(Date = as.Date(Date)) 
if (max(data$Date) < Sys.Date() - 1) { 
    asx <- read_yahoo("^AXJT", freq="Monthly") 
    data <- asx |>
        dplyr::filter(Measure=="adjclose") |>
        dplyr::mutate(Date=dplyr::if_else(dplyr::row_number() == c(1,2), eom(Date, 0), Date)) |> 
        dplyr::select(Date, ASXAccum=Value) |> 
        dplyr::bind_rows(data) |> 
        dplyr::distinct(Date, .keep_all = TRUE) |> 
        dplyr::arrange(Date)
    utils::write.csv(data, "asxaccum.csv", row.names =FALSE) 
}

