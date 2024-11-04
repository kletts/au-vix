
source("yahoo.R")

data <- utils::read.csv("avix.csv") |> 
    dplyr::mutate(Date = as.Date(Date))
if (max(data$Date) < Sys.Date() - 1) { 
    vix <- read_yahoo("^AXVI", freq="Monthly", start_date = Sys.Date()-1)
    data <- data |> dplyr::bind_rows(vix |> 
                dplyr::filter(Measure=="adjclose") |> 
                dplyr::filter(!(Date %in% data$Date)) |> 
                dplyr::select(Date, VIX=Value))
    utils::write.csv(data, "avix.csv", row.names =FALSE)
    } 

data <- utils::read.csv("asxaccum.csv") |> 
    dplyr::mutate(Date = as.Date(Date)) 
if (max(data$Date) < Sys.Date() - 1) { 
    asx <- read_yahoo("^AXJT", freq="Monthly", start_date = Sys.Date()-1)
    data <- data |> dplyr::bind_rows(asx |> 
                dplyr::filter(Measure=="adjclose") |> 
                dplyr::filter(!(Date %in% data$Date)) |> 
                dplyr::select(Date, ASXAccum=Value))
    utils::write.csv(data, "asxaccum.csv", row.names =FALSE) 
}

