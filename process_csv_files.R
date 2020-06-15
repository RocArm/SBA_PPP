# process_csv_files.R
#-------------------------------------
# Read SBA PPP tables in csv format and save them as various Rdata objects.
#-------------------------------------

#-------------------------------------
# Definitions

# Input/Output
wd <- getwd()
folder.in <- paste0(wd, "/test_read/csv_files/") 
folder.out <- paste0(wd, "/test_read/Rdata_files/") 
file.dates <- paste0(wd, "/test_read/report_dates.csv") 

# Files to process
files.vec <- dir(folder.in, full.names = TRUE)


#-------------------------------------
# Libraries
library(tidyverse)

#-------------------------------------
# Functions

make_file.in <- function(date_string) {
  # For each report, create list with all csv files available
  list_out <- list(
    states = paste0(folder.in, "PPP_StateTable", date_string, ".csv"),
    size = paste0(folder.in, "PPP_LoanSizeTable", date_string, ".csv"),
    rank = paste0(folder.in, "PPP_LenderRankTable", date_string, ".csv"),
    sector = paste0(folder.in, "PPP_NAICSTable", date_string, ".csv")
  )
  return(list_out)
}


read_reports <- function(date_string) {
  # Main function: For each report, read csv files via readr::read_csv()
  #-------------------------------------

  # File names
  files.in <- make_file.in(date_string)

  # Not all reports have all the tables:
  files.exist <- sapply(files.in, file.exists)

  # States
  if (files.exist["states"]) {
    states.df <- read_csv(
      file = files.in$states,
      col_types = cols(
        State = col_character(),
        LoanCount = col_number(),
        NetDollars = col_number()
      )
    )
  } else {
    states.df < NULL
  }

  # Size
  if (files.exist["size"]) {
    size.df <- read_csv(
      file = files.in$size,
      col_types = cols(
        LoanSize = col_character(),
        LoanCount = col_double(),
        NetDollars = col_double(),
        PctofCount = col_double(),
        PctofAmount = col_double()
      )
    )
  } else {
    size.df <- NULL
  }

  # Rank
  if (files.exist["rank"]) {
    rank.df <- read_csv(
      file = files.in$rank,
      col_types = cols(
        Rank = col_double(),
        LenderName = col_character(),
        LoanCount = col_double(),
        NetDollars = col_double(),
        AvgLoanSize = col_double(),
        PctofTotalAuthority = col_double()
      )
    )
  } else {
    rank.df <- NULL
  }

  if (files.exist["sector"]) {
    sector.df <- read_csv(
      file = files.in$sector,
      col_types = cols(
        NAICS_Subsector_Desc = col_character(),
        LoanCount = col_double(),
        NetDollars = col_double(),
        PctOfAmount = col_double()
      )
    )
  } else {
    sector.df <- NULL
  }

  # Combine in a list
  list.out <- list(
    states = states.df,
    size = size.df,
    rank = rank.df,
    sector = sector.df
  )

  return(list.out)
}

#-----------------------------------
# Execute: Read csv files

date_strings.vec <- gsub("[^0-9]", "", files.vec) %>% unique()

date_reports.df <- read_csv(
  file = file.dates,
  col_types = "ccc"
)

list.df <- lapply(date_strings.vec, function(date_string) {
  list.out <- read_reports(date_string)
  list.out$dates <- filter(date_reports.df, ReportDate == date_string)
  return(list.out)
})

#------------------------------------
# Output format

# Latest report
date.vec <- as.Date(date_strings.vec, format = "%m%d%Y")
latest.list <- list.df[[which.max(date.vec)]]

save(file = paste0(folder.out, "latest.Rdata"), latest.list)

# First round complete
firstround.list <- list.df[[which(date_strings.vec == "04162020")]]
save(file = paste0(folder.out, "firstround.Rdata"), firstround.list)

# By report
byreport.list <- list.df
save(file = paste0(folder.out, "byreport.Rdata"), byreport.list)

# By table
states.list <- lapply(list.df, function(list) {
  states.df <- list$states
  dates.df <- mutate_all(list$dates, ~ as.Date(., format = "%m%d%Y"))
  return(cbind(states.df, dates.df))
})
states.df <- do.call("rbind", states.list)

size.list <- lapply(list.df, function(list) {
  size.df <- list$size
  dates.df <- mutate_all(list$dates, ~ as.Date(., format = "%m%d%Y"))
  return(cbind(size.df, dates.df))
})
size.df <- do.call("rbind", size.list)

rank.list <- lapply(list.df, function(list) {
  if (is.null(list$rank)) {
    return(NULL)
  } else {
    rank.df <- list$rank
    dates.df <- mutate_all(list$dates, ~ as.Date(., format = "%m%d%Y"))
    return(cbind(rank.df, dates.df))
  }
})

rank.df <- do.call("rbind", rank.list)

sector.list <- lapply(list.df, function(list) {
  if (is.null(list$sector)){
    return(NULL)
  } else {
    sector.df <- list$sector
    dates.df <- mutate_all(list$dates, ~ as.Date(., format = "%m%d%Y"))
    return(cbind(sector.df, dates.df))
  }
})

sector.df <- do.call('rbind', sector.list)

bytable.list <- list(
  states = states.df,
  size = size.df,
  rank = rank.df,
  sector = sector.df
)
save(file = paste0(folder.out, "bytable.Rdata"), bytable.list)

#eof