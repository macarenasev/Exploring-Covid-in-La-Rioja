
library(data.table)
library(pdftools)
library(R.utils)
library(leaflet)
source("./src.R")

files = list(open_cases = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODU1fGNmPTAz",
             mean_time_diagnosis = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODY0fGNmPTAz",
             cumulative_incidence = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODU3fGNmPTAz",
             cumulative_incidence_region = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODYwfGNmPTAz",
             tests = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODU2fGNmPTAz",
             tests_age_range = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODU4fGNmPTAz", 
             tests_region = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODYxfGNmPTAz")


column_names_translator = list(
  open_cases = c("City_w_more_than_1000_people", "Active_Cases"),
  mean_time_diagnosis = c("Week", "Mean_Time_Diagnosis"),
  cumulative_incidence = c("Date", "New_Pos", "Cum_14_Days", "Cum_Inc_14_Days", "Cum_7_Days", "Cum_Inc_7_Days"),
  cumulative_incidence_region = c("Region", "New_Pos", "Cum_14_Days", "Cum_Inc_14_Days", "Cum_7_Days", "Cum_Inc_7_Days"),
  tests = c("Date", "Tests_Total", "Tests_Pos", "Tests_Neg", "Tests_Cum_Pos", "Tests_Cum_Neg", "Tests_Cum_Total"),
  tests_age_range = c("Age_Range", "PCR_Last_14_Days", "PCR_Pos_Last_14_Days", "PCR_Neg_Last_14_Days", "PCR_Cum_Pos", "PCR_Cum_Neg", "PCR_Cum_Total"),
  tests_region = c("Lat", "Lon", "City", "Tests_Total", "Tests_Cum_Total", "Tests_Pos", "Tests_Neg", "Tests_Cum_Pos", "Tests_Cum_Neg",
                   "New_Detected_Cum_14_Days", "New_Detected_Cum_7_Days", "Cum_Inc_14_Days", "Cum_Inc_7_Days"))


update.files(files)
logs <- sapply(files, load.file, files, column.translator = column_names_translator)

