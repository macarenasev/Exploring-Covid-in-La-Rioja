
library(data.table)
library(pdftools)

df_open_cases <- as.data.table(read.csv("https://ias1.larioja.org/opendata/download?r=Y2Q9ODU1fGNmPTAz", sep = ","))

df_cumulative_incidence <- as.data.table(read.csv("https://ias1.larioja.org/opendata/download?r=Y2Q9ODU3fGNmPTAz", sep = ","))
names(df_cumulative_incidence) <- c("Date", 'New_Positives', 'Cum_14_Days', "Cum_Inc_14_Days", "Cum_7_Days", "Cum_Inc_7_Days")
df_cumulative_incidence[, Date := as.Date(Date, "%d/%m/%y")]

df_tests<- as.data.table(read.csv("https://ias1.larioja.org/opendata/download?r=Y2Q9ODU2fGNmPTAz", sep = ","))
names(df_tests) <- c("Date", 'No_Tests', 'No_Pos', "No_Neg", "Cum_Pos", "Cum_Neg", "Cum_Tests")
df_tests[, Date := as.Date(Date, "%d/%m/%y")]
df_tests[, Positive_Ratio := round((No_Pos/No_Tests)*100, digits = 2)]

df_tests_age_range <- as.data.table(read.csv("https://ias1.larioja.org/opendata/download?r=Y2Q9ODU4fGNmPTAz", sep = ","))

df_mean_time_diagnosis <- as.data.table(read.csv("https://ias1.larioja.org/opendata/download?r=Y2Q9ODY0fGNmPTAz", sep = ","))

text <- pdf_text("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Actualizacion_244_COVID-19.pdf") %>% 
  readr::read_lines()
