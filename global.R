
library(data.table)
library(pdftools)
library(R.utils)
source("./src.R")

files = list(open_cases = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODU1fGNmPTAz",
             mean_time_diagnosis = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODY0fGNmPTAz",
             cumulative_incidence = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODU3fGNmPTAz",
             cumulative_incidence_region = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODYwfGNmPTAz",
             tests = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODU2fGNmPTAz",
             tests_age_range = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODU4fGNmPTAz", 
             tests_region = "https://ias1.larioja.org/opendata/download?r=Y2Q9ODYxfGNmPTAz")

column_names_translator = list(Region = "ZONA.BASICA",
                               New_Pos = "NUEVOS.POSITIVOS",
                               Cum_14_Days = "ACUMULADOS.14.DIAS",
                               Cum_Inc_14_Days = "IA.14.DIAS", 
                               Rate_Inc_14_Days = "TASA.IA.14",
                               Cum_7_Days = "ACUMULADOS.7.DIAS",
                               Cum_Inc_7_Days = "IA.7.DIAS", 
                               Rate_Inc_7_Days = "TASA.IA.7",
                               Age_Range = "GRUPO.EDAD",
                               PCR_Last_14_Days = "PCR.REALIZADOS..ULTIMOS.14.DIAS.",
                               PCR_Pos_Last_14_Days = "PCR.POSITIVOS..ULTIMOS.14.DIAS.",
                               PCR_Neg_Last_14_Days = "PCR.NO.POSITIVOS..ULTIMOS.14.DIAS.",
                               PCR_Cum_Pos = "PCR.POSITIVOS..ACUMULADOS.",
                               PCR_Cum_Neg = "PCR.NO.POSITIVOS..ACUMULADOS.",
                               PCR_Cum_Total = "PCR.REALIZADOS..ACUMULADOS.",
                               Date = "FECHA" ,
                               Tests_Total = "PRUEBAS.REALIZADAS",
                               PCR_Total =  "PCR.REALIZADOS" ,
                               Tests_Pos = "POSITIVOS" ,
                               Tests_Neg = "NEGATIVOS" ,
                               Tests_Cum_Pos = "ACUMULADO.POSITIVOS",
                               Tests_Cum_Neg = "ACUMULADO.NEGATIVOS",
                               Tests_Cum_Total = "TOTAL.ACUMULADO"  ,
                               Week = "SEMANA",
                               Mean_Time_Diagnosis = "TIEMPO.PROMEDIO.DIAGNOSTICO",
                               Lat = "Lat",
                               Lon = "Lon",
                               City = "Title",
                               New_Detected_Cum_14_Days = "NUEVOS.CASOS.ACUMULADO.14.DIAS",
                               New_Detected_Cum_7_Days = "NUEVOS.CASOS.ACUMULADO.7.DIAS",
                               PCR_Test_Cum_Pos = "ACUMULADO.POSITIVO",
                               PCR_Test_Cum_Neg = "ACUMULADO.NEGATIVO",
                               City_w_more_than_1000_people = "Localidades.de.más.de.1.000.habitantes",
                               Active_Cases = "Casos.activos" )


update.files(files)
logs <- sapply(files, load.file, files, column.translator = column_names_translator)

