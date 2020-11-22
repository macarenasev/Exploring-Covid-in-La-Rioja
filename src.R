

#' Update needed files for dashboard
#' @param files A list of files to download (url)
#' @param opath Output path. Gy default is ./data/
#' Download file from url. If download was succesfull overwrite existing file and updates changelog
update.files <- function(files, opath = "./data/"){
  
  today <- Sys.Date()
  if(file.exists(paste0(opath, "changelog.csv.gz"))){
    changelog = fread(paste0(opath, "changelog.csv.gz"))
    if (min(changelog$date_updated) == today){
      print('Files are up to date')
      return()
    }else{
      print('Downloading files up to date')
    }
    
  }else{
    changelog = data.table(file = character(), date_updated = character())
  }

  for(file in files){
    file_name = names(which(files == file))
    
    dt <- as.data.table(read.csv(file, sep = ",", check.names = F))
    if(ncol(dt) == 1)
      dt <- as.data.table(read.csv(file, sep = ";", check.names = F))
    
    if(nrow(dt) > 1){
      
      ofile <- paste0(opath, file_name, ".csv.gz")
      fwrite(dt, ofile)
      
      if(nrow(changelog[file == file_name]) == 0){
        changelog = rbind(changelog, data.table(file = file_name, date_updated = as.character(today)))
      }else{
        changelog[file == file_name, date_updated := today]
      }
    }
  }
  
  ofile <- paste0(opath, "changelog.csv.gz")
  fwrite(changelog, ofile)
}


#' Load file and convert it to data.table
#' @param file Name of the file to load
#' @param files List of available files
#' @param opath Path were file is located
#' Load file in Global Environment and convert it to data.table
load.file <- function(file, files, opath = "./data/", column.translator = column_names_translator){
  
  file_name = names(which(files == file))
  column_names = as.character(unlist(column.translator[names(column.translator) == file_name]))
  
  if(file.exists(paste0(opath, file_name, ".csv.gz"))){
    eval(parse(text = paste0(file_name, "= fread('", opath, file_name, ".csv.gz', encoding = 'UTF-8')")), envir=.GlobalEnv)
    eval(parse(text = paste0("setnames(", file_name, ", column_names)")))
    eval(parse(text = paste0(file_name, " <- apply(", file_name, ", 2, toupper)")))
    
  }else{
    eval(parse(text = paste0(file_name, "= data.table()")), envir=.GlobalEnv)
  }
  
}



