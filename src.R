

#' Update needed files for dashboard
#' @param files A list of files to download (url)
#' @param opath Output path. Gy default is ./data/
#' Download file from url. If download was succesfull overwrite existing file and updates changelog
update.files <- function(files, opath = "./data/"){
  
  if(file.exists(paste0(opath, "date_updated.csv.gz"))){
    changelog = fread(paste0(opath, "date_updated.csv.gz"))
  }else{
    changelog = data.table(file = character(), date_updated = character())
  }

  for(file in files){
    file_name = names(which(files == file))
    today <- Sys.Date()
    
    dt <- as.data.table(read.csv(file, sep = ","))
    if(ncol(dt) == 1)
      dt <- as.data.table(read.csv(file, sep = ";"))
    
    if(nrow(dt) > 1){
      
      ofile <- paste0(opath, file_name, ".csv.gz")
      fwrite(dt, ofile)
      
      changelog[file == file_name, date_updated := today]
    }
  }
  
  ofile <- paste0(opath, "changelog.csv.gz")
  fwrite(dt, ofile)
}


load.file <- function(file, files, opath = "./data/", column.translator = column_names_translator){
  
  file_name = names(which(files == file))
  print(file_name)
  
  if(file.exists(paste0(opath, file_name, ".csv.gz"))){
    eval(parse(text = paste0(file_name, "= fread('", opath, file_name, ".csv.gz')")), envir=.GlobalEnv)
    eval(parse(text = paste0("names(", file_name, ") = names(column_names_translator)[unlist(lapply(names(", file_name,
                             "), function(x){ grep(paste0('^', x, '$'), column_names_translator)}))]")), envir=.GlobalEnv)
  }else{
    eval(parse(text = paste0(file_name, "= data.table()")), envir=.GlobalEnv)
  }
  
  
}



