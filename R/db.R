drop_save_rds <- function(data, file, path, ...){
  saveRDS(data, file, ...)
  drop_upload(file, path = path)
  print(getwd())
}

drop_read_rds <- function(file, output_dir, dest = tempdir(), 
                          dtoken = get_dropbox_token(), ...) {
  localfile = paste0(dest, "/", basename(file))
  print(localfile)
  drop_download(paste(output_dir, file, sep = '/'), local_path = localfile, overwrite = TRUE, dtoken = dtoken)
  readRDS(localfile, ...)
}

backup <- function(dtoken){

  today <- Sys.Date()
  files <- drop_dir(paste("Tarot/back_up/", sep = '/'))
  back_up_date <- as.Date(stringr::str_extract(files$name, 
                                               "(?<=scores_)(.*?)(?=.RDS)")
                          )

  if (! today %in% back_up_date){
    drop_copy(from_path = "tarot/www/scores.RDS", 
              to_path = paste0("tarot/back_up/scores_", today, ".RDS"),
              dtoken=dtoken
    )
  }

  invisible(
    lapply(files$path_display[
      ((back_up_date < today - 7) &
        (tolower(format(back_up_date, "%a")) == "lun." |
           tolower(format(back_up_date, "%a")) == "mon.")) |
        back_up_date < today - 30], 
      drop_delete, dtoken = dtoken)  
    )
  
  }
