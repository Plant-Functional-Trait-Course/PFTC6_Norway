main <- function(){
  files <- list.files(path = "~/Desktop/Svalbard_leaves")

    # check extension
    wrong_extension <- !grepl("([^\\s]+(\\.(jpg|jpeg))$)", files, ignore.case = TRUE)
   if(any(wrong_extension)){
     cat("These files have the wrong extension (or a space somewhere)\n")
     print(t(files[wrong_extension]))
     cat("-----\n")
   }

    files <- files[!wrong_extension]#don't consider bad files further
        
    # check filename format is correct
    wrong_format <- !grepl("^[A-Z]{3}\\d{4}\\.(jpg|jpeg)$", files, ignore.case = TRUE)
    if(any(wrong_format)){
      cat("Expected format is 3-letters + 4-numbers.\nThese files have the wrong format\n")
      print(t(files[wrong_format]))
      cat("-----\n")
    }
    
    files <- files[!wrong_format]#don't consider bad files further    
    
    #check names valid
    file_base <- gsub("(^[A-Z]{3}\\d{4}).*", "\\1", files)
    
    load("envelope_codes.Rdata")# loads codes  
    
    invalid_name <- !file_base %in% all_codes$hashcode
    if(any(invalid_name)){
      cat("Invalid codes")
      print(t(file_base[invalid_name]))
      cat("-----\n")
    }
    
    cat("Finished")
    cat("Hit enter to continue")
    readLines("stdin",n=1)
}
main()