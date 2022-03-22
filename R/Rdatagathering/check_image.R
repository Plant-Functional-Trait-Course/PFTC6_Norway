check_image <- function(pathfile, check_ij = TRUE){
  resolution <- 300
  imageSize <- "2552x3508"
  BitsPerSample <- 8 # colour depth
  
  file <- basename(pathfile)  
  
  stop2 <- function(msg, rename = FALSE) {
    x11()
    plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
    title(main = paste(strwrap(msg, width = 30), collapse = "\n"), col.main = "red", line = -1)
    locator(1)
    graphics.off()
    if(isTRUE(rename)){
      message("rename")
      source("varEntryDialog.r")
      newName <- varEntryDialog(
        vars=c('Correct_name'),
        labels=c('Enter correct name'),
        fun=c(function(x) {
          # check extension
          if(!grepl("([^\\s]+(\\.(jpg|jpeg))$)", x, ignore.case = TRUE)){
            stop(paste0("File extension on ", x, " not permitted - use '.jpg'"))
          }
                               
          # check file name is permitted
          load("envelope_codes.Rdata")
          if(!grepl("^[A-Z]{3}\\d{4}\\.(jpg|jpeg)$", x, ignore.case = TRUE)){
           stop(paste0("File name ", x, " not expected format (3-letters, 4-numbers)"))
          }
          file_base <- gsub("(^[A-Z]{3}\\d{4}).*", "\\1", x)
         

          if(!file_base %in% all_codes$hashcode){
            stop(paste0("File name ", x, " not in list of permitted names"))
          }
      }))
      file.rename(from = pathfile, to = paste0(dirname(pathfile), newName))
      
    } else{
      stop(msg)
    }
  }
  

  # check extension
  if(!grepl("([^\\s]+(\\.(jpg|jpeg))$)", file, ignore.case = TRUE)){
    stop2(paste0("File extension on ", file, " not permitted - use '.jpg'"), rename = FALSE)
  }
  
  # check file name is permitted
 
  if(!grepl("^[A-Z]{3}\\d{4}\\.(jpg|jpeg)$", file, ignore.case = TRUE)){
    stop2(paste0("File name ", file, " not expected format (3-letters, 4-numbers)"), rename = FALSE)
  }
  file_base <- gsub("(^[A-Z]{3}\\d{4}).*", "\\1", file)

  load("envelope_codes.Rdata")# makes all_codes  

  if(!file_base %in% all_codes$hashcode){
    stop2(paste0("File name ", file, " not in list of permitted names"), rename = FALSE)
  }
  

  # check exif information is good
  require("exifr")
  exif <- read_exif(pathfile)
  #correct resolution
  if(exif$XResolution != resolution | exif$YResolution != resolution){
    stop2(paste0("Scan resolution is ", exif$XResolution, " not expected ", resolution))
  }
  
  #correct size (with tolerance)
  if(exif$ImageSize != imageSize){
    stop2(paste0("Scan size is ", exif$ImageSize, " pixcels not expected ", imageSize, " (A4)"))
  }

    #colour depth
  if(exif$BitsPerSample != BitsPerSample){
    stop2(paste0("Colour depth is ", exif$BitsPerSample, " bits not expected ", BitsPerSample, " (full colour)"))
  }
  
  #imagej check
  if(isTRUE(check_ij)){
    #to do
  }
  
  print("Passed checks")
}
  

# check_image("folder/good.jpgx")
# check_image("folder/AA1111.jpg")
# check_image("folder/AAA1111.jpg")
# check_image("folder/AAA4667.jpg")
