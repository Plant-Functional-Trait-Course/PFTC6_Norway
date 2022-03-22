
### CALCULATE LEAF AREA

#load packages etc
if(!require("LeafArea")){
  if(!require("devtools")){
    install.packages("devtools", repos = "https://cran.uib.no")
  }

  devtools::install_github("richardjtelford/LeafArea")
}
source("varEntryDialog.r")


##get filename to check
path <- "~/Desktop/Svalbard_leaves"

print(dir(path = path))
#cat("Enter name of file to check:\n")
#f <- readLines(con = stdin(), n = 1)
f <- varEntryDialog(
  vars=c('file'),
  labels=c('File to check'))$file
    
cat("\nFile chosen", f) 
filepath <- file.path(path, f)

#Function to calculate leaf area
process.file <-  function(file){
  file.copy(file, new.folder)
  area <- run.ij(
    path.imagej = "/usr/share/java/", 
    set.directory = new.folder, 
    distance.pixel = 237, 
    known.distance = 2, 
    log = TRUE, 
    low.size = 0.005, 
    trim.pixel = 50, 
    trim.pixel2 = 150, 
    save.image = TRUE)

  file.copy(dir(new.folder, full.names = TRUE, pattern = "\\.tif"), output.folder)
  Sys.sleep(0.1)
  if(any(!file.remove(dir(new.folder, full.names = TRUE) ))) stop()
  res <- data.frame(ID = names(unlist(area[[2]])), LeafArea = (unlist(area[[2]])))
  return(res)
}



# Calculate leaf area
new.folder <- file.path(path, "temp")
if(!dir.exists(new.folder)){
  dir.create(new.folder)
}  

output.folder <- file.path(path, "ij_out")
if(!dir.exists(output.folder)){
  dir.create(output.folder)
}  


process.file(filepath)
