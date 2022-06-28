# Download clean data from OSF
# install.packages("devtools")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

# Check node on OSF
node <- "node"

# For example
#node <- "4c5v2"

# Data set 1
get_file(node = node,
         file = "File name with extension",
         path = "clean_data",
         remote_path = "OSF folder name")

# Data set 2
get_file(node = node,
         file = "File name with extension",
         path = "clean_data",
         remote_path = "OSF folder name")


