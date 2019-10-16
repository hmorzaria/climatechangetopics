install.packages("corpus.JSS.papers", repos = "http://datacube.wu.ac.at/", type = "source")

.packages = c(
  "RCurl", "XML","rvest", "httr", "tidyverse","data.table","parallel","doSNOW","here", 
  "tidytext", "wordcloud","tm", "reshape2","topicmodels","robotstxt","citr","janitor",
  "rjson","tokenizers")


# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if (length(.packages[!.inst]) > 0)
  install.packages(.packages[!.inst], dependencies = TRUE)

# Load packages into session
lapply(.packages, require, character.only = TRUE)