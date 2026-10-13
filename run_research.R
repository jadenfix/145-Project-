args <- commandArgs(trailingOnly=TRUE)
script <- if(length(args)) args[[1]] else "msmodel4.12(works).R"
allowed <- c("msmodel4.12(kindaworks.R","msmodel4.12(works).R","145project4.8.R","145dataviz.R")
if (!(script %in% allowed)) stop("Select an original research script in this repository")
inputs <- c("data_1.csv","data_11.csv")
if(any(!file.exists(inputs))) stop("Missing original data: ", paste(inputs[!file.exists(inputs)], collapse=", "))
source(script)
