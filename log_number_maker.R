## This file creates "log_table_values.txt" which holds suitably
## rounded (integer!) values for the log table.
## File log_table_values.txt is imported by table_log.tex.
rm(list=ls())
library("magrittr")
filename <- "log_table_values.txt"


quick <- FALSE # change this for faster working when debugging
if(quick){
  load("log.Rdata")
} else {
  source("logtable.R")   # creates R variables table_main and table_Delta
}

formatted_main <- function(x){
  out <- as.character(x)
  isn <- is.na(x)
  if(any(!isn)){ out[!isn] <- sprintf("%04.0f",x[!isn])}
  if(any( isn)){ out[ isn] <- " "}
  return(out)
}

formatted_Delta <- function(x){sprintf("%04.0f",x)}


table_filewriter <- function(filename,main,Delta,fun){

  write("%this file is not human-readable, it was created by 'log_number_maker.R'",file=filename,append=FALSE)
  for(i in seq_len(nrow(main))){
    
    text_rows <- c(formatted_main(main[i,]),Delta[i,])
    bodyline <- paste("{\\large ",rownames(main)[i],"}&",collapse="")  %>%
      paste(paste(text_rows,collapse="&"))
    print(fun)
    if(fun(i)){
      bodyline %<>% paste("\\\\[3mm]")
    } else {
      bodyline %<>% paste("\\\\")
    }
    write(bodyline,file=filename,append=TRUE)
  }
}


first_page <- 1:44
linespacefunction1 <- function(i){ (i>6) & (i%%5==4) }
linespacefunction2 <- function(i){  i%%5==0}

table_filewriter("log_table_values_page1.txt",table_main[ first_page,],table_Delta[ first_page,],fun=linespacefunction1)
table_filewriter("log_table_values_page2.txt",table_main[-first_page,],table_Delta[-first_page,],fun=linespacefunction2)
