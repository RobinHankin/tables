## This file creates "log_table_values.txt" which holds suitably
## rounded (integer!) values for the log table.
## File log_table_values.txt is imported by table_log.tex.
rm(list=ls())
library("magrittr")
filename <- "log_table_values.txt"


quick <- TRUE
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


table_filewriter <- function(filename,main,Delta){

  write("%this file is not human-readable, it was created by 'log_number_maker.R'",file=filename,append=FALSE)
  for(i in seq_len(nrow(main))){
  
    text_rows <- c(formatted_main(main[i,]),Delta[i,])
  bodyline <- paste("{\\large ",rownames(main)[i],"}&",collapse="")  %>%
    paste(paste(text_rows,collapse="&"))
    
    if((i>6) & (i%%5==4)){
      bodyline %<>% paste("\\\\[3mm]")
    } else {
      bodyline %<>% paste("\\\\")
    }
    write(bodyline,file=filename,append=TRUE)
  }
}

table_filewriter("log_table_values_page1.txt",table_main[1:44,],table_Delta[1:44,])
