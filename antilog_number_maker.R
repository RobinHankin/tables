## This file creates "antilog_table_values.txt" which holds suitably
## rounded (integer!) values for the log table.
## File log_table_values.txt is imported by table_log.tex.
rm(list=ls())
library("magrittr")


quick <- FALSE # change this for faster working when debugging
if(quick){
    load("antilog.Rdata")
} else {
    source("antilogtable.R")   # creates R variables table_main and table_Delta
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

    write("%this file is not human-readable, it was created by 'antilog_number_maker.R'",
          file=filename, append=FALSE)
    for(i in seq_len(nrow(main))){
    
        text_rows <- c(formatted_main(main[i,]),Delta[i,])
        bodyline <- paste("{\\normalsize ",rownames(main)[i],"}&",collapse="")  %>%
            paste(paste(text_rows,collapse="&"))
        if(fun(i)){
            bodyline %<>% paste("\\\\[2mm]")
        } else {
            bodyline %<>% paste("\\\\")
        }
        write(bodyline,file=filename,append=TRUE)
    }
}

## Full table first:
first_page <- 1:50
linespacefunction <- function(i){ i%%5==0 }

table_filewriter("antilog_table_values_page1.txt",antilogtable_main[ first_page,],antilogtable_delta[ first_page,],fun=linespacefunction)
table_filewriter("antilog_table_values_page2.txt",antilogtable_main[-first_page,],antilogtable_delta[-first_page,],fun=linespacefunction)
