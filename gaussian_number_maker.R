## This file creates "gaussian_table_values.txt" which holds suitably
## rounded numerical values of Gaussian distribution function.  File
## gaussian_table_values.txt is imported by file
## table_gaussian_distribution_function.tex.

filename <- "gaussian_table_values.txt"
library("magrittr")
x <- 
  seq(from=0, by=0.01,to=3.09) %>%
  pnorm                        %>%
  round(4)                     %>%
  sprintf("%1.4f", .)          %>%
  sub("0." ,".",.)             %>%
  matrix(byrow=TRUE,ncol=10)   %>%
  noquote

rownames(x) <-
  seq(from=0,to=3.0,by=0.1)    %>%
  sprintf("%1.1f", .)       

colnames(x) <- 
  seq(from=0,to=0.09,by=0.01)  %>%
  sprintf("%1.2f", .)         


write("",file=filename,append=FALSE)

for(i in seq_len(nrow(x))){
  bodyline <- paste(rownames(x)[i],paste(x[i,],collapse = "&"),sep= "&")
  print(bodyline)
  if((i>1) & (i%%5==1)){
    bodyline %<>% paste("\\\\[3mm]")
  } else {
    bodyline %<>% paste("\\\\")
  }
  write(bodyline,file=filename,append=TRUE)
}


