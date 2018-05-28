## This file creates "gaussian_table_values1.txt" which holds suitably
## rounded numerical values of Gaussian distribution function in a
## nice LaTeX format including nice line spacing.  File
## gaussian_table_values1.txt is imported by file
## table_gaussian_distribution_function.tex.

## Files gaussian_diagram[123].pdf are created by gaussian_diagram.R


filename <- "gaussian_table_values1.txt"
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
  bodyline <- paste("{\\large ",rownames(x)[i],"}&",collapse="")  %>%
    paste(paste(x[i,],collapse="&"))
  if((i>1) & (i%%5==1)){
    bodyline %<>% paste("\\\\[3mm]")
  } else {
    bodyline %<>% paste("\\\\")
  }
  write(bodyline,file=filename,append=TRUE)
}


