## This file creates "gaussian_table_values3.txt" which holds suitably
## rounded numerical values of Gaussian distribution function in a
## nice LaTeX format including nice line spacing.  File
## gaussian_table_values3.txt is imported by file
## table_gaussian_percentile.tex.

filename <- "gaussian_table_values3.txt"

qq <- c(0.001,0.002,0.005,(1:10)/100,0.2,0.3)


library("magrittr")


write("%this file is not human-readable, it was created by 'gaussian_number_maker3.R'",file=filename,append=FALSE)

for(i in qq){
  bodyline <- paste(i,round(qnorm(i/2),4), sep=" & ") %>% paste("\\\\")
  write(bodyline,file=filename,append=TRUE)
}


