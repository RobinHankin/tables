## This file creates "gaussian_table_values2.txt" which holds suitably
## rounded numerical values of Gaussian distribution function in a
## nice LaTeX format including nice line spacing.  File
## gaussian_table_values.txt is imported by file
## table_gaussian_percentile.tex.

filename <- "gaussian_table_values3.txt"

qq <- c(0.001,0.002,0.005,(1:10)/100,0.2,0.3)


library("magrittr")

write("",file=filename,append=FALSE)

for(i in qq){
  bodyline <- paste(i,round(qnorm(i/2),4), sep=" & ") %>% paste("\\\\")
  write(bodyline,file=filename,append=TRUE)
}


