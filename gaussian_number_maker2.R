## This file creates "gaussian_table_values2.txt" which holds suitably
## rounded numerical values of Gaussian distribution function in a
## nice LaTeX format including nice line spacing.  File
## gaussian_table_values.txt is imported by file
## table_gaussian_percentile.tex.

filename <- "gaussian_table_values2.txt"

qq <- c(0.001,0.005,seq(from=0.01,to=0.04,by=0.01),seq(from=0.05,to=0.2,by=0.05),0.3,0.4)
qq <- sort(c(qq,0.5,1-qq))   # increasing

library("magrittr")

write("%this file is not human-readable, it was created by 'gaussian_number_maker2.R'",file=filename,append=FALSE)

for(i in qq){
  bodyline <- paste(i,round(qnorm(i),4), sep=" & ") %>% paste("\\\\")
  write(bodyline,file=filename,append=TRUE)
}


