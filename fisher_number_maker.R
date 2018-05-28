## This file creates "fisher_table_values.txt" which holds suitably
## rounded numerical values of critical points of the F-distribution.
## File fisher_table_values.txt is imported by file table_fisher.tex

## File fisher_diagram.pdf is created by fisher_diagram.R



library("magrittr")
filename <- "fisher_table_values.txt"

num <- c(1,2,3,4,5,10,15,20,30)
den <- c(1:20,25,30,40,50,Inf)

x  <-
  expand.grid(num,den)                             %>% 
  apply(1,function(x){qf(0.95, df1=x[1],df=x[2])}) %>%
  matrix(ncol=length(num),byrow=TRUE)              %>%
  round(2)

x[x>  1] %<>% round(2) # two decimal places
x[x> 10] %<>% round(1) # only one decimal place for values >10
x[x>100] %<>% round(0) # closest integer sufficient!

formatted <- function(x){  # different formatting for one-digit and two-digit values
  out <- rep('na',length(x))
  out[x>=100] <- sprintf("%4.0f",x[x>=100])
  out[x< 100] <- sprintf("%4.1f",x[x< 100])
  out[x<  10] <- sprintf("%5.2f",x[x<  10])
  out[x<   1] <- sprintf("%4.3f",x[x<   1])
  return(out)
}

jj <- as.character(den)
jj[jj=="Inf"] <- "$\\infty$"
rownames(x) <- jj
colnames(x) <-  num

write("\\multirow{25}{*}{\\rotatebox{90}{Denominator degrees of freedom}}",file=filename,append=FALSE)
for(i in seq_len(nrow(x))){
  text_rows <- formatted(x[i,])
  bodyline <- paste("& {\\large ",rownames(x)[i],"}&",collapse="")  %>%
    paste(paste(text_rows,collapse="&"))
  if((i>1) & (i%%5==0)){
    bodyline %<>% paste("\\\\[3mm]")
  } else {
    bodyline %<>% paste("\\\\")
  }
  write(bodyline,file=filename,append=TRUE)
}

