## This file creates "chisq_table_values.txt" which holds suitably
## rounded numerical values of critical points of the chisquare
## distribution.  File chisq_table_values.txt is imported by file
## table_chisq.tex


library("magrittr")
filename <- "chisq_table_values.txt"

p <- c(0.9,0.95,0.975,0.99,0.995)
df <- 1:30

x  <-
  expand.grid(p,df)                          %>% 
  apply(1,function(x){qchisq(x[1],df=x[2])}) %>%
  matrix(ncol=length(p),byrow=TRUE)  %>%
  round(2)


x[x>10] %<>% round(1) # only one decimal place for large values



rownames(x) <- as.character(df)
colnames(x) <-  p



write("",file=filename,append=FALSE)

for(i in seq_len(nrow(x))){
  bodyline <- paste("{\\large ",rownames(x)[i],"}&",collapse="")  %>%
    paste(paste(x[i,],collapse="&"))
  if((i>1) & (i%%5==0)){
    bodyline %<>% paste("\\\\[3mm]")
  } else {
    bodyline %<>% paste("\\\\")
  }
  write(bodyline,file=filename,append=TRUE)
}




