## This file creates "student_table_values.txt" which holds suitably
## rounded numerical values of critical points of the student "t"
## distribution.  File student_table_values.txt is imported by file
## table_student.tex

## File student_diagram.pdf is created by gaussian_diagram.R



library("magrittr")
filename <- "student_table_values.txt"

p <- c(0.9,0.95,0.975,0.99,0.995)
df <- c(1:29,Inf)

x  <-
  expand.grid(p,df)                      %>% 
  apply(1,function(x){qt(x[1],df=x[2])}) %>%
  matrix(ncol=length(p),byrow=TRUE)

formatted <- function(x){  # different formatting for one-digit and two-digit values
  out <- rep('na',length(x))
  out[x<  1] <- sprintf("%4.3f",x[x<  1])
  out[x< 10] <- sprintf("%4.2f",x[x< 10])
  out[x>=10] <- sprintf("%4.1f",x[x>=10])
  return(out)
}

jj <- as.character(df)
jj[jj=="Inf"] <- "$\\infty$"
rownames(x) <- jj
colnames(x) <-  p

write("%this file is not human-readable, it was created by 'student_number_maker.R'",file=filename,append=FALSE)

for(i in seq_len(nrow(x))){
  text_rows <- formatted(x[i,])
  bodyline <- paste("{\\large ",rownames(x)[i],"}&",collapse="")  %>%
    paste(paste(text_rows,collapse="&"))
  if((i>1) & (i%%5==0)){
    bodyline %<>% paste("\\\\[3mm]")
  } else {
    bodyline %<>% paste("\\\\")
  }
  write(bodyline,file=filename,append=TRUE)
}




