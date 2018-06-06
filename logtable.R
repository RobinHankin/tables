## This file is designed to be called by "log_number_maker.R".

## Makes R variables 'table_main' and 'table_Delta' (both matrices),
## which are the main and Delta parts of the log table.  Also makes R
## variables 'simple_main' and 'simple_Delta' which are the same but
## without the split rows of 'table_main'.

## These four matrices are numeric, not string, so need to be
## processed to (eg) turn integer 86 into "0086" which would appear in
## the finished table.

## I am assuming that we know how log tables are used.

## The documentation of this file includes some overlap with that in
## antilogtable.R, but the two files are sufficiently different to be
## considered separately.



showdebug <- FALSE

log <- function(...){stop("do not use log(), use log10() here")}

func <- log10

tableentry <- function(x,numerical=TRUE){
 ## try x=1.32, table entry is "1206" (that is, the actual table
 ## entry, as it actually appears on the table; notionally an integer)

  out <- round(func(x)*10000)
  if(numerical){
    return(out)
  } else {
    return(noquote(sprintf("%04i",out)))
  }
}

tablevalue <- function(x){
  ## try x=1.31, table gives .1173 [that is, the numerical equivalent
  ## of the table entry as given by tableentry()

  tableentry(x)/10000
}


tablevalue_delta <- function(x, Delta){
  ## try x=1.31, delta=3, table gives 1173+3=1176 -> 0.1176 returned

  tablevalue(x) + Delta/10000  
}

tableerror <- function(x, third_digit, Delta){  
  ## for 1.326, use x=1.32, third_digit=6.  Function returns the
  ## *numerical* difference between the true value and the value given
  ## by the table.

  true_value <- func(x+third_digit/1000)
  table_value <- tablevalue_delta(x,Delta=Delta)
  return(table_value-true_value)
}

error <- function(x,third_digit,Delta){
  ## Returns the error from each of a series of x values.  Try
  ## error(x=seq(from=1.3,by=0.01,to=1.34),third_digit=5,Delta=16) and
  ## compare this with
  ## error(x=seq(from=1.3,by=0.01,to=1.34),third_digit=5,Delta=17) to
  ## see whether Delta=16 is better or worse than Delta=17 for the "5"
  ## entry of the differences on the "1.3" row of the table.  See how
  ## a *single* Delta value (here we are choosing between 16 and 17)
  ## gives rise to 10 different errors, one for each of 1.30 -> 1.305,
  ## 1.31 -> 1.315, 1.32 -> 1.325,..., 1.39 -> 1.395.

  sapply(x,function(x){tableerror(x, third_digit=third_digit, Delta=Delta)})
}

badness <- function(x,third_digit,Delta,measure){
  
  ## As per the comments in error(), any Delta value [we were
  ## comparing Delta=16 and Delta=17 above, for the '5' entry on the
  ## '1.3' row] has associated with it 10 distinct errors, one for
  ## each column of its row.  To choose a particular value of Delta,
  ## here to choose whether 16 is preferable to 17, we need to
  ## summarize the 10 error values.  We can do this either by
  ## returning the maximum absolute error ('max'), the root mean
  ## square error ('mse'), or the mean absolute deviation ('mad').
  ## Function badness() returns either the max, mse, or mad as
  ## required.  Note that these three different summary methods give
  ## different measures of badness, and this means that the value of
  ## Delta might differ between max,mse, and mad.
  
  error <- error(x,third_digit,Delta)
  switch(measure,
         max=max(abs(error)),      # max = Maximum error
         mse=sqrt(mean(error^2)),  # mse = Mean Square Error
         mad=mean(abs(error))      # mad = Mean Absolute Deviation
         )
}

differences <- function(x,show=FALSE){   
  ## Given a particular value of x, which specifies a row of the
  ## table, function differences() finds the "best" value of Delta [it
  ## tries everything from Delta=0 to Delta=40] with respect to the
  ## three different badness measures above.  Here "best" is defined
  ## as "the value of Delta that minimizes the badness".

  third_digit <- 1:9
  max <- sapply(third_digit,function(d){which.min(sapply(0:40,function(Delta){badness(x,d,Delta,'max')}))-1})
  mse <- sapply(third_digit,function(d){which.min(sapply(0:40,function(Delta){badness(x,d,Delta,'mse')}))-1})
  mad <- sapply(third_digit,function(d){which.min(sapply(0:40,function(Delta){badness(x,d,Delta,'mad')}))-1})

  ## NB: in the above three lines, "0:40" is the values of Delta that
  ## we are looking at.  NB: the "-1" is because we start at zero
  ## [i.e.  "0:40"], not one [which would be "1:40"]. This is because
  ## it is possible for the optimal Delta to be zero, and indeed this
  ## is the case for third_digit=1 if x\geqapprox 8.9

  ## Take max as an example.  'max' is a vector of 9 entries showing
  ## the optimal value of Delta for third_digit = 1,2,...,9 [here,
  ## 'optimal' means 'value of Delta that mimimizes the max() of the
  ## absolute error values'].
  
  out <- rbind(max,mse,mad)
  colnames(out) <- as.character(third_digit)
  out <- rbind(out,range=apply(out,2,function(x){max(x)-min(x)}))

  jj <- function(x){round(x*1e5)}

  if(show){
    out <- rbind(out,
                 max_bad = jj(sapply(third_digit,function(i){badness(x,i,max[i],'max')})),
                 mse_bad = jj(sapply(third_digit,function(i){badness(x,i,mse[i],'mse')})),
                 mad_bad = jj(sapply(third_digit,function(i){badness(x,i,mad[i],'mad')}))
                 )
  }
  return(out)
}

di <- function(x,l,give=FALSE,norm_choice=1){ 
  ## Function di() is a cut-down version of differences() which
  ## returns a list of length two, the first element of which is the
  ## main table entry for x, the second is the Delta entries.

  ##  Argument 'l' is the length of the sequence; l=10 for the full
  ##  lines but l=5 for the split entries at the top.

  ## there is more documentation in di() of antilogtable.R, which
  ## discusses the norm_choice argument.
  
  x <- seq(from=x,by=0.01,len=l)
  main_table <- tableentry(x)
  names(main_table) <- x

  if(give){
    Delta <-  differences(x)
  } else {
    Delta <-  differences(x)[norm_choice,]  ## Choose the max() badness measure
  }
  list(
      main_table = main_table,
      Delta = Delta
  )
}

process_rownames <- function(x){
  ## Function process_rownames() makes the rownames suitable for
  ## passing to LaTeX.  It turns "1" into "1.0", leaves "1.05" as
  ## "1.05", leaves "1.3" as "1.3" for use in the split row table

  out <- as.character(x)
  odd <- round(x*100)%%10 != 0
  out[odd] <- " "
  out[!odd] <- sprintf("%1.1f",x[!odd])
  
  return(out)
}

if(showdebug){
  x <- seq(from=2.4,by=0.01,len=10)
  dd <- differences(x)
  print(dd)
  
  print(di(1.3,5))
  print(di(3.4,10))
}


## Now make the table, variable 'maintable', differences is 'Delta'

xsplit <- seq(from=1.0,to=1.35,by=0.05) # split rows
xfull <- seq(from=1.4,to=9.9,by=0.1) # full rows

table_main <- matrix(NA,length(xsplit),10)  
table_Delta <- matrix(NA,0,9)    # sic

## First, do the split rows:
for(i in seq_along(xsplit)){
  jj <- di(xsplit[i],5)
  if(i%%2==1){
    indices <- 1:5
  } else {
    indices <- 6:10
  }

  table_main[i,indices] <- jj$main_table
  table_Delta <- rbind(table_Delta,jj$Delta)
}

## Now the full rows:
for(i in seq_along(xfull)){
  jj <- di(xfull[i],10)
  table_main <- rbind(table_main,jj$main_table)
  table_Delta <- rbind(table_Delta,jj$Delta)
}

rownames(table_main) <- process_rownames(c(xsplit,xfull))
rownames(table_Delta) <- rownames(table_main)
colnames(table_main) <- 0:9
colnames(table_Delta) <- 1:9


## Now make the simple table:

x <- seq(from=1,to=9.9,by=0.1)
simple_main <- matrix(NA,length(x),10)
simple_Delta <- matrix(NA,length(x),9)

## For simple_main, all rows are full:
for(i in seq_along(x)){
  jj <- di(x[i],10)
  simple_main[i,] <- jj$main_table
  simple_Delta[i,] <- jj$Delta
}

rownames(simple_main) <- process_rownames(x)
rownames(simple_Delta) <- rownames(simple_main)
colnames(simple_main) <- 0:9
colnames(simple_Delta) <- 1:9

save(table_main,table_Delta, simple_main,simple_Delta, file="log.Rdata")


