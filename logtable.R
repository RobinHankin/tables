## This file is designed to be called by "log_number_maker.R".

## It takes about 10 seconds to run.

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
 ## Example: x=1.32, we have log10(x)=0.1205739 from R; table entry is
 ## "1206" (that is, the actual table entry, as it actually appears on
 ## the table; notionally an integer)

  out <- round(func(x)*10000)
  if(numerical){
    return(out)
  } else {
    return(noquote(sprintf("%04i",out)))
  }
}

tablevalue <- function(x){
  ## Example: x=1.31, we have log10(x)=0.1172713 from R; table entry
  ## of "1173" would be interpreted as 0.1173 [that is, the numerical
  ## equivalent of the table entry as given by tableentry()]

  tableentry(x)/10000
}

tablevalue_delta <- function(x, Delta){
  ## This function evaluates the effect of a suggested Delta value in
  ## the differences part of the table.  The idea is that we will try
  ## different values of Delta and see which value is the "best", as
  ## measured by badness().  For example, tablevalue_delta(x=1.31,
  ## Delta=8) returns the table's value at the point x=1.31, if the
  ## value of Delta used is 8.  The table gives "1173" for x=1.31, and
  ## if Delta=8 this is 1173+8=1181, which would mean that 0.1181 is
  ## given by the table.

  tablevalue(x) + Delta/10000
}

tableerror <- function(x, third_digit, Delta){
    ## This function calculates the error induced by using the table for
    ## a particular value of x and third_digit, when using a particular
    ## value of Delta.  We will try different values of Delta and see
    ## which one is the "best".

    ## For example, consider

    ## tableerror(x=1.32, third_digit=4, Delta=19)

    ## This means we are considering the log10 of 1.324=0.121888.  The
    ## table would give "1206" with a delta of 19 which would
    ## correspond to 1206+19=1225, with a numerical value of 0.1225

    ## So the difference between the true value and the value given by
    ## the table would be 0.121888-0.1225= -0.000612

  true_value <- func(x+third_digit/1000)
    table_value <- tablevalue_delta(x,Delta=Delta)
    return(table_value-true_value)
}

error <- function(x,third_digit,Delta){

    ## Returns the error from each of a series of x values: given a
    ## particular value of Delta, error() returns the difference between
    ## the true value of log(x) and the value given by the table.

    ## For example, consider:

    ## error(x=seq(from=1.3,by=0.01,to=1.34),third_digit=5,Delta=16)

    ## and compare this with

    ## error(x=seq(from=1.3,by=0.01,to=1.34),third_digit=5,Delta=17)

    ## so we can see whether Delta=16 is better or worse than Delta=17
    ## for the "5" entry of the differences on the "1.3" row of the
    ## table.

    ##  error(x=seq(from=1.3,by=0.01,to=1.34),third_digit=5,Delta=16)
    ## [1] -1.1051e-04 -2.5752e-05 -1.5878e-05  1.8734e-05 -2.2284e-05
    ## >  error(x=seq(from=1.3,by=0.01,to=1.34),third_digit=5,Delta=17)
    ## [1] -1.0511e-05  7.4247e-05  8.4121e-05  1.1873e-04  7.7715e-05

    ## (above cut-n-pasted but slightly edited for clarity).  More
    ## explicitly, we would have, for Delta=16, the following:

    ## 1.30 -> 1.305: 1139+16=1155; 0.1155-log10(1.305) = -0.0001105117
    ## 1.31 -> 1.315: 1173+16=1189; 0.1189-log10(1.315) = -0.0000257528
    ## 1.32 -> 1.325: 1206+16=1222; 0.1222-log10(1.325) = -0.0000158782
    ## 1.33 -> 1.335: 1239+16=1255; 0.1255-log10(1.335) = +0.0001873430
    ## 1.34 -> 1.345: 1271+16=1287; 0.1287-log10(1.345) = -0.0000228434

    ## See how a *single* Delta value (here we are using 16) gives
    ## rise to 5 different errors when using a third digit of 5, one
    ## for each the five cases it would be used.  In this case have
    ## four negative errors and one positive.  From the R session
    ## above, with Delta=17 we have one negative and four positive
    ## errors.

  sapply(x,function(x){tableerror(x, third_digit=third_digit, Delta=Delta)})
}

badness <- function(x,third_digit,Delta,measure){

    ## As per the comments in error(), any Delta value [we were
    ## comparing Delta=16 and Delta=17 above, for the '5' entry on the
    ## '1.3' row] has associated with it 5 or 10 distinct errors, one
    ## for each column of its row.  To choose a particular value of
    ## Delta, for example to choose whether 16 is preferable to 17, we
    ## need to summarize *all* the error values associated with the
    ## different values of Delta.  We can do this either by returning
    ## the maximum absolute error ('max'), the root mean square error
    ## ('mse'), or the mean absolute deviation ('mad').  Function
    ## badness() returns either the max, mse, or mad as required.
    ## Note that these three different summary methods give different
    ## measures of badness, and this means that the value of Delta
    ## might differ between max,mse, and mad.

  error <- error(x,third_digit,Delta)
  switch(measure,
         max=max(abs(error)),      # max = Maximum error
         mse=sqrt(mean(error^2)),  # mse = Mean Square Error
         mad=mean(abs(error))      # mad = Mean Absolute Deviation
         )
}

differences <- function(x,show=FALSE){

    ## Given a particular value of x, which specifies a row of the
    ## table, function differences() finds the "best" values to use
    ## for the entry in the differences section of the table [it tries
    ## everything from Delta=0 to Delta=40] with respect to the three
    ## different badness measures above.  Here "best" is defined as
    ## "the value of Delta that minimizes the badness".

    ## For example, suppose we are wondering what differences to use
    ## in the 1.05-1.09 (half) line of the log table.  The Deltas need
    ## to be good (ie low badness()) for all five numbers 1.05-1.09:
    
    ##    R> differences(seq(from=1.05,to=1.09,by=0.001))
    ##       1 2  3  4  5  6  7  8  9
    ## max   4 8 12 16 20 25 29 33 37
    ## mse   4 8 12 16 20 24 28 32 36
    ## mad   4 8 12 16 20 24 28 32 36
    ## range 0 0  0  0  0  1  1  1  1
    
    ## If we want to use 'max' as a measure of badness, we use the
    ## first row of the output in the table, which would be

    ##  4 8 12 16 20 25 29 33 37

    ## We see that the different badness measures give slightly
    ## different results, with disagreement of one unit for 6-9.
    ## Passing show=TRUE gives a little more information:

    ##    R> differences(seq(from=1.05,to=1.09,by=0.001),show=TRUE)
    ##         1 2  3  4  5  6  7  8  9
    ## max     4 8 12 16 20 25 29 33 37
    ## mse     4 8 12 16 20 24 28 32 36
    ## mad     4 8 12 16 20 24 28 32 36
    ## range   0 0  0  0  0  1  1  1  1
    ## max_bad 6 6  7  9 10 11 10 10 10
    ## mse_bad 3 3  3  4  4  5  5  6  6
    ## mad_bad 2 3  3  3  3  4  4  4  5

    ## In the above, the last three lines show the worst (ie highest)
    ## badness score across the five numbers x <-
    ## seq(from=1.05,to=1.09,by=0.0001) so we can get some insight
    ## into how the badnesses are distributed across x
    
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

  ## Argument 'norm_choice' specifies which norm to use in badness().
  ## It specifies which row of the output of differences() to use, so
  ## currently norm_choice=1 gives max, 2 gives mse, and 3 gives mad.

  ## To reproduce the 1.1 line (which is split) of the log table:

  ## R> di(1.1,5)
  ## $main_table
  ##  1.1 1.11 1.12 1.13 1.14 
  ##  414  453  492  531  569 
  ##
  ## $Delta
  ##  1  2  3  4  5  6  7  8  9 
  ##  4  8 12 15 19 23 27 31 35 
  
  ## So the above gives the main body of the table ("0414 0453 0492
  ## 0531 0569") together with the differences ("4 8 12...35").  The
  ## norm_choice argument of the di() function specifies which row of
  ## the output of differences() to use.  The rows are max, mse, mad;
  ## these are defined in badness().  So the default of norm_choice=1
  ## corresponds to the first row, which is max.
  

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
