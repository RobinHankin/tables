## This file is an modification of logtable.R but adapted for antilogs.

## This file is designed to be called by "antilog_number_maker.R".

## Makes R variables 'table_main' and 'table_Delta' (both matrices),
## which are the main and Delta parts of the antilog table.  These
## variables correspond to the simple version of the log table (that
## is, the version that does not have split lines for the rapidly
## varying PPs).

## Antilog tables have some subtleties.  Take '0.55' as an example.
## The table entry is '3548' and the corresponding mathematical
## statement is '10^0.55 = 3.548' although it might be better to think
## in terms of the barred version: '10^{\overline{1}.55} = 0.3548'.

## I am assuming that we know how antilog tables are used.


showdebug <- TRUE

func <- function(x){10^x} 

tableentry <- function(x,numerical=TRUE){
    ## try x=0.55; the table entry is "3548" (that is, the actual table
    ## entry, as it actually appears on the table; notionally an integer)
    
    ## > tableentry(0.55)
    ## [1] 3548
    ## > 
    
    out <- round(func(x)*1000)
    if(numerical){
        return(out)
    } else {
        return(noquote(sprintf("%04i",out)))
    }
}

tablevalue <- function(x){
    ## try x=0.65, table entry is "4467" and tablevalue(0.65) is the
    ## numerical value of this, which is 4.467, that is, '10^0.65 =
    ## 4.467'.
    
    ## > tablevalue(0.65)
    ## [1] 4.467
    ## > 

    tableentry(x)/1000
}


tablevalue_delta <- function(x, Delta){
    ## try x=0.502, delta=6, table gives 3177+6=3183 -> 3.183 returned

    ## > tablevalue_delta(0.502,6)
    ## [1] 3.183
    ## > 

  tablevalue(x) + Delta/1000
}


tableerror <- function(x, fourth_digit, Delta){  
    ## for 0.5127 use x=0.512, fourth_digit=7.  Function tableerror()
    ## returns the *numerical* difference between the true value and
    ## the value given by the table.
    
    ## > tableerror(0.502, 7,4)
    ## [1] -0.0009987155
    ## > tableerror(0.502, 7,5)    # this is the best value!
    ## [1] 1.28447e-06
    ## > tableerror(0.502, 7,6)
    ## [1] 0.001001284
    ## > 


  true_value <- func(x+fourth_digit/10000)
  table_value <- tablevalue_delta(x,Delta=Delta)
  return(table_value-true_value)
}

error <- function(x,fourth_digit,Delta){
    
    ## Returns the error from each of a series of x values.  Try
    ## error(x=seq(from=0.510,by=0.001,to=0.519),fourth_digit=8,Delta=7)
    ## and compare this with
    ## error(x=seq(from=0.510,by=0.001,to=0.519),fourth_digit=8,Delta=8)
    ## to see whether Delta=6 is better or worse than Delta=7 for the
    ## "8" entry of the differences on the ".51" row of the table.

    ## > error(x=seq(from=0.510,by=0.001,to=0.519),fourth_digit=8,Delta=7)
    ## [1] 0.0010971 0.0006237 0.0011331 0.0006253 0.0011001
    ## [6] 0.0005576 0.0009977 0.0014204 0.0008256 0.0012133
    ## > error(x=seq(from=0.510,by=0.001,to=0.519),fourth_digit=8,Delta=8)
    ## [1] 0.0020971 0.0016237 0.0021331 0.0016253 0.0021001 0.0015576
    ## [7] 0.001997 0.00242043 0.0018256 0.0022133
    ## >
    ## (above values slightly abridged)

    
    ## See how a *single* Delta value (here we are choosing between 7
    ## and 8) gives rise to 10 different errors, one for each of 0.510
    ## -> 0.5108, 0.511 -> 0.5118, 0.512 -> 0.5128, ..., 0.519 -> 0.5198
    
    sapply(x,function(x){tableerror(x, fourth_digit=fourth_digit, Delta=Delta)})
}


badness <- function(x,fourth_digit,Delta,measure){
  
    ## As per the comments in error(), any Delta value [we were
    ## comparing Delta=7 and Delta=8 above, for the '8' entry on the
    ## '0.51' row] has associated with it 10 distinct errors, one for
    ## each column of its row.  To choose a particular value of Delta,
    ## here to choose whether 7 is preferable to 8, we need to
    ## summarize the 10 error values.  See badness() in logtable.R for
    ## more discussion, which is the same as it would be here.

    ## > badness(seq(from=0.510,to=0.519,by=0.001),3,8,'max')
    ## [1] 0.00621128
    ## > badness(seq(from=0.510,to=0.519,by=0.001),3,8,'mse')
    ## [1] 0.005735551
    ## > badness(seq(from=0.510,to=0.519,by=0.001),3,8,'mad')
    ## [1] 0.005728606
    ## > 

    ## In the above, note the 'by=0.001', which is different from the
    ## corresponding part of logtable.R, which uses 'by=0.01'
    
    
    error <- error(x,fourth_digit,Delta)
    switch(measure,
           max=max(abs(error)),      # max = Maximum error
           mse=sqrt(mean(error^2)),  # mse = Mean Square Error
           mad=mean(abs(error))      # mad = Mean Absolute Deviation
           )
}

differences <- function(x,show=FALSE){## Given a particular value of x, which specifies a row of the
    ## table, function differences() finds the "best" value of Delta [it
    ## tries everything from Delta=0 to Delta=40] with respect to the
    ## three different badness measures above.  Here "best" is defined
    ## as "the value of Delta that minimizes the badness".
    
    ## Thus:

    
    ## > differences(seq(from=0.910,to=0.919,by=0.001))
    ##       1 2 3 4  5  6  7  8  9
    ## max   2 4 6 8  9 11 13 15 17
    ## mse   2 4 6 8 10 11 13 15 17
    ## mad   2 4 6 8 10 11 13 15 17
    ## range 0 0 0 0  1  0  0  0  0
    ## > 

    ## In the above, we see that the three badness() methods agree on
    ## the best value of the PPs for every value of fourth_digit,
    ## except for 5.  In this case, mse and mad give '10' and 'max'
    ## gives '9'.

    ## We can get a little more insight by using the show=TRUE
    ## argument, which gives numerical indication of the three
    ## badnesses:

    ## > differences(seq(from=0.910,to=0.919,by=0.001),show=TRUE)
    ##            1    2    3    4    5    6    7    8    9
    ## max        2    4    6    8    9   11   13   15   17
    ## mse        2    4    6    8   10   11   13   15   17
    ## mad        2    4    6    8   10   11   13   15   17
    ## range      0    0    0    0    1    0    0    0    0
    ## max_bad 5954 7058 8158 9254 9592 8680 7772 6870 5971
    ## mse_bad 3746 4085 4657 5386 6214 5520 4835 4311 4007
    ## mad_bad 3407 3437 3624 3976 4937 4398 3960 3749 3651
    ## >

    ## NB: the three rows at the bottom are not strictly comparable
    ## with one another, only row-wise comparison is possible.  Each
    ## entry shows the badness of the least bad choice.  It is not a
    ## very useful thing; it would have been better to display the
    ## badnesses of the different choices.
    
    fourth_digit <- 1:9
    max <- sapply(fourth_digit,function(d){which.min(sapply(0:40,function(Delta){badness(x,d,Delta,'max')}))-1})
    mse <- sapply(fourth_digit,function(d){which.min(sapply(0:40,function(Delta){badness(x,d,Delta,'mse')}))-1})
    mad <- sapply(fourth_digit,function(d){which.min(sapply(0:40,function(Delta){badness(x,d,Delta,'mad')}))-1})
    
    ## NB: in the above three lines, "0:40" is the values of Delta that
    ## we are looking at.  NB: the "-1" is because we start at zero
    ## [i.e.  "0:40"], not one [which would be "1:40"].  This is because
    ## it is possible for the optimal Delta to be zero.
    
    ## Take max as an example.  'max' is a vector of 9 entries showing
    ## the optimal value of Delta for fourth_digit = 1,2,...,9 [here,
    ## 'optimal' means 'value of Delta that mimimizes the max() of the
    ## absolute error values'].
   
  
    out <- rbind(max,mse,mad)
    colnames(out) <- as.character(fourth_digit)
    out <- rbind(out,range=apply(out,2,function(x){max(x)-min(x)}))
    
    jj <- function(x){round(x*1e7)}
    
    if(show){
        out <- rbind(out,
                     max_bad = jj(sapply(fourth_digit,function(i){badness(x,i,max[i],'max')})),
                     mse_bad = jj(sapply(fourth_digit,function(i){badness(x,i,mse[i],'mse')})),
                     mad_bad = jj(sapply(fourth_digit,function(i){badness(x,i,mad[i],'mad')}))
                     )
    }
    return(out)
}



di <- function(x,l,give=FALSE){ 
    ## Function di() is a cut-down version of differences() which
    ## returns a list of length two, the first element of which is the
    ## main table entry for x, the second is the Delta entries.
    
    ## There is no facility for split lines here.


    ## > di(0.54,10)
    ## $main_table
    ## 0.54 0.541 0.542 0.543 0.544 0.545 0.546 0.547 0.548 0.549 
    ## 3467  3475  3483  3491  3499  3508  3516  3524  3532  3540 
    ## 
    ## $Delta
    ## 1 2 3 4 5 6 7 8 9 
    ## 1 2 2 3 4 5 6 6 7 
    ## >
    
    x <- seq(from=x,by=0.001,len=l)
    main_table <- tableentry(x)
    names(main_table) <- x
    
    if(give){
        Delta <-  differences(x)
    } else {
        Delta <-  differences(x)[1,]  ## Choose the max() badness measure
    }
    list(
        main_table = main_table,
        Delta = Delta
    )
}

process_rownames <- function(x){

    ## Function process_rownames() makes the rownames suitable for
    ## passing to LaTeX.  It turns "0.5" into "0.50", but it is a lot
    ## simpler than the equivalent in logtable.R.

    sprintf("%1.2f",x)
}

if(showdebug){
  x <- seq(from=0.53,by=0.001,len=10)
  dd <- differences(x)
  print(dd)
  
  print(di(0.54,10))
}


## Now make the table, variable 'maintable', differences is 'Delta'.
## There is no mucking about with split lines.

xfull <- seq(from=0.00,to=0.99,by=0.01) # all rows are full rows here.

antilogtable_main  <- matrix(NA,length(xfull),10)
antilogtable_delta <- matrix(NA,length(xfull), 9)  #sic

for(i in seq_along(xfull)){
    jj <- di(xfull[i],10)
    antilogtable_main [i,] <- jj$main_table
    antilogtable_delta[i,] <- jj$Delta
}

rownames(antilogtable_main ) <- process_rownames(xfull)
rownames(antilogtable_delta) <- rownames(antilogtable_main)
colnames(antilogtable_main ) <- 0:9
colnames(antilogtable_delta) <- 1:9


save(table_main,table_Delta, file="antilog.Rdata")


