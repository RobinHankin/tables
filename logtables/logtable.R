## Makes R variables table_main and table_Delta which are the main and
## Delta parts of the log table.  Also makes simple_main and
## simple_Delta which is the same but without the split rows of
## table_main.


rm(list=ls())

showdebug <- FALSE

log <- function(...){stop("do not use log(), use log10() here")}

func <- log10

tablevalue <- function(x){ # try x=1.31, table gives .1173 [that is, the numerical equivalent of the table entry)
  tableentry(x)/10000
}

tableentry <- function(x,numerical=TRUE){ # try x=1.32, table entry is "1206" (that is, the actual table entry, notional integer)
  out <- round(func(x)*10000)
  if(numerical){
    return(out)
  } else {
    return(noquote(sprintf("%04i",out)))
  }
}

tablevalue_delta <- function(x,Delta){# try x=1.31, delta=3, table
                                        # gives 1173+3=1176 -> 0.1176
                                        # returned
  tablevalue(x) + Delta/10000  
}

tableerror <- function(x, third_digit, Delta){  
 ## for 1.326, use x=1.32,third_digit=6.  Function returns the
 ## numerical difference between the true value and the table value.
  true_value <- func(x+third_digit/1000)
  table_value <- tablevalue_delta(x,Delta=Delta)
  return(table_value-true_value)
}

error <- function(x,third_digit,Delta){
  ## Returns the error from each of a series of x values.  Try
  ## error(x=seq(from=1.3,by=0.01,to=1.34),third_digit=5,Delta=16)
  sapply(x,function(x){tableerror(x, third_digit=third_digit, Delta=Delta)})
}


badness <- function(x,third_digit,Delta,measure){
  error <- error(x,third_digit,Delta)
  switch(measure,
         max=max(abs(error)),      # max = Maximum error
         mse=sqrt(mean(error^2)),  # mse = Mean Square Error
         mad=mean(abs(error))      # mad = Mean Absolute Deviation
         )
}

differences <- function(x,show=FALSE){   
  ## Finds the best value of Delta [it tries everything from 0 to 40],
  ## with respect to the different badness measures above.

  third_digit <- 1:9
  max <- sapply(third_digit,function(d){which.min(sapply(0:40,function(Delta){badness(x,d,Delta,'max')}))-1})
  mse <- sapply(third_digit,function(d){which.min(sapply(0:40,function(Delta){badness(x,d,Delta,'mse')}))-1})
  mad <- sapply(third_digit,function(d){which.min(sapply(0:40,function(Delta){badness(x,d,Delta,'mad')}))-1})

  ## NB: in the above three lines, the "-1" is because we start at
  ## zero [in 0:40], not one [as in 1:40]; it possible for the optimal
  ## Delta to be zero, and indeed this is the case for third_digit=1
  ## if x\geqapprox 8.9 

  ## Take max as an example.  'max' is a vector of 9 entries showing
  ## the optimal value of Delta for third_digit = 1,2,...,9 [here,
  ## 'optimal' means 'value of Delta that mimimizes the max() of the
  ## absolute error values'].

  
  out <- rbind(max,mse,mad)
  colnames(out) <- as.character(1:9)
  out <- rbind(out,range=apply(out,2,function(x){max(x)-min(x)}))

  jj <- function(x){round(x*1e5)}

  if(show){
    out <- rbind(out,
                 max_bad = jj(sapply(1:9,function(i){badness(x,i,max[i],'max')})),
                 mse_bad = jj(sapply(1:9,function(i){badness(x,i,mse[i],'mse')})),
                 mad_bad = jj(sapply(1:9,function(i){badness(x,i,mad[i],'mad')}))
                 )
  }
  return(out)
}


di <- function(x,l,give=FALSE){
  x <- seq(from=x,by=0.01,len=l)
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

process_rownames <- function(x){ # turns "1" into "1.0", leaves "1.05"
                                 # as "1.05", leaves "1.3" as "1.3" for use in the split row table


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
