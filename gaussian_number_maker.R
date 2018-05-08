library("magrittr")
x <- 
  seq(from=0, by=0.01,to=2.99) %>%
  pnorm                        %>%
  round(4)                     %>%
  sprintf("%1.4f", .)          %>%
  sub("0." ,".",.)             %>%
  matrix(byrow=TRUE,ncol=10)   %>%
  noquote

rownames(x) <-
  seq(from=0,to=2.9,by=0.1)    %>%
  sprintf("%1.1f", .)       


colnames(x) <- 
  seq(from=0,to=0.09,by=0.01)  %>%
  sprintf("%1.2f", .)         


