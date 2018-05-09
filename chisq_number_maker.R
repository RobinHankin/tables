library("magrittr")

expand.grid(1:30,c(0.9,0.95,0.975,0.99,0.995)) %>% 
apply(1,function(x){qchisq(x[2],df=x[1])})     %>%
matrix(ncol=5)                                 %>%
round(2)
