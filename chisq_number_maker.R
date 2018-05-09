library("magrittr")

p <- c(0.9,0.95,0.975,0.99,0.995)
df <- 1:30

expand.grid(df,p)                          %>% 
apply(1,function(x){qchisq(x[2],df=x[1])}) %>%
matrix(ncol=length(df))                    %>%
round(2)
