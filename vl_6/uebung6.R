library(faraway)
library(mice)
library(naniar)
library(mi)
library(tidyr)
library(dplyr)

pima <- faraway::pima



#replace 0 with NA
pima <- pima %>% 
  mutate(across(c(glucose, diastolic, triceps, insulin, bmi,
                  diabetes, age), ~ na_if(., 0)))

pct_miss(pima)
pct_miss_case(pima)  
pct_complete_case(pima)


gg_miss_var(pima, show_pct = TRUE)

#1.1

dim(pima)

dim(na.omit(pima))

#1.2

mdf <- missing_data.frame(pima)

show(mdf)
image(mdf)


md.pattern(pima, rotate.names = T)

#2.1 mi 


options(mc.cores = 4)

imputations <- mi(mdf, n.iter = 30, n.chains = 4, max.minutes = 20)
show(imputations)


round(mipply(imputations, mean, to.matrix = TRUE), 3)

Rhats(imputations)

imputations <- mi(imputations, n.iter = 5)

plot(imputations)







