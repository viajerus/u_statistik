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

imput.mi.one <- mi::complete(imputations, 1)
summary(imput.mi.one)


#2.2: using mice

imputed.mice.mean <- mice::complete(mice(pima, method="mean", printFlag=F), m=5)[[1]]

imputed.mice.pmm <- mice::complete(mice(pima, method="pmm", printFlag=F), m=5)[[1]]

imputed.mice.rf <- mice::complete(mice(pima, method="rf", printFlag=F), m=5)[[1]]

cor(cbind(pima$insulin, imput.mi.one$insulin, imputed.mice.mean$insulin,
          imputed.mice.pmm$insulin), use="pairwise.complete.obs")


#3 Quantify effect of imputation on model estimates

pima.mice.rf <- mice(pima, method="rf", printFlag=F, m=10)
fits <- with(data=pima.mice.rf, exp=glm(test ~ pregnant + diastolic + triceps + bmi + age + insulin,
                                        family=binomial))
imputed.dats <- mice::complete(pima.mice.rf)
summary(glm(test ~ pregnant + diastolic + triceps + bmi + age + insulin,
            family=binomial, data=imputed.dats))


mice::pool(fits)






