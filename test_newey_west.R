require(foreign)
require(plm)
require(lmtest)
test <- read.dta("http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.dta")
test <- test[1:100,]
fpmg <- pmg(y~x, test, index=c("year","firmid")) ##Fama-MacBeth

fpmg.coefficients <- fpmg$coefficients
coeftest(fpmg)

the.years <- unique(test$year)
a.formula <- y ~ x


first.step <-  lapply(the.years, function(a.year) {
  temp.data <- test[test$year == a.year, ]
  an.lm <- lm(a.formula, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.year, t(the.coefficients)))
  the.results
}) 

first.step.df <- do.call('rbind', first.step)

second.step.coefficients <- apply(first.step.df[, -1], 2, mean)
second.step.coefficients
# (Intercept)           x 
#  0.03127797  1.03558610 

identical(fpmg.coefficients, second.step.coefficients)
# [1] TRUE

library(sandwich)
second.step.NW.sigma.sq <- apply(first.step.df[, -1], 2, 
                                 function(x) sqrt(NeweyWest(lm(x ~ 1), 
                                                            lag = 1, prewhite = FALSE)['(Intercept)',       
                                                                                       '(Intercept)']))
second.step.NW.sigma.sq
#  (Intercept)            x 
#   0.02438398   0.02859447

t.statistics.NW.lag.1 <- second.step.coefficients / second.step.NW.sigma.sq

t.statistics.NW.lag.1
# (Intercept)           x 
#    1.282726   36.216301
coeftest(lm(first.step.df$'(Intercept)' ~ 1), vcov = NeweyWest(lm(first.step.df$'(Intercept)' ~ 1), lag = 1, prewhite = FALSE))
# t test of coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) 0.031278   0.024384  1.2827   0.2316
coeftest(lm(first.step.df$x ~ 1), vcov = NeweyWest(lm(first.step.df$x ~ 1), lag = 1, prewhite = FALSE))
