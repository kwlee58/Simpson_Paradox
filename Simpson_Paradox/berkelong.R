### berkelong.R: Using Berkeley data to illustrate various
### manipulations for contingency tables in R, and fitting of logit
### models.
### 
### REQUIRES:  odds.ratio.R
###            adj.residuals.R
###
### Copyright © 2004 Brett Presnell
### 
### This work is licensed under the Creative Commons
### Attribution-NonCommercial-ShareAlike License. To view a copy of
### this license, visit
### 
###     http://creativecommons.org/licenses/by-nc-sa/1.0/
### 
### or send a letter to Creative Commons, 559 Nathan Abbott Way,
### Stanford, California 94305, USA.

## The Berkeley admissions data are so popular that they are one of
## the datasets distributed with R.

data(UCBAdmissions)
objects()

## The data are stored in a 6 x 2 x 2 "table", which is just an array
## of counts.

class(UCBAdmissions)
is.array(UCBAdmissions)
dim(UCBAdmissions)
dimnames(UCBAdmissions)
UCBAdmissions

## The ftable() function presents an array as a "flat table":

ftable(UCBAdmissions)
ftable(UCBAdmissions, row.vars="Dept")
ftable(UCBAdmissions, row.vars="Dept", col.vars = c("Gender", "Admit"))

###
### The margin.table() function is a simple way to compute various
### proportions.  It is a simple wrapper around the apply() function.
###
### The prop.table() function is a simple way to compute various
### proportions.  It is just a standard application of the sweep() and
### apply() functions, which are more general and versatile.
###

## Default behavior expresses each cell count as a proportion of the
## table total (not very interesting in most cases).

prop.table(UCBAdmissions)

## Same thing in a more readable format.

ftable(round(prop.table(UCBAdmissions), 3),
       row.vars="Dept", col.vars = c("Gender", "Admit"))

## More interesting are the proportions admitted for each Gender by
## Dept combination (dimensions 2 and 3 of the array).  Notice that
## male and female admission rates are about the same in all
## departments, except "A", where female admission rates are higher.

prop.table(UCBAdmissions, c(2,3))
ftable(round(prop.table(UCBAdmissions, c(2,3)), 2),
       row.vars="Dept", col.vars = c("Gender", "Admit"))

## Here's the sweep/apply equivalent:

sweep(UCBAdmissions, c(2,3), apply(UCBAdmissions, c(2,3), sum), "/")
ftable(round(sweep(UCBAdmissions, c(2,3),
                   apply(UCBAdmissions, c(2,3), sum), "/"), 2),
       row.vars="Dept", col.vars = c("Gender", "Admit"))

## And here are the estimated odds ratios for each department.
## Incrementing the counts by 0.5 makes very little difference here,
## because none of the counts are close to zero.

source("Src/odds.ratio.R")
round(apply(UCBAdmissions, 3, odds.ratio), 2)
round(apply(UCBAdmissions, 3, odds.ratio, addtocounts = 0.5), 2)

## As we would expect from the proportions above, all of these seem
## reasonably close to 1, except perhaps for department "A", which
## appears less likely to admit a male candidate than a female.  Are
## any of the others significantly different from zero?  Check by
## applying the chisquare test to the data for each department.

round(apply(UCBAdmissions, 3, function(x) chisq.test(x)$p.value), 4)

## Conclusion: department "A" is more likely to admit a female
## applicant than a male, and there is no evidence of any difference
## among the other departments.

## So how did the controversy arise?  Check the proportions of each
## gender admitted overall:

admit.gender <- margin.table(UCBAdmissions, c(1,2))
admit.gender
apply(UCBAdmissions, c(1,2), sum)  # The apply() equivalent.
round(prop.table(admit.gender, 2), 2)
summary(admit.gender)

## So, if we ignore department, it looks like males are more likely to
## be admitted (45%) than females (30%).  However, if we control for
## department as above, the admission rates are about the same for
## males and female in most departments, with females more likely than
## males to be admitted to department "A".  This is an example of
## Simpson's paradox.

## Can we explain what is happening in this example?
## To what departments does each gender predominantly apply?

gender.dept <- margin.table(UCBAdmissions, c(2,3))
gender.dept
round(prop.table(gender.dept, 1), 3)

## So, males mostly apply to departments "A" and "B", while females
## mostly apply to departments "C", "D", "E", and "F".  But from the
## earlier proportions we saw that departments "A" and "B" admit about
## 2/3 (or more) of applicants of either gender, while departments
## "C", "D", "E", and "F" admit only about 1/3 or fewer of applicants
## of either gender.  So female candidates mostly apply to departments
## that are more difficult to get into, while males mostly apply to
## departments that admit a fairly large proportion of their
## applicants.

### To analyze the data using glm(), we need them in the form of a
### data frame.

## Note that the data currently  an array and a 
## Turn the contingency table (array of counts) into a data frame.

berk <- as.data.frame(UCBAdmissions)
berk

## Hmmm, we could use the data in this form in SAS, so I'll save it as
## a text file before continuing.

write.table(berk, "berkeley.txt", quote = FALSE, row.names = FALSE)

## xtabs reverses the operation, and takes a formula argument.

xtabs(Freq ~ ., data=berk)
ftable(xtabs(Freq ~ ., data=berk))
xtabs(Freq ~ Gender + Admit, data=berk)
xtabs(Freq ~ Dept + Admit, data=berk)

## Unfortunately, the data frame is not in the form we need for
## using the glm function.  We could build our own as follows:

Dept <- rep(c("A", "B", "C", "D", "E", "F"), each=2)
Dept
Gender <- rep(c("Male", "Female"), 6)
Gender
Counts <- matrix(berk$Freq, ncol=2, byrow=TRUE)
Counts
dimnames(Counts) <- list(NULL, c("Admit", "Reject"))
Counts
berk <- data.frame(Dept, Gender, Counts)
berk

## Note that, by default, data.frame() converts character variables to
## factors.

is.factor(berk$Dept)
is.factor(berk$Gender)
is.factor(berk$Admit)
is.factor(berk$Reject)

## Here is a simple logit model.

fit1 <- glm(cbind(Admit, Reject) ~ Dept + Gender, binomial, berk)
summary(fit1)
anova(fit1, test="Chisq")

## Note that this model does not fit well (deviance is 20.2 on 5 df).
## Not suprisingly, this seems mainly due to the Admit/Reject counts
## for female applicants to department "A", which have a deviance
## residual of 3.72.

residuals(fit1)
residuals(fit1, "pearson")

## Here is a way to compute adjusted residuals.

source("Src/adj.residuals.R")
adj.residuals(fit1)
adj.residuals(fit1, "pearson")

## Department "A" seems a clear outlier.
## Here is the same model fit to departments "B" through "F" only.
## Note that
##
## fit1 <- glm(cbind(Admit, Reject) ~ Dept + Gender, binomial, berk,
##             subset = (Dept != "A"))
##
## has gives the same fit, but it is more convenient to use update().

fit2 <- update(fit1, subset = (Dept != "A"))
summary(fit2)

## This model does fits well (deviance is 2.56 on 4 df).

## Now, is there any evidence of an effect of Gender after controlling
## for Dept (for departments "B" through "F")?  The Wald test
## statistic can be found in the summary() output, and has value
## 0.031/0.0868 = 0.354, leading to a p-value of 0.724.  The deviance,
## or likelihood ratio test statistic can be obtained using anova(),
## and has value 0.13 on 1 degree of freedom, giving a chisquare
## p-value of 0.72.

anova(fit2, test="Chisq")

## One could also directly fit the model without Gender.  Comparing
## models in this way is useful when testing more complicated
## hypotheses.

fit3 <- update(fit2, cbind(Admit, Reject) ~ Dept)
summary(fit3)
anova(fit3, fit2, test="Chisq")

## Note also that the deviance for the reduced model is 2.68 on 5 df,
## indicating a good fit.

## Finally, just for fun, note that the model Dept * Gender is
## saturated (as many parameters as observations).  If we fit it to
## the complete data set, we can recover the same raw odds ratios that
## we computed earlier.

fit4 <- glm(cbind(Admit, Reject) ~ Dept * Gender, binomial, berk)
summary(fit4)
lambda <- coef(fit4)
lambda
round(exp(lambda[7] + c(0, lambda[8:12])), 2)
round(apply(UCBAdmissions, 3, odds.ratio), 2)
