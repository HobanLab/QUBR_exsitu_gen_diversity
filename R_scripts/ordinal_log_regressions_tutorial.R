rm(list=ls())
#install.packages("reshape2")
library(foreign)
library(MASS)
library(ggplot2)
library(Hmisc)
library(reshape2)

####ORDINAL LOGISTIC REGRESSION####
dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
head(dat)

## one at a time, table apply, pared, and public
lapply(dat[, c("apply", "pared", "public")], table)
#0=private, 1=public


## three way cross tabs (xtabs) and flatten the table
ftable(xtabs(~ public + apply + pared, data = dat))
summary(dat$gpa)

sd(dat$gpa)

ggplot(dat, aes(x = apply, y = gpa)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(pared ~ public, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
#DANA: Is there a good way to label the axes for pared and public? As it currently is, you just see both as 0 and 1 with no differentiation of which is which, and ylab/xlab only changes the GPA/apply labels 

#could relabel things using a case_when
#they have to be treated as a factor, not a character

#The overall relationship between apply and gpa is slightly positive


## fit ordered logit model and store results 'm'
m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE)

#POLR = Proportional Odds Logistic Regression


## view a summary of the model
summary(m)

## store table
(ctable <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(m)) # default method gives profiled CIs

confint.default(m) # CIs assuming normality

## odds ratios
exp(coef(m))

## OR and CI
exp(cbind(OR = coef(m), ci))

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(dat, summary(as.numeric(apply) ~ pared + public + gpa, fun=sf)))

glm(I(as.numeric(apply) >= 2) ~ pared, family="binomial", data = dat)
glm(I(as.numeric(apply) >= 3) ~ pared, family="binomial", data = dat)

s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s # print

plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))

newdat <- data.frame(
  pared = rep(0:1, 200),
  public = rep(0:1, each = 200),
  gpa = rep(seq(from = 1.9, to = 4, length.out = 100), 4))

newdat <- cbind(newdat, predict(m, newdat, type = "probs"))

##show first few rows
head(newdat)

lnewdat <- melt(newdat, id.vars = c("pared", "public", "gpa"),
                variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat)

dev.off() #This was not in the tutorial code, but it resolved the Error (invalid graphics state) occuring in the next plot by removing any prior plot settings
ggplot(lnewdat, aes(x = gpa, y = Probability, colour = Level)) +
  geom_line() + facet_grid(pared ~ public, labeller="label_both")


