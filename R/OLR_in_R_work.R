# DDA with R
# Ch 8
# Section 8.1

library(vcdExtra)
library(MASS)
library(car)
library(effects)

data("Arthritis")
str(Arthritis$Improved)
summary(Arthritis$Improved)

# use factor(ordered = TRUE) to create ordered factor

# cumulative probability
tab <- table(Arthritis$Improved)
N <- sum(tab)
# cell proportions
proportions(tab)
# Probability Some or lower is about 0.67
cumsum(tab)/N

# Probability Some or more is about 0.50
rev(cumsum(rev(tab))/N)


# intercept-only model
arth.0 <- polr(Improved ~ 1, data = Arthritis)
summary(arth.0)

# convert logodds to probability
plogis(arth.0$zeta)
# compare to raw cumulative probabilities
cumsum(tab)/N

# incorporate a predictor: Treatment
tab2 <- xtabs(~ Treatment + Improved, data = Arthritis)
tab2
tab2 |> proportions(margin = 1)

apply(tab2, 1, function(x)cumsum(x)/sum(x)) |> t()

arth.1 <- polr(Improved ~ Treatment, data = Arthritis)
summary(arth.1)
arth.1$coefficients
arth.1$zeta

# Formula for P(Improve <= None) when Placebo
p1 <- plogis(0.7499107)
p1
# Formula for P(Improve <= None) when Treated
p2 <- plogis(0.7499107 - 1.567644)
p2
# Formula for P(Improve <= Some) when Placebo
p3 <- plogis(1.5498756)
p3
# Formula for P(Improve <= Some) when Treated
p4 <- plogis(1.5498756 - 1.567644)
p4

matrix(c(p1,p2,p3,p4,1,1), nrow = 2, ncol = 3)

# compare to observed cumulative proportions
apply(tab2, 1, function(x)cumsum(x)/sum(x)) |> t()

# Let's look at coefficient
coef(arth.1)
# odds ratio
exp(coef(arth.1))

# Interpretation: the estimated odds that a Treated subject's response is higher
# improvement is about 4.8 times the odds that a Placebo subject's response is
# higher improvement

# Treated cumulative probabilities
tcp <- exp(arth.1$zeta - arth.1$coefficients)/(1 + exp(arth.1$zeta - arth.1$coefficients))
# Placebo cumulative probabilities
pcp <- exp(arth.1$zeta)/(1 + exp(arth.1$zeta))

# Treated odds
tcp/(1-tcp)

# Placebo odds
pcp/(1-pcp)

# odds ratios all the same
(tcp/(1-tcp))/(pcp/(1-pcp))

# log of odds ratios: same as model coefficient but with opposite sign
log((tcp/(1-tcp))/(pcp/(1-pcp)))

# same thing another way.
# get predicted probabilities for each category; NOT cumulative
preds <- predict(arth.1, 
                 newdata=data.frame(Treatment = c("Placebo", "Treated")),
                 type = "probs")
preds

# compare to observed
tab2 |> proportions(margin = 1)

# placebo cumulative probs
cum_placebo <- cumsum(preds[1,])
cum_placebo
# treated cumulative probs
cum_treat <- cumsum(preds[2,])
cum_treat

# placebo odds
cum_placebo/(1 - cum_placebo)
# treated odds
cum_treat/(1 - cum_treat)

# odds ratio
(cum_treat/(1 - cum_treat))/(cum_placebo/(1 - cum_placebo))

# log odds ratio
log((cum_treat/(1 - cum_treat))/(cum_placebo/(1 - cum_placebo)))

# Notice they're the same, except for the sign.  R tacks on a minus sign to the
# coefficients in the summary output so when you exponentiate and take the odds
# ratio, higher levels of predictors correspond to the response falling in the
# higher end of the ordinal scale.

saveRDS(Arthritis, file = "data/arthritis.rds")



# effect display
Effect(arth.1, focal.predictors = "Treatment") |> plot()
Effect(arth.1, focal.predictors = "Treatment") |> plot(style = "stacked")

# recall observed proportions
tab2 <- xtabs(~ Treatment + Improved, data = Arthritis)
tab2 |> proportions(margin = 1)

Effect(arth.1, focal.predictors = "Treatment")

# compare to rms
library(rms)
arth.1rms <- lrm(Improved ~ Treatment, data = Arthritis)
arth.1rms


# Formula for P(Improve >= Some) when Placebo
plogis(-0.7501)
# Formula for P(Improve >= Some) when Treated
plogis(-0.7501 + 1.5679) 

# Formula for P(Improve >= Marked) when Placebo
plogis(-1.5501)
# Formula for P(Improve >= Marked) when Treated
plogis(-1.5501 + 1.5679)

# incorporate a predictor: Treatment
tab2 <- xtabs(~ Treatment + Improved, data = Arthritis)
tab2

apply(tab2, 1, function(x)rev(cumsum(rev(x))/sum(x))) |> t()



# 8.1.2

# set Hess = TRUE to get standard errors
arth.polr <- polr(Improved ~ Sex + Treatment + Age, data = Arthritis, 
                  Hess = TRUE)
summary(arth.polr)
car::S(arth.polr)

# Some|Marked is cutoff between Some and Marked.

Anova(arth.polr)

# cumulative logits
# L_1: Prob(None) versus Prob(Some or Marked)
# L_2: Prob(None or Some) versus Prob(Marked)

# odds ratios (always positive)
# need to reverse signs to state the model
coef(arth.polr)
-coef(arth.polr)
exp(coef(arth.polr))
exp(-coef(arth.polr))

# Odds of "None" versus "Some or Marked" is about 3.5 times higher for males than the odds of "None" versus "Some or Marked" for females. This should be reflected in predicted probabilities

p <- predict(arth.polr, newdata = data.frame(Sex = c("Male", "Female"),
                                        Age = 50,
                                        Treatment = "Placebo"),
        type = "probs")
p

# Prob(y>=Some) for Female age 50 on placebo
sum(p[2,2:3])

cum_prob <- apply(p,1,cumsum) |> t() # transpose back to original shape

odds <- cum_prob/(1-cum_prob)
odds
# proportional odds
odds[1,]/odds[2,]

predict(arth.polr, newdata = data.frame(Sex = c("Male", "Female"),
                                        Age = 50,
                                        Treatment = "Placebo"),
        type = "class")

# predicted probability using formula 3.11 in ACD with R (page 174)
# see also ?polr
# j = 2 (some)
# Prob(y<=Some) for 50 year old female on placebo
# use intercept Some|Marked
summary(arth.polr)
plogis(3.430942 - 50*0.03816199)

p
sum(p[2,1:2])

# effect displays

Effect("Age", arth.polr) |> plot()
Effect("Age", arth.polr) |> plot(style = "stacked")

Effect(c("Treatment", "Sex", "Age"), arth.polr) |> 
  plot() # very busy

Effect(c("Treatment", "Sex", "Age"), arth.polr) |> 
  plot(style = "stcaked")

# test PO assumption
car::poTest(arth.polr)

# Two prediction types: class and probs, with class as default
predict(arth.polr, type = "class")[1:5]
predict(arth.polr, type = "probs")[1:5,]
# all probs sum to 1
rowSums(predict(arth.polr, type = "probs")[1:5,])

# 8.1.3 testing the PO assumption

library(VGAM)
# PO model (signs of coefs reversed)
arth.po <- vglm(Improved ~ Sex + Treatment + Age, data = Arthritis,
                family = cumulative(parallel = TRUE))
arth.po
summary(arth.po)
coef(arth.po, matrix = TRUE)
arth.polr

# NPO model
arth.npo <- vglm(Improved ~ Sex + Treatment + Age, data = Arthritis,
                 family = cumulative(parallel = FALSE))
arth.npo
coef(arth.npo, matrix = TRUE)

VGAM::lrtest(arth.po, arth.npo)

# PPO model
arth.ppo <- vglm(Improved ~ Sex + Treatment + Age, data = Arthritis,
                 family = cumulative(parallel = FALSE ~ Sex))
coef(arth.ppo, matrix = TRUE)

# 8.1.4 graphical assessment of proportional odds
library(rms)
arth.po2 <- orm(Improved ~ Sex + Treatment + Age, data = Arthritis)
arth.po2

op <- par(mfrow=c(1,3))
plot.xmean.ordinaly(Improved ~ Sex + Treatment + Age, data = Arthritis,
                    lwd = 2, pch =16, subn = FALSE)
par(op)

# predicted probability using 13.4 in RMS (page 313)
# j = 2 (some)
# Prob(y>=Some) for 50 year old female on placebo
1/(1 + exp(-(-2.5319100 + 50*0.0381614)))
# cumulative probabilities
predict(arth.po2, 
        newdata = data.frame(Sex = "Female", Treatment = "Placebo", Age = 50),
        type="fitted")
# individual probabilities
predict(arth.po2, 
        newdata = data.frame(Sex = "Female", Treatment = "Placebo", Age = 50),
        type="fitted.ind")

library(ordinal)
arth.clm <- clm(Improved ~ Sex + Treatment + Age, data = Arthritis)
summary(arth.clm)


