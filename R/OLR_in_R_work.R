# DDA with R
# Ch 8
# Section 8.1

library(vcdExtra)
library(MASS)
library(car)
library(effects)

data("Arthritis")
summary(Arthritis$Improved)
str(Arthritis$Improved)

# set Hess = TRUE to get standard errors
arth.polr <- polr(Improved ~ Sex + Treatment + Age, data = Arthritis, 
                  Hess = TRUE)
summary(arth.polr)

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
                                        Treatment = "Treated"),
        type = "probs")
p

cum_prob <- apply(p,1,cumsum) |> t() # transpose back to original shape

odds <- cum_prob/(1-cum_prob)
odds
# proportional odds
odds[1,]/odds[2,]

predict(arth.polr, newdata = data.frame(Sex = c("Male", "Female"),
                                        Age = 50,
                                        Treatment = "Treated"),
        type = "class")


# effect displays

Effect("Age", arth.polr) |> plot()
Effect("Age", arth.polr) |> plot(style = "stcaked")

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
