library(tidyverse)

# this is a simplified version of the Jena experiment:
# there are 80 plots, each with 1,2,4,8 or 16 plant species
d = data.frame(plot = as.factor(1:80),
               plant.rich = rep(c(1,2,4,8,16), 16),
               response = NA)
View(d)

# now we add a dummy response variable
# values are randomly drawn from a normal distribution
# with a mean of 0.15 and a standard deviation of 0.01
# these are (I think) realistic values for fungal/bacterial 
# composition, expressed as a proportion of fungi 
# (similar to what we would get based on plfas)
# the fact that they are random, and therefore their variation  
# is unrelated to plant richness, makes this compatible with 
# what we would see if the null hypothesis is true: 
# no effect of plant richness on f/b composition
d$response = rnorm(80, .15, .01)


# fitting a linear model to these data to assess the effect of 
# plant richness on the proportion of fungi
# we estimate alpha and beta in the formula y = alpha + beta*x
# alpha is our intercept, the value of y when x=0
# beta is the slope, how y changes as x changes
# so, in our case y, the response, is the proportion of fungi,
# x is plant richness, 
# beta is the rate of change of fungal prop. as plant richness changes
# and weirdly, alpha is the fungal prop when plant richness is 0
lm(response ~ plant.rich, data = d)

ggplot(d, aes(y=response,
              x=plant.rich)) +
  geom_point() +
  geom_smooth(method = "lm")

# we can change things slightly by log-transforming the predictor variable
# now we assess how fungal prop changes as a function of the doubling of plant 
# richness (with base 2 logarithms the change from 1 to 2, 2 to 4, 4 to 8 etc is 
# 1 unit of change)
# this is consistent with how the Jena experiment was designed:
log2(c(1,2,4,8,16))

lm(response ~ log2(plant.rich), data = d)

# now the estimated intercept is the proportion of fungi when log2(x) = 0
# which is when plant richness is 1 (log2(1) = 0)
# the estimated slope is also different because it tells us how fungal prop 
# changes when richness doubles (in this case it doesn't; it is practically zero)

ggplot(d, aes(y=response,
              x=log2(plant.rich))) +
  geom_point() +
  geom_smooth(method = "lm")

# the intercept (alpha) is always the value of y when the value of the predictor
# is 0 but we can shift that 0 wherever we want it; this is called centering
# sometimes we want to center our predictor to a meaningful reference point
# (in our case monocultures might make sense, so by doing plant.rich - 1 would 
# be a reasonable option. log2 also turns 1 to 0 but it only works for 1...)

# a more technical choice for centering is the mean of the predictor
d$plant.rich - mean(d$plant.rich)
# then our intercept would give us an estimate of fungal prop at average plant 
# richness. If we also divide by the standard deviation (sd) of the predictor 
(d$plant.rich - mean(d$plant.rich)) / sd(d$plant.rich)
# then we have a predictor that is centered at zero and scaled to one sd
# this in turn changes the meaning of the slope estimate because now it tells us 
# how much the response changes when the predictor changes by 1 standard deviation
# this may seem unnecessarily confusing, but it will become important when 
# we have interactions of different predictors, such as the interaction between
# plant richness and history treatment

# the function scale() does both scaling and centering by default (see ?scale)

# so here I will do both of these things: log2 because we are interested in how 
# the doubling of plant richness is affecting the fungal proportion
# and scaling+centering because we will need it later

lm(response ~ scale(log2(plant.rich)), data = d)
ggplot(d, aes(y=response,
              x=scale(log2(plant.rich)))) +
  geom_point() +
  geom_smooth(method = "lm",
              level = .95)


# now I will create another dummy dataset consistent with the dBEF design
d = data.frame(# the 80 plots
               plot = as.factor(rep(1:80, each = 3)),
               # the three treatment subplots
               treatment = as.factor(rep(c("++","+-","--"), 80)),
               # the plant richness gradient, simplified
               plant.rich = rep(rep(c(1,2,4,8,16), 16), each = 3),
               response = NA)
# I will also change the order of the treatment levels, so that 
# ++ (the control) comes first. It will be our reference against which we compare 
# the two treatments
d$treatment = factor(d$treatment, levels = c("++","+-","--"))

d$response = rnorm(240, .15, .01)
# again this random response is consistent with what we would get if two
# null hypotheses are true: no effect of richness on fungal prop and 
# no effect of history treatment

lm(response ~ 1 
            + scale(log2(plant.rich)) 
            + treatment 
            + scale(log2(plant.rich)):treatment, 
   data = d)
# now the output of the model becomes a bit more complicated:
# the intercept, once again the value of the response when the continuous 
# predictor is zero, BUT now this is the intercept of our reference level (++)
# so in control communities. Then plant.rich again the slope across control communities
# 'treatment+-' is the difference of the intercept in (+-) communities from that of (++)
# 'treatment--' is the difference of the intercept in (--) communities from that of (++)
# 'plant.rich:treatment+-' is the difference of the slope in (+-) communities from that of (++)
# 'plant.rich:treatment--' is the difference of the slope in (--) communities from that of (++)
# (they are all very close to zero)

ggplot(d, aes(y=response,
              x=scale(log2(plant.rich)))) +
  geom_point() +
  geom_smooth(method = "lm",
              level = .95) +
  facet_wrap(~treatment)
# or 
ggplot(d, aes(y=response,
              x=scale(log2(plant.rich)),
              color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm",
              level = .95)

# now we can save the model into an object m
m <- lm(response ~ 1 
                 + scale(log2(plant.rich)) 
                 + treatment 
                 + scale(log2(plant.rich)):treatment, 
           data = d)
# and get a summary. This has the same coefficients as before
# but now we get more information:
# Estimate is the mean estimate for each coefficient
# Std. Error is the standard deviation of the estimate
# t value is the t statistic
# Pr(>|t|) is the significance level, the "p value". Formally, 
# it tells us what is the probability of getting a t value at
# least as large in magnitude as the one we got, assuming 
# the null hypothesis is true. This is very confusing but it 
# is fairly easier to think about it as the level of compatibility
# with the null hypothesis. The smaller the p-value the less compatible
# our estimate is with "no difference from zero"
summary(m)
# we can also look at the so called confidence intervals for each coefficient
# the exclusion of zero from a confidence interval (here a 95% confidence interval)
# indicates that the coefficient is clearly different from zero
# so our intercept is estimated to be ~ 0.15 with a confidence interval [0.149, 0.153]
# the other coefficients have intervals [-something, +something] (so, zero is in)
# this indicates that, for example, plant richness in this dummy data set 
# has no clear effect on the response
confint(m, level = .95)

# let's add blocks now
d = data.frame(block = rep(c("b1","b2","b3","b4"), each = 3*20),
               # the 80 plots
               plot = as.factor(rep(1:80, each = 3)),
               # the three treatment subplots
               treatment = as.factor(rep(c("++","+-","--"), 80)),
               # the plant richness gradient, simplified
               plant.rich = rep(rep(c(1,2,4,8,16), 16), each = 3),
               response = NA)
d$treatment = factor(d$treatment, levels = c("++","+-","--"))
d$response = rnorm(240, .15, .01)

library(glmmTMB) # for model fitting
library(performance) # for model evaluation
library(marginaleffects) # for plots
library(emmeans) # for contrasts

# the addition of block as a random effect accounts for the fact that
# on average, the value of the response may differ somewhat among blocks
m <- glmmTMB(response ~ 1 
                      + scale(log2(plant.rich)) 
                      + treatment 
                      + scale(log2(plant.rich)):treatment
                      + (1|block), 
             data = d)

check_model(m)
summary(m)
confint(m, level = .95)
plot_predictions(m, condition = list("plant.rich", "treatment"))

# we can also do the loging and scaling outside of the model
d$plant.rich.sc = (log2(d$plant.rich) - mean(log2(d$plant.rich))) / sd(log2(d$plant.rich))

m <- glmmTMB(response ~ 1 
             + plant.rich.sc 
             + treatment 
             + plant.rich.sc:treatment
             + (1|block), 
             data = d)

check_model(m)
summary(m)
confint(m, level = .95)
plot_predictions(m, condition = list("plant.rich.sc", "treatment"))

# since our response is a proportion we can model it with a beta distribution
# the main reason to do this is that a simple linear model might make predictions
# that are outside of the (0,1) range. This is not realistic, we can't have proportions
# with values less than zero, nor can fungi be > than 100% of the microbial community

d$response[d$treatment=="++"] = rbeta(80, .95*d$plant.rich[d$treatment=="++"], 1)
d$response[d$treatment=="+-"] = rbeta(80, .55*d$plant.rich[d$treatment=="++"], 1)
d$response[d$treatment=="--"] = rbeta(80, .25*d$plant.rich[d$treatment=="++"], 1)

m <- glmmTMB(response ~ 1 
             + plant.rich.sc 
             + treatment 
             + plant.rich.sc:treatment
             + (1|block), 
             family = beta_family(),
             data = d)
check_model(m)
summary(m)
confint(m, level = .95)
plot_predictions(m, condition = list("plant.rich.sc", "treatment"))

