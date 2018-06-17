
'http://bit.ly/ipasmall'
'http://bit.ly/ipabraya'

## load dalringtonia data

wasp <- read.csv('darlintonia.csv', skip = 1)

wasp <- read.csv('http://bit.ly/ipawasp', skip = 1)
head(wasp)

library('readr')
wasp <- read_csv('http://bit.ly/ipawasp', skip = 1)

wasp <- transform(wasp, visited = factor(visited))

mod <- glm(visited ~ leafHeight, data = wasp,
           family = binomial(link = 'logit'))

summary(mod)

mod0 <- glm(visited ~ 1, data = wasp,
            family = binomial(link = 'logit'))

anova(mod0, mod, test = "LRT")

library('ggplot2')

## create data to predict at
pdata <- with(wasp, data.frame(leafHeight = seq(min(leafHeight), 
                                                max(leafHeight), 
                                                length = 100)))

pdata <- with(wasp, expand.grid(leafHeight = seq(min(leafHeight), 
                                                 max(leafHeight), 
                                                 length = 100),
                                colour = mean(colour)))

## predict on the scale of the linear predictor
fit <- predict(mod, newdata = pdata, type = 'link', se.fit = TRUE)

## gather the predictions
pdata <- transform(pdata, fit = fit$fit, se = fit$se.fit)
## add a confidence interval
pdata <- transform(pdata, upper = fit + (2*se), lower = fit - (2*se))
# transform to probability scale
ilink <- family(mod)$linkinv
pdata <- transform(pdata, fit = ilink(fit),
                   upper = ilink(upper), lower = ilink(lower))
head(pdata) # check

##plot
ggplot(pdata, aes(x = leafHeight)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = fit)) +
  geom_point(data = wasp,
             mapping = aes(x = leafHeight,
                           y = as.numeric(as.character(visited))))


## Gamma GLM
peat <- read_csv('http://bit.ly/ipapeat')
peat <- read.csv('http://bit.ly/ipapeat')

## or
peat <- read.csv('maddy-peat.csv')

peat <- transform(peat,
                  midDepth = upperDepth - 
                    (0.5*(upperDepth - lowerDepth)),
                  midCal = calUpper - 
                    (0.5*(calUpper - calLower)))

ggplot(peat, aes(x = midDepth, y = midCal)) +
  geom_point() +
  labs(x = "Depth (cm)", y = "Calibrarted Age")

peat_mod <- glm(midCal ~ midDepth, data = peat,
                family = Gamma(link = "identity"))
summary(peat_mod)

## live coding

## load braya so data set
braya <- read.table("http://bit.ly/ipabraya", skip = 84)
names(braya) <- c("Depth", "DepthUpper", "DepthLower", "Year", "YearYoung", "YearOld", "UK37")
braya <- transform(braya, sampleInterval = YearYoung - YearOld)

## plot labels
d15n_label <- expression(delta^{15}*N)
braya_ylabel <- expression(italic(U)[37]^{italic(k)})

library("mgcv")
library("ggplot2")

## source Small Water data
small <- readRDS("small-water-isotope-data.rds")

small <- readRDS(url("http://bit.ly/ipasmall"))
head(small)

d15n_label <- expression(delta^{15}*N) # ?plotmath

small_plt <- ggplot(small, aes(x = Year, y= d15N)) +
  geom_point()+
  labs(y = d15n_label, x = "Year CE")
small_plt

## Simple wrong
sw1 <- gam(d15N ~ s(Year, k = 15), data = small,
           method = "REML")
summary(sw1)
plot(sw1, shade = TRUE, residuals = TRUE, pch = 19)
gam.check(sw1)

sw2 <- gamm(d15N ~ s(Year, k = 15), data = small,
            method = "REML",
            correlation = corCAR1(form = ~ Year))
summary(sw2$gam)
intervals(sw2$lme)
plot(sw2$gam, shade = TRUE, residuals = TRUE, pch = 19)

## Braya so

## load braya so data set
braya <- read.table("file_name.txt", skip = 84)

braya <- read.table("http://bit.ly/ipabraya", skip = 84)
names(braya) <- c("Depth", "DepthUpper", "DepthLower", 
                  "Year", "YearYoung", "YearOld", "UK37")
braya <- transform(braya,
                   sampleInterval = YearYoung - YearOld)

## plot labels
braya_ylabel <- expression(italic(U)[37]^{italic(k)})

braya_plt <- ggplot(braya, aes(x = Year, y = UK37)) +
  geom_line(colour = "grey") +
  geom_point() +
  labs(y = braya_ylabel, x = "Year CE")

theme_set(theme_bw())
braya_plt

bs1 <- gamm(UK37 ~ s(Year, k = 80), data = braya,
            correlation = corCAR1(form = ~ Year),
            method = "REML",
            control = list(niterEM = 0,
                           optimMethod = "BFGS",
                           opt = "optim"))
summary(bs1$gam)
intervals(bs1$lme)

plot(bs1$gam, shade = TRUE)

plot(gam(UK37 ~ s(Year, k= 30), data = braya), shade = TRUE)


bs3 <- gam(UK37 ~ s(Year, k = 40), data = braya,
           method = "REML",
           weights = sampleInterval / mean(sampleInterval))
summary(bs3)
plot(bs3, shade = TRUE, residuals = TRUE, pch = 19)

