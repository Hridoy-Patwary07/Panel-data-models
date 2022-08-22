
install.packages("plm")
library(plm)

mydata<- read.csv("panel_wage.csv", header = T)
names(mydata)

# Set data as panel data
pdata <- pdata.frame(mydata, index=c("id","t"))

# Descriptive statistics
summary(pdata)

# Pooled OLS estimator
pooling <- plm(lwage ~ exp + exp2 + wks + ed, data=pdata, model= "pooling")
summary(pooling)

# Between estimator
between <- plm(lwage ~ exp + exp2 + wks + ed, data=pdata, model= "between")
summary(between)

# First differences estimator
firstdiff <- plm(lwage ~ exp + exp2 + wks + ed, data=pdata, model= "fd")
summary(firstdiff)

# Fixed effects or within estimator
fixed <- plm(lwage ~ exp + exp2 + wks + ed, data=pdata, model= "within")
summary(fixed)

# Random effects estimator
random <- plm(lwage ~ exp + exp2 + wks + ed, data=pdata, model= "random")
summary(random)

# LM test for random effects versus OLS
plmtest(pooling)

# LM test for fixed effects versus OLS
pFtest(fixed, pooling)

# Hausman test for fixed versus random effects model
phtest(random, fixed)




library(foreign)
Panel <-read.dta("http://dss.princeton.edu/training/Panel101.dta")
coplot(y ~ year|country, type="l", data=Panel) # Lines
coplot(y ~ year|country, type="b", data=Panel) # Points and lines


library(car)
scatterplot(y~year|country, boxplots=FALSE, smooth=TRUE, data=Panel)

library(gplots)
plotmeans(y ~ country, main="Heterogeineityacross countries", data=Panel)

plotmeans(y ~ year, main="Heterogeineityacross years", data=Panel)

fixed <-plm(y ~ x1, data=Panel, index=c("country", "year"), model="within")
random <-plm(y ~ x1, data=Panel, index=c("country", "year"), model="random")
phtest(fixed, random)