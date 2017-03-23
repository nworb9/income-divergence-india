#==========================================================================================
# Regressions 
#==========================================================================================


#==========================================================================================
# Unconditional Convergence Model
#==========================================================================================
# GSDP.Growth = a + Beta(lagged.log.GSDP) + u
#
# Pooling model used
# -----------------------------------------------------------------------------------------

unconditional2 <- plm(GSDP.Growth ~ Lagged.Log.GSDP.per.capita, data = df.alpha,
                      index = c("State", "Year"), model = "pooling")

summary(unconditional2)

coeftest(unconditional2, vcov = vcovHC(unconditional2, 
                                       type = "HC0", cluster = "group")) # Clustered SEs

resettest(GSDP.Growth ~ Lagged.Log.GSDP.per.capita, type = "regressor", 
          data = df.alpha) # Functional form test

#==========================================================================================
# Conditional Convergence Model
#==========================================================================================
# GSDP.Growth = a + Beta(lagged.log.GSDP.per.capita) + Gamma(Variables) + u
#------------------------------------------------------------------------------------------
# Fixed Effects
#------------------------------------------------------------------------------------------
# N Entity-Specific Intercepts

fixed <- plm(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + Population.Growth + Credit.by.SCBs +
                      Per.Capita.Elec.Cons + Share.Rural.Pop, 
             data = df.alpha, index = c("State", "Year"), model = "within")

summary(fixed)

coeftest(fixed, vcov = vcovHC(fixed, type = "HC0", 
                              cluster = "group")) # Clustered SEs

resettest(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + Population.Growth + Grain.Yields + Water.Access + Credit.by.SCBs +
                  Gross.Fixed.Capital.Formation + Per.Capita.Elec.Cons + Share.Rural.Pop,
          type = "regressor", data = df.alpha) # Functional form test

fixef(fixed) # display the fixed effects

#------------------------------------------------------------------------------------------
# Random Effects
#------------------------------------------------------------------------------------------

random <- plm(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + Population.Growth + Credit.by.SCBs +
                      Per.Capita.Elec.Cons + Share.Rural.Pop,  
              data = df.alpha, index = c("State", "Year"), model = "random")

coeftest(random, vcov = vcovHC(random, type = "HC0", 
                               cluster = "group")) # Clustered SEs

summary(random)

#==========================================================================================
# Hausman Test [Fixed vs. Random]
#==========================================================================================
# Tests whether the unique errors (u) are correlated with the regressors
# Null hypothesis is that they are not
#------------------------------------------------------------------------------------------

phtest(fixed, random) # Regressors too dependent :/

#==========================================================================================
# Testing for time-fixed effects
#==========================================================================================

fixed.time <- plm(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + factor(Year) + Population.Growth + 
                          Grain.Yields + Water.Access + Credit.by.SCBs +
                          Gross.Fixed.Capital.Formation + Per.Capita.Elec.Cons + Share.Rural.Pop,  
                  data = df.alpha, index = c("State", "Year"), model = "within")

coeftest(fixed.time, vcov = vcovHC(fixed.time, type = "HC0", 
                                   cluster = "group")) # Clustered SEs

summary(fixed.time)

# Null is that no time-fixed effect needed
# If p < 0.05 then use time-fixed effects

pFtest(fixed.time, fixed) # p-value < 2.2e-16

plmtest(fixed, c("time"), type = ("bp")) # p-value < 2.2e-16

#==========================================================================================
# Testing for random effects: Breusch-Pagan Lagrange multiplier (LM test)
#==========================================================================================

# Regular OLS (pooling model) using plm

pool <- plm(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + Population.Growth + Grain.Yields + Water.Access + Credit.by.SCBs +
                    Gross.Fixed.Capital.Formation + Per.Capita.Elec.Cons + Share.Rural.Pop,  
            data = df.alpha, index = c("State", "Year"), model = "pooling")

coeftest(pool, vcov = vcovHC(pool, type = "HC0", 
                             cluster = "group")) # Clustered SEs

summary(pool)

# LM test helps decide between a random effects regression and a simple OLS
# regression- the null hypothesis is that variances across entities is zero,
# or that there is no significant difference across units (ie. no panel effect)

plmtest(pool, type = c("bp")) # p-value = 0.2154

# Null not rejected, random effects inappropriate

#==========================================================================================
# Testing for cross-sectional dependence/contemporaneous correlation: using
# Breusch-Pagan LM test of independence and Pasaran CD test
#==========================================================================================
# According to Baltagi, cross-sectional dependence is a problem in macro panels with long time 
# series.  The null hypothesis in the B-P/LM and Pasaran CD tests of independence is that residuals 
# across entities are not correlated.   B-P/LM and Pasaran CD (cross-sectional dependence) tests are 
# used to test whether the residuals are correlated across entities.  Cross-sectional dependence can 
# lead to bias in tests results (also called contemporaneous correlation)
#------------------------------------------------------------------------------------------

pcdtest(fixed, test = c("lm")) # p-value < 2.2e-16

pcdtest(fixed, test = c("cd")) # p-value < 2.2e-16

# Definitely cross-sectional dependence

#==========================================================================================
# Testing for cross-sectional dependence/contemporaneous correlation: using
# Breusch-Pagan LM test of independence and Pasaran CD test
#==========================================================================================
# Serial correlation tests apply to macro panels with long time series. Not a problem in 
# micro panels (with very few years). The null is that there is not serial correlation.
#------------------------------------------------------------------------------------------

pbgtest(fixed) # p-value = 7.337e-06

# Serial correlation in idiosyncratic errors

#==========================================================================================
# Testing for heteroskedasticity
#==========================================================================================
# The null hypothesis for the Breusch-Pagan test is homoskedasticity.
# Breusch-Pagan test for model because of amount of regressors.
#------------------------------------------------------------------------------------------

bptest(GSDP.Growth ~ Lagged.Log.GSDP.per.capita +factor(State) + Population.Growth + Grain.Yields 
       + Water.Access + Credit.by.SCBs + Gross.Fixed.Capital.Formation + Per.Capita.Elec.Cons 
       + Share.Rural.Pop,  
       data = df.alpha, studentize = F) # No heteroskedasticity

#==========================================================================================
# Testing for normality
#==========================================================================================
# Shapiro-Wilk normality test
#------------------------------------------------------------------------------------------

lapply()

shapiro.test(df.alpha$GSDP.Growth)

shapiro.test(df.alpha$Lagged.Log.GSDP.per.capita)

shapiro.test(df.alpha$Population.Growth)

shapiro.test(df.alpha$Grain.Yields)

shapiro.test(df.alpha$Water.Access)

shapiro.test(df.alpha$Credit.by.SCBs)

shapiro.test(df.alpha$Gross.Fixed.Capital.Formation)

shapiro.test(df.alpha$Per.Capita.Elec.Cons)

shapiro.test(df.alpha$Share.Rural.Pop)
