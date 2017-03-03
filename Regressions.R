##  [Regressions]  ##


#==========================================================================================
# Unconditional Convergence Model
#==========================================================================================
# Avg.GSDP.Growth = a + Beta(log.initial.GSDP) + u
#
# Pooling model used
# -----------------------------------------------------------------------------------------


unconditional <- plm(Average.GSDP.Growth ~ Log.Initial.GSDP, data = df.alpha, index = c("State", "Year"),
                     model = "pooling")

summary(unconditional)

#==========================================================================================
# Conditional Convergence Model
#==========================================================================================
# GSDP.Growth = a + Beta(lagged.log.GSDP.per.capita) + Gamma(Variables) + u
# -----------------------------------------------------------------------------------------
#  Log initial gsdp per capita?  or laggedgsdp per capita? lagged gsdp per capita?
#  explain why lagged
#------------------------------------------------------------------------------------------
# Fixed Effects
#------------------------------------------------------------------------------------------
# N Entity-Specific Intercepts

fixed <- plm(GSDP.Growth ~  + Population + Grain.Yields + Water.Access + Credit.by.SCBs +
                     Gross.Fixed.Capital.Formation + Per.Capita.Elec.Cons + Share.Rural.Pop, 
             data = df.alpha, index = c("State", "Year"), model = "within")

summary(fixed)

fixef(fixed) # display the fixed effects

## Random Effects

random <- plm(Average.GSDP.Growth ~ GSDP.per.capita + Population + Grain.Yields + Water.Access + Credit.by.SCBs +
                      Gross.Fixed.Capital.Formation + Per.Capita.Elec.Cons + Share.Rural.Pop,  
              data = df.alpha, index = c("State", "Year"), model = "random")

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

fixed.time <- plm(GSDP.Growth ~ GSDP.per.capita + factor(Year) + Population + Grain.Yields + Water.Access + Credit.by.SCBs +
                          Gross.Fixed.Capital.Formation + Per.Capita.Elec.Cons + Share.Rural.Pop,  
             data = df.alpha, index = c("State", "Year"), model = "within")

summary(fixed.time)

# Null is that no time-fixed effect needed
# If p < 0.05 then use time-fixed effects

pFtest(fixed.time, fixed) # p-value < 2.2e-16

plmtest(fixed, c("time"), type = ("bp")) # p-value < 2.2e-16

#==========================================================================================
# Testing for random effects: Breusch-Pagan Lagrange multiplier (LM test)
#==========================================================================================

# Regular OLS (pooling model) using plm

pool <- plm(GSDP.Growth ~ GSDP.per.capita + Population + Grain.Yields + Water.Access + Credit.by.SCBs +
                    Gross.Fixed.Capital.Formation + Per.Capita.Elec.Cons + Share.Rural.Pop,  
            data = df.alpha, index = c("State", "Year"), model = "pooling")

summary(pool)

# LM test helps decide between a random effects regression and a simple OLS
# regression- the null hypothesis is that variances across entities is zero,
# or that there is no significant difference across units (ie. no panel effect)

plmtest(pool, type = c("bp")) # p-value = 0.07779

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

pbgtest(fixed) # p-value = 0.0001209

# Serial correlation in idiosyncratic errors

#==========================================================================================
# Testing for heteroskedasticity
#==========================================================================================
# The null hypothesis for the Breusch-Pagan test is homoskedasticity.
#------------------------------------------------------------------------------------------

bptest(GSDP.Growth ~ GSDP.per.capita +factor(State) + Population + Grain.Yields + Water.Access + Credit.by.SCBs +
               Gross.Fixed.Capital.Formation + Per.Capita.Elec.Cons + Share.Rural.Pop,  
       data = df.alpha, studentize = F) # No heteroskedasticity