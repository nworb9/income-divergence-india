#==========================================================================================
# Regressions 
#==========================================================================================
# Test for Unit Root
#------------------------------------------------------------------------------------------

Andhra.Growth <- df.Andhra$GSDP.Growth[2:21]
adf.test(Andhra.Growth, k = 1) #Stationary

Assam.Growth <- df.Assam$GSDP.Growth[2:21]
adf.test(Assam.Growth, k = 1) #Stationary

Bihar.Growth <- df.Bihar$GSDP.Growth[2:21]
adf.test(Bihar.Growth, k = 1) #Unit root

Gujarat.Growth <- df.Gujarat$GSDP.Growth[2:21]
adf.test(Gujarat.Growth, k = 1) #Unit root

Haryana.Growth <- df.Haryana$GSDP.Growth[2:21]
adf.test(Haryana.Growth, k = 1) #Stationary

Karnataka.Growth <- df.Karnataka$GSDP.Growth[2:21]
adf.test(Karnataka.Growth, k = 1) #Unit root

Kerala.Growth <- df.Kerala$GSDP.Growth[2:21]
adf.test(Kerala.Growth, k = 1) #Unit root

Madhya.Growth <- df.Madhya$GSDP.Growth[2:21]
adf.test(Madhya.Growth, k = 1) #Unit root

Maha.Growth <- df.Maha$GSDP.Growth[2:21]
adf.test(Maha.Growth, k = 1) #Unit root

Odisha.Growth <- df.Odisha$GSDP.Growth[2:21]
adf.test(Odisha.Growth, k = 1) #Stationary

Punjab.Growth <- df.Punjab$GSDP.Growth[2:21]
adf.test(Punjab.Growth, k = 1) #Unit root

Raja.Growth <- df.Raja$GSDP.Growth[2:21]
adf.test(Raja.Growth, k = 1) #Unit root

Tamil.Growth <- df.Tamil$GSDP.Growth[2:21]
adf.test(Tamil.Growth, k = 1) #Unit root

UP.Growth <- df.UP$GSDP.Growth[2:21]
adf.test(UP.Growth, k = 1) #Unit root

WBengal.Growth <- df.WBengal$GSDP.Growth[2:21]
adf.test(WBengal.Growth, k = 1) #Unit root

#==========================================================================================
# Sigma Convergence
#==========================================================================================
# Levels of GSDP.per.capita
#------------------------------------------------------------------------------------------

df.alpha1 <- subset(df.alpha, Year == "1991")
df.alpha2 <- subset(df.alpha, Year == "1992")
df.alpha3 <- subset(df.alpha, Year == "1993")
df.alpha4 <- subset(df.alpha, Year == "1994")
df.alpha5 <- subset(df.alpha, Year == "1995")
df.alpha6 <- subset(df.alpha, Year == "1996")
df.alpha7 <- subset(df.alpha, Year == "1997")
df.alpha8 <- subset(df.alpha, Year == "1998")
df.alpha9 <- subset(df.alpha, Year == "1999")
df.alpha10 <- subset(df.alpha, Year == "2000")
df.alpha11 <- subset(df.alpha, Year == "2001")
df.alpha12 <- subset(df.alpha, Year == "2002")
df.alpha13 <- subset(df.alpha, Year == "2003")
df.alpha14 <- subset(df.alpha, Year == "2004")
df.alpha15 <- subset(df.alpha, Year == "2005")
df.alpha16 <- subset(df.alpha, Year == "2006")
df.alpha17 <- subset(df.alpha, Year == "2007")
df.alpha18 <- subset(df.alpha, Year == "2008")
df.alpha19 <- subset(df.alpha, Year == "2009")
df.alpha20 <- subset(df.alpha, Year == "2010")
df.alpha21 <- subset(df.alpha, Year == "2011")

cv1 <- cv(df.alpha1$GSDP.per.capita)
cv2 <- cv(df.alpha2$GSDP.per.capita)
cv3 <- cv(df.alpha3$GSDP.per.capita)
cv4 <- cv(df.alpha4$GSDP.per.capita)
cv5 <- cv(df.alpha5$GSDP.per.capita)
cv6 <- cv(df.alpha6$GSDP.per.capita)
cv7 <- cv(df.alpha7$GSDP.per.capita)
cv8 <- cv(df.alpha8$GSDP.per.capita)
cv9 <- cv(df.alpha9$GSDP.per.capita)
cv10 <- cv(df.alpha10$GSDP.per.capita)
cv11 <- cv(df.alpha11$GSDP.per.capita)
cv12 <- cv(df.alpha12$GSDP.per.capita)
cv13 <- cv(df.alpha13$GSDP.per.capita)
cv14 <- cv(df.alpha14$GSDP.per.capita)
cv15 <- cv(df.alpha15$GSDP.per.capita)
cv16 <- cv(df.alpha16$GSDP.per.capita)
cv17 <- cv(df.alpha17$GSDP.per.capita)
cv18 <- cv(df.alpha18$GSDP.per.capita)
cv19 <- cv(df.alpha19$GSDP.per.capita)
cv20 <- cv(df.alpha20$GSDP.per.capita)
cv21 <- cv(df.alpha21$GSDP.per.capita)

sigmas <- c(cv1, cv2, cv3, cv4, cv5, cv6, cv7, cv8, cv9, cv10, cv11, cv12,
            cv13, cv14, cv15, cv16, cv17, cv18, cv19, cv20, cv21)
years <- df.Andhra$Year

df.sigma <- data.frame(cbind(years, sigmas))

sigma.convergence <- ggplot(df.sigma, aes(years, sigmas)) +
        geom_point(aes(colour = sigmas)) +
        scale_colour_gradient(high = "#9966CC") + 
        labs(x = "Year", y = "Coefficient of Variation", colour = "Coefficient\n of Variation") +
        theme_bw() +
        theme(axis.line = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              legend.position = "none")


ggsave("sigma.convergence.pdf", plot = sigma.convergence)

#==========================================================================================
# Unconditional Beta Convergence
#==========================================================================================
# Avg.GSDP.Growth = a + Beta(initial.GSDP) + u
#
# Pooling model used, cross-sectional
# -----------------------------------------------------------------------------------------

shapiro.test(df.alpha$GSDP.Growth) #0.05353
shapiro.test(df.alpha$Lagged.Log.GSDP.per.capita) #0.001646 ... ok

resettest(Average.GSDP.Growth ~ Log.Initial.GSDP, type = "regressor", 
          data = df.alpha) # Functional form test (probably a diff relationship... unimportant)

states <- unique(df.alpha$State)
levels <- unique(df.alpha$Log.Initial.GSDP)
growth <- unique(df.alpha$Average.GSDP.Growth)

df.unconditional <- data.frame(cbind(states, levels, growth))

unconditional <- lm(growth ~ levels, data = df.unconditional)

summary(unconditional)

stargazer(unconditional)

#==========================================================================================
# Conditional Beta Convergence
#==========================================================================================
# GSDP.Growth = a + Beta(lagged.log.GSDP.per.capita) + Gamma(Variables) + u
#
# Pooled OLS
#------------------------------------------------------------------------------------------

correlations.pool <- cor(df.alpha[1:21,c(16, 18, 21, 19, 28, 26, 7, 20)])
corrplot(correlations.pool, method = "circle")

resettest(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + lag(Population.Growth, k = 1) + 
                  lag(log(Social.Expenditure), k = 1) + lag(log(Gross.Fixed.Capital.Formation), k = 1) +
                  lag(log(Personal.Loans.by.SCBs), k = 1) + lag(Literacy.Rate, k = 1) + lag(Per.Capita.Elec.Cons, k = 1) +
                  lag(Infant.Mortality.Rate, k = 1) + lag(Percentage.Ag.Share.GDP, k = 1), type = "regressor", 
          data = df.alpha) # Functional form test p = 0.04181

pool <- plm(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + lag(Population.Growth, k = 1) + 
                    lag(log(Social.Expenditure), k = 1) + lag(log(Gross.Fixed.Capital.Formation), k = 1) +
                    lag(log(Personal.Loans.by.SCBs), k = 1) + lag(Literacy.Rate, k = 1) + lag(Per.Capita.Elec.Cons, k = 1) +
                    lag(Infant.Mortality.Rate, k = 1) + lag(Percentage.Ag.Share.GDP, k = 1), 
            data = df.alpha, index = c("State", "Year"), model = "pooling")

summary(pool)

cov.p <- vcovHC(pool, type = "HC0", cluster = "group")
robust.se.p <- sqrt(diag(cov.p))

#==========================================================================================
# Fixed Effects
#==========================================================================================

fixed <- plm(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + lag(Population.Growth, k = 1) + 
                     lag(log(Social.Expenditure), k = 1) + lag(log(Gross.Fixed.Capital.Formation), k = 1) +
                     lag(log(Personal.Loans.by.SCBs), k = 1) + lag(Literacy.Rate, k = 1) + lag(Per.Capita.Elec.Cons, k = 1) +
                     lag(Infant.Mortality.Rate, k = 1) + lag(Percentage.Ag.Share.GDP, k = 1), 
             data = df.alpha, index = c("State", "Year"), model = "within")

cov.f <- vcovHC(fixed, type = "HC0", cluster = "group")
robust.se.f <- sqrt(diag(cov.f))

summary(fixed)


shapiro.test(fixed$residuals)


summary(plm(GSDP.Growth ~ factor(State), 
            data = df.alpha, index = c("State", "Year"), model = "within"))

summary(lm(GSDP.Growth ~ factor(State), data = df.alpha)) # Just state dummies

#------------------------------------------------------------------------------------------
# Testing for fixed effects
#------------------------------------------------------------------------------------------

pFtest(pool, fixed)

#==========================================================================================
# Fixed Effects w/ Time
#==========================================================================================

fixed.time <- plm(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + factor(Year) + lag(Population.Growth, k = 1) + 
                          lag(log(Social.Expenditure), k = 1) + lag(log(Gross.Fixed.Capital.Formation), k = 1) +
                          lag(log(Personal.Loans.by.SCBs), k = 1) + lag(Literacy.Rate, k = 1) + lag(Per.Capita.Elec.Cons, k = 1) +
                          lag(Infant.Mortality.Rate, k = 1) + lag(Percentage.Ag.Share.GDP, k = 1), 
                  data = df.alpha, index = c("State", "Year"), model = "within")

cov.f.t <- vcovHC(fixed.time, type = "HC0", cluster = "group")
robust.se.f.t <- sqrt(diag(cov.f.t))

summary(fixed.time)

shapiro.test(fixed.time$residuals)

resettest(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + factor(Year) + lag(Population.Growth, k = 1) + 
                  lag(log(Social.Expenditure), k = 1) + lag(log(Gross.Fixed.Capital.Formation), k = 1) +
                  lag(log(Personal.Loans.by.SCBs), k = 1) + lag(Literacy.Rate, k = 1) + lag(Per.Capita.Elec.Cons, k = 1) +
                  lag(Infant.Mortality.Rate, k = 1) + lag(Percentage.Ag.Share.GDP, k = 1), type = "regressor", 
          data = df.alpha)

# Null is that no time-fixed effect needed
# If p < 0.05 then use time-fixed effects

pFtest(fixed.time, fixed) #Use time effects

#==========================================================================================
# Hausman Test [Fixed vs. Random]
#==========================================================================================
# Tests whether the unique errors (u) are correlated with the regressors
# Null hypothesis is that they are not
#------------------------------------------------------------------------------------------

random <- plm(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + lag(Population.Growth, k = 1) + 
                      lag(log(Social.Expenditure), k = 1) + lag(log(Gross.Fixed.Capital.Formation), k = 1) +
                      lag(log(Personal.Loans.by.SCBs), k = 1) + lag(Literacy.Rate, k = 1) + lag(Per.Capita.Elec.Cons, k = 1) +
                      lag(Infant.Mortality.Rate, k = 1) + lag(Percentage.Ag.Share.GDP, k = 1), 
              data = df.alpha, index = c("State", "Year"), model = "random")


phtest(fixed, random) #Use Fixed Effects

#==========================================================================================
# Testing for heteroskedasticity
#==========================================================================================
# The null hypothesis for the Breusch-Pagan test is homoskedasticity.
# Breusch-Pagan test for model because of amount of regressors.
#------------------------------------------------------------------------------------------

bptest(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + lag(Population.Growth, k = 1) + 
               lag(log(Social.Expenditure), k = 1) + lag(log(Gross.Fixed.Capital.Formation), k = 1) +
               lag(log(Personal.Loans.by.SCBs), k = 1) + lag(Literacy.Rate, k = 1) + lag(Per.Capita.Elec.Cons, k = 1) +
               lag(Infant.Mortality.Rate, k = 1) + lag(Percentage.Ag.Share.GDP, k = 1), 
       data = df.alpha, studentize = F) # Heteroskedasticity :/

#==========================================================================================
# Generalized Least Squares
#==========================================================================================

gls.p <- pggls(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + lag(Population.Growth, k = 1) + 
                       lag(log(Social.Expenditure), k = 1) + lag(log(Gross.Fixed.Capital.Formation), k = 1) +
                       lag(log(Personal.Loans.by.SCBs), k = 1) + lag(Literacy.Rate, k = 1) + lag(Per.Capita.Elec.Cons, k = 1) +
                       lag(Infant.Mortality.Rate, k = 1) + lag(Percentage.Ag.Share.GDP, k = 1), 
               data = df.alpha, index = c("State", "Year"), model = "pooling")

summary(gls.p)


gls.f <- pggls(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + lag(Population.Growth, k = 1) + 
                       lag(log(Social.Expenditure), k = 1) + lag(log(Gross.Fixed.Capital.Formation), k = 1) +
                       lag(log(Personal.Loans.by.SCBs), k = 1) + lag(Literacy.Rate, k = 1) + lag(Per.Capita.Elec.Cons, k = 1) +
                       lag(Infant.Mortality.Rate, k = 1) + lag(Percentage.Ag.Share.GDP, k = 1), 
               data = df.alpha, index = c("State", "Year"), model = "within")

summary(gls.f)

shapiro.test(gls.f$residuals)

gls.f.t <- pggls(GSDP.Growth ~ Lagged.Log.GSDP.per.capita + factor(Year) + lag(Population.Growth, k = 1) + 
                         lag(log(Social.Expenditure), k = 1) + lag(log(Gross.Fixed.Capital.Formation), k = 1) +
                         lag(log(Personal.Loans.by.SCBs), k = 1) + lag(Literacy.Rate, k = 1) + lag(Per.Capita.Elec.Cons, k = 1) +
                         lag(Infant.Mortality.Rate, k = 1) + lag(Percentage.Ag.Share.GDP, k = 1), 
                 data = df.alpha, index = c("State", "Year"), model = "within")

summary(gls.f.t)

shapiro.test(gls.f.t$residuals)


#==========================================================================================
# Testing for cross-sectional dependence/contemporaneous correlation: using
# Breusch-Pagan LM test of independence and Pasaran CD test
#==========================================================================================
# Serial correlation tests apply to macro panels with long time series. Not a problem in 
# micro panels (with very few years). The null is that there is not serial correlation.
#------------------------------------------------------------------------------------------

pbgtest(fixed) # p-value = 0.008253, serial correlation (hence clustered SEs)

#==========================================================================================
# Testing for Unit Root
#==========================================================================================

adf.test(fixed$residuals) # Stationary, so co-integrated
adf.test(pool$residuals) # Stationary
adf.test(fixed.time$residuals) # Stationary

#==========================================================================================
# Stargazer Tables
#==========================================================================================

stargazer(fixed, fixed, se = list(robust.se.f, NULL),
          column.labels=c("Fixed", "Fixed GLS"), single.row = T)

stargazer(fixed.time, fixed.time, se = list(robust.se.f.t, NULL),
          column.labels=c("Fixed w/ Time", "Fixed w/ Time GLS"), single.row = T)

