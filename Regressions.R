##  [Regressions]  ##

##  plm package

##  pdata.frame() -makes data a panel data frame
##  make.pbalanced() -each individual has the same time periods
##  lag() - lags data

a <- plm(GSDP.Base.1980 ~ Grain.Yields + Corruption.Convictions + Percent.Irrigated + Birth.Rate +
            Water.Access + Rural.Bank.Branches + Capex + Credit.by.SCBs + Credit.Deposit.Ratio +
            Credit.to.Ag + Credit.to.Industry + Gross.Fixed.Capital.Formation + Km.Highways + Infant.Mortality.Rate +
            Literacy.Rate + Per.Capita.Elec.Cons + Personal.Loans.by.SCBs + Percentage.BPL + Total.Registered.Vehicles +
            Approx.Life.Expectancy + Percentage.Ag.Share.GDP + Share.Rural.Pop + Social.Expenditure + Gross.Fiscal.Deficit,
    data = df.alpha, index = c("State", "Year"), model = "random", effect = "time")

p <- plm(GSDP.Base.1980 ~ Grain.Yields + Percent.Irrigated + Capex + Credit.by.SCBs + 
            Percentage.Ag.Share.GDP + Share.Rural.Pop + Social.Expenditure,
    data = df.alpha, index = c("State", "Year"), model = "within", effect = "time")
library(sandwich)
robust.se = sqrt(diag(vcovHC(a, type =  "HC1")))
library(lmtest)
coeftest(a, robust.se)


summary(a)

