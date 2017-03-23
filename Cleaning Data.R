#==========================================================================================
# Cleaning Data
#==========================================================================================
# Drop empty columns
#------------------------------------------------------------------------------------------

drop.cols <- c('X', 'X.1')
df.alpha <- df.alpha %>% select(-one_of(drop.cols))
df.alpha$State[df.alpha$State=='']=NA
df.alpha$State = droplevels(df.alpha$State)
head(df.alpha)

#------------------------------------------------------------------------------------------
# Fix Bihar km highways (original data reports 35339, a value 10x more than previous 
# year's value, and greater than the next year's value by a similar factor)
#------------------------------------------------------------------------------------------



df.alpha$Km.Highways[105] ##  35339 reported, clearly an error
df.alpha$Km.Highways[105] <- 3533
df.alpha$Km.Highways[105] ##  3533

#------------------------------------------------------------------------------------------
#  Assam's Percentage.BPL values are missing
#------------------------------------------------------------------------------------------

df.alpha$Percentage.BPL[69] <- 40.86
df.alpha$Percentage.BPL[75] <- 36.09
df.alpha$Percentage.BPL[80] <- 34.04
df.alpha$Percentage.BPL[85] <- 37.90

#------------------------------------------------------------------------------------------
#  Select time period
#------------------------------------------------------------------------------------------

df.alpha <- subset(df.alpha, State != "All.India" & Year <= 2011 & Year > 1990)

#------------------------------------------------------------------------------------------
#  Interpolate values
#------------------------------------------------------------------------------------------

colnames(df.alpha)[unlist(lapply(df.alpha, function(x) anyNA(x)))]




df.alpha <- df.alpha %>%
        group_by(State) %>% 
        arrange(State, Year) %>%
        mutate(Population = approx(Year, Population, Year, 
                                   method = "linear", rule = 1, f = 0, ties = mean)$y) %>%
        mutate(Total.Registered.Vehicles = approx(Year, Total.Registered.Vehicles, Year, 
                                                  method = "linear", rule = 1, f = 0, ties = mean)$y) %>%
        mutate(Share.Rural.Pop = approx(Year, Share.Rural.Pop, Year, 
                                        method = "linear", rule = 1, f = 0, ties = mean)$y)


#==========================================================================================
# New Interpretable Values
#==========================================================================================
#  GSDP.per.capita
#------------------------------------------------------------------------------------------

df.alpha$GSDP.per.capita <- df.alpha$GSDP.Base.1980/df.alpha$Population

#------------------------------------------------------------------------------------------
#  Corruption Convictions
#------------------------------------------------------------------------------------------

df.alpha$Corruption.Convictions <- df.alpha$Corruption.Convictions/df.alpha$Population

#------------------------------------------------------------------------------------------
#  Grain Yields
#------------------------------------------------------------------------------------------

df.alpha$Grain.Yields <- df.alpha$Grain.Yields/df.alpha$Population

#------------------------------------------------------------------------------------------
#  Rural Bank Branches
#------------------------------------------------------------------------------------------

df.alpha$Rural.Bank.Branches <- df.alpha$Rural.Bank.Branches/df.alpha$Population

#------------------------------------------------------------------------------------------
#  Capex
#------------------------------------------------------------------------------------------

df.alpha$Capex <- df.alpha$Capex/df.alpha$Population

#------------------------------------------------------------------------------------------
#  Credit.by.SCBs
#------------------------------------------------------------------------------------------

df.alpha$Credit.by.SCBs <- df.alpha$Credit.by.SCBs/df.alpha$GSDP.Base.1980

#------------------------------------------------------------------------------------------
#  Credit/Deposit Ratio
#------------------------------------------------------------------------------------------

df.alpha$Credit.Deposit.Ratio <- df.alpha$Credit.Deposit.Ratio/df.alpha$GSDP.Base.1980

#------------------------------------------------------------------------------------------
#  Credit to Agriculture
#------------------------------------------------------------------------------------------

df.alpha$Credit.to.Ag <- df.alpha$Credit.to.Ag/df.alpha$GSDP.Base.1980

#------------------------------------------------------------------------------------------
#  Credit to Industry
#------------------------------------------------------------------------------------------

df.alpha$Credit.to.Industry <- df.alpha$Credit.to.Industry/df.alpha$GSDP.Base.1980

#------------------------------------------------------------------------------------------
#  Gross Fixed Capital Formation
#------------------------------------------------------------------------------------------

df.alpha$Gross.Fixed.Capital.Formation <- df.alpha$Gross.Fixed.Capital.Formation/df.alpha$GSDP.Base.1980

#------------------------------------------------------------------------------------------
#  Km Highways
#------------------------------------------------------------------------------------------

df.alpha$Km.Highways <- df.alpha$Km.Highways/df.alpha$Population

#------------------------------------------------------------------------------------------
#  Personal Loans by SCBs
#------------------------------------------------------------------------------------------

df.alpha$Personal.Loans.by.SCBs <- df.alpha$Personal.Loans.by.SCBs/df.alpha$Population

#------------------------------------------------------------------------------------------
#  Total Registered Vehicles
#------------------------------------------------------------------------------------------

df.alpha$Total.Registered.Vehicles <- df.alpha$Total.Registered.Vehicles/df.alpha$Population

#------------------------------------------------------------------------------------------
#  Social Expenditure
#------------------------------------------------------------------------------------------

df.alpha$Social.Expenditure <- df.alpha$Social.Expenditure/df.alpha$GSDP.Base.1980

#------------------------------------------------------------------------------------------
#  Gross Fiscal Deficit
#------------------------------------------------------------------------------------------

df.alpha$Gross.Fiscal.Deficit <- df.alpha$Gross.Fiscal.Deficit/df.alpha$GSDP.Base.1980

#------------------------------------------------------------------------------------------
#  Log GSDP
#------------------------------------------------------------------------------------------

df.alpha$Log.GSDP.per.capita <- log(df.alpha$GSDP.per.capita)

#------------------------------------------------------------------------------------------
#  Rank GSDP.per.capita
#------------------------------------------------------------------------------------------

df.alpha <- transform(df.alpha,
                      Income.Rank = ave(GSDP.per.capita, Year,
                                        FUN = function(x) rank(-x, ties.method = "first")))

#------------------------------------------------------------------------------------------
#  Log Initial GSDP
#------------------------------------------------------------------------------------------

df.alpha <- df.alpha %>%
        group_by(State) %>% 
        arrange(State, Year) %>%
        mutate(Log.Initial.GSDP = log(GSDP.Base.1980[1]))

#------------------------------------------------------------------------------------------
#  Coerce into panel data frame
#------------------------------------------------------------------------------------------

df.alpha <- pdata.frame(df.alpha, index = c("State", "Year"), row.names = F)
df.alpha$Year <- as.numeric(df.alpha$Year)

#------------------------------------------------------------------------------------------
#  Growth
#------------------------------------------------------------------------------------------

df.alpha$GSDP.Growth <- diff(df.alpha$Log.GSDP.per.capita, lag = 1)

#------------------------------------------------------------------------------------------
#  Log Population
#------------------------------------------------------------------------------------------

df.alpha$Log.Population <- log(df.alpha$Population)

#------------------------------------------------------------------------------------------
#  Population Growth
#------------------------------------------------------------------------------------------

df.alpha$Population.Growth <- diff(df.alpha$Log.Population, lag = 1)

#------------------------------------------------------------------------------------------
#  Average Growth
#------------------------------------------------------------------------------------------

df.alpha <- df.alpha %>%
        group_by(State) %>% 
        arrange(State) %>%
        mutate(Average.GSDP.Growth = mean(GSDP.Growth, na.rm = T))

#------------------------------------------------------------------------------------------
#  Lagged GDSP per capita
#------------------------------------------------------------------------------------------

df.alpha$Lagged.Log.GSDP.per.capita <- lag(df.alpha$Log.GSDP.per.capita, k = 1)

#------------------------------------------------------------------------------------------
# Subset alpha frame
#------------------------------------------------------------------------------------------

df.Andhra <- subset(df.alpha, State == "Andhra.Pradesh")
df.Assam <- subset(df.alpha, State == "Assam")
df.Bihar <- subset(df.alpha, State == "Bihar")
df.Gujarat <- subset(df.alpha, State == "Gujarat")
df.Haryana <- subset(df.alpha, State == "Haryana")
df.Karnataka <- subset(df.alpha, State == "Karnataka")
df.Kerala <- subset(df.alpha, State == "Kerala")
df.Madhya <- subset(df.alpha, State == "Madhya.Pradesh")
df.Maha <- subset(df.alpha, State == "Maharashtra")
df.Odisha <- subset(df.alpha, State == "Odisha")
df.Punjab <- subset(df.alpha, State == "Punjab")
df.Raja <- subset(df.alpha, State == "Rajasthan")
df.Tamil <- subset(df.alpha, State == "Tamil.Nadu")
df.UP <- subset(df.alpha, State == "Uttar.Pradesh")
df.WBengal <- subset(df.alpha, State == "West.Bengal")

#------------------------------------------------------------------------------------------
# Change Years
#------------------------------------------------------------------------------------------
df.alpha$Year[df.alpha$Year == 1] <- 1991 
df.alpha$Year[df.alpha$Year == 2] <- 1992
df.alpha$Year[df.alpha$Year == 3] <- 1993
df.alpha$Year[df.alpha$Year == 4] <- 1994
df.alpha$Year[df.alpha$Year == 5] <- 1995
df.alpha$Year[df.alpha$Year == 6] <- 1996
df.alpha$Year[df.alpha$Year == 7] <- 1997
df.alpha$Year[df.alpha$Year == 8] <- 1998
df.alpha$Year[df.alpha$Year == 9] <- 1999
df.alpha$Year[df.alpha$Year == 10] <- 2000
df.alpha$Year[df.alpha$Year == 11] <- 2001
df.alpha$Year[df.alpha$Year == 12] <- 2002
df.alpha$Year[df.alpha$Year == 13] <- 2003
df.alpha$Year[df.alpha$Year == 14] <- 2004
df.alpha$Year[df.alpha$Year == 15] <- 2005
df.alpha$Year[df.alpha$Year == 16] <- 2006
df.alpha$Year[df.alpha$Year == 17] <- 2007
df.alpha$Year[df.alpha$Year == 18] <- 2008
df.alpha$Year[df.alpha$Year == 19] <- 2009
df.alpha$Year[df.alpha$Year == 20] <- 2010
df.alpha$Year[df.alpha$Year == 21] <- 2011
