##  [Cleaning Data]  ##

##  Drop empty columns

drop.cols <- c('X', 'X.1')
df.alpha <- df.alpha %>% select(-one_of(drop.cols))
df.alpha$State[df.alpha$State=='']=NA
df.alpha$State = droplevels(df.alpha$State)
head(df.alpha)

##  Fix Bihar km highways (original data reports 35339, a value 10x more 
##  than previous year's value, and greater than the next year's value by
##  a similar factor)

df.alpha$Km.Highways[105] ##  35339 reported, clearly an error
df.alpha$Km.Highways[105] <- 3533
df.alpha$Km.Highways[105] ##  3533

##  Select time period

df.alpha <- subset(df.alpha, State != "All.India" & Year <= 2011 & Year > 1990)

##  Linearly Interpolate NAs

time_interpolate <- function(data_frame,
                             GroupingVariable,
                             time_var,
                             output_times) {
        input_times <- data_frame[, time_var]
        exclude_vars <- c(time_var, GroupingVariable)
        value_vars <- setdiff(colnames(data_frame), exclude_vars)
        output_df <- data.frame(rep(data_frame[1,GroupingVariable], length(output_times)), output_times)
        colnames(output_df) <- c(GroupingVariable, time_var)
        for (value_var in value_vars) {
                output_df[,value_var] <- approx(input_times, data_frame[, value_var], output_times)$y
        }
        return(output_df)
}

time_interpolate(df.alpha, "State" , "Year", seq(from=1991, to=2011, by=1))

TimeInterpolateByGroup <- function(DataFrame, 
                                   GroupingVariable, 
                                   TimeVariable,
                                   TimeInterval){
        min_time <- min(DataFrame[, TimeVariable])
        max_time <- max(DataFrame[, TimeVariable])
        output_times <- seq(from=min_time, to=max_time, by=TimeInterval)
        ddply(DataFrame,
              GroupingVariable,
              time_interpolate,
              GroupingVariable=GroupingVariable,
              time_var=TimeVariable,
              output_times=output_times)
}

TimeInterpolateByGroup(df.alpha, "State", "Year", 1)

df.alpha$Year <- as.integer(df.alpha$Year)

##  Subset alpha frame

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

##  New interpretable variables  ##

##  Coerce into panel data frame

df.alpha <- pdata.frame(df.alpha, index = c("State", "Year"))
df.alpha$Year <- as.numeric(df.alpha$Year)

##  GSDP

df.alpha$GSDP.per.capita <- df.alpha$GSDP.Base.1980/df.alpha$Population

##  Corruption Convictions

df.alpha$Corruption.Convictions <- df.alpha$Corruption.Convictions/df.alpha$Population

##  Grain Yields

df.alpha$Grain.Yields <- df.alpha$Grain.Yields/df.alpha$Population

##  Rural Bank Branches

df.alpha$Rural.Bank.Branches <- df.alpha$Rural.Bank.Branches/df.alpha$Population

##  Capex

df.alpha$Capex <- df.alpha$Capex/df.alpha$Population

##  Credit.by.SCBs

df.alpha$Credit.by.SCBs <- df.alpha$Credit.by.SCBs/df.alpha$GSDP.Base.1980

##  Credit/Deposit Ratio

df.alpha$Credit.Deposit.Ratio <- df.alpha$Credit.Deposit.Ratio/df.alpha$GSDP.Base.1980

##  Credit to Agriculture

df.alpha$Credit.to.Ag <- df.alpha$Credit.to.Ag/df.alpha$GSDP.Base.1980

##  Credit to Industry

df.alpha$Credit.to.Industry <- df.alpha$Credit.to.Industry/df.alpha$GSDP.Base.1980

##  Gross Fixed Capital Formation

df.alpha$Gross.Fixed.Capital.Formation <- df.alpha$Gross.Fixed.Capital.Formation/df.alpha$GSDP.Base.1980

##  Km Highways

df.alpha$Km.Highways <- df.alpha$Km.Highways/df.alpha$Population

##  Personal Loans by SCBs

df.alpha$Personal.Loans.by.SCBs <- df.alpha$Personal.Loans.by.SCBs/df.alpha$Population

##  Total Registered Vehicles

df.alpha$Total.Registered.Vehicles <- df.alpha$Total.Registered.Vehicles/df.alpha$Population

##  Social Expenditure

df.alpha$Social.Expenditure <- df.alpha$Social.Expenditure/df.alpha$GSDP.Base.1980

##  Gross Fiscal Deficit

df.alpha$Gross.Fiscal.Deficit <- df.alpha$Gross.Fiscal.Deficit/df.alpha$GSDP.Base.1980

##  Log GSDP

df.alpha$Log.GSDP <- log(df.alpha$GSDP.per.capita)

##  Growth

library(plm)
df.alpha$GSDP.Growth <- diff(df.alpha$Log.GSDP, lag = 1)

