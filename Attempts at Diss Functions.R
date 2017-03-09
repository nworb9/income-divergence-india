##  df.alpha.plot


#==========================================================================================
# Factor Analysis
#==========================================================================================

drops <- c("GSDP.per.capita", "Log.Initial.GSDP", "GSDP.Growth", "Log.GSDP",
           "Average.GSDP.Growth", "Income.Rank", "GSDP.Base.1980", "Population", "State",
           "Year")

corMat <- cor(x = df.alpha[,!names(df.alpha %in% drops)], use = "complete.obs", method = c("pearson"))
solution <- fa(r = corMat)
solution

fit <- factanal(data = df.alpha[,!names(df.alpha %in% drops)], factor = 1 , covmat = corMat, na.action = na.omit)
fit

#------------------------------------------------------------------------------------------

names <- c("Andhra.Pradesh", "Assam", "Bihar", "Gujarat", "Haryana", "Karnataka",
           "Kerala", "Madhya.Pradesh", "Maharashtra", "Odisha", "Punjab", "Rajasthan",
           "Tamil.Nadu", "Uttar.Pradesh", "West.Bengal")

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


##  [Manually Cleaning Data for Ggplot2 ?????????(???_???)?????????] ##

Pop <- as.vector(df.Andhra$Population)
Population <- na.approx(Pop)
df.Andhra$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Assam$Population)
Population <- na.approx(Pop)
df.Assam$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Bihar$Population)
Population <- na.approx(Pop)
df.Bihar$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Gujarat$Population)
Population <- na.approx(Pop)
df.Gujarat$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Haryana$Population)
Population <- na.approx(Pop)
df.Haryana$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Karnataka$Population)
Population <- na.approx(Pop)
df.Karnataka$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Kerala$Population)
Population <- na.approx(Pop)
df.Kerala$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Madhya$Population)
Population <- na.approx(Pop)
df.Madhya$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Maha$Population)
Population <- na.approx(Pop)
df.Maha$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Odisha$Population)
Population <- na.approx(Pop)
df.Odisha$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Punjab$Population)
Population <- na.approx(Pop)
df.Punjab$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Raja$Population)
Population <- na.approx(Pop)
df.Raja$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Tamil$Population)
Population <- na.approx(Pop)
df.Tamil$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.UP$Population)
Population <- na.approx(Pop)
df.UP$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.WBengal$Population)
Population <- na.approx(Pop)
df.WBengal$Population <- as.numeric(as.character(Population))

df.alpha.plot <- do.call("rbind", list(df.Andhra, df.Assam, df.Bihar, df.Gujarat,
                                       df.Haryana, df.Karnataka, df.Kerala, df.Madhya,
                                       df.Maha, df.Odisha, df.Punjab, df.Raja, df.Tamil,
                                       df.UP, df.WBengal))

##  New intepretable values

##  GSDP

df.alpha.plot$GSDP.per.capita <- df.alpha.plot$GSDP.Base.1980/df.alpha.plot$Population

##  Corruption Convictions

df.alpha.plot$Corruption.Convictions <- df.alpha.plot$Corruption.Convictions/df.alpha.plot$Population

##  Grain Yields

df.alpha.plot$Grain.Yields <- df.alpha.plot$Grain.Yields/df.alpha.plot$Population

##  Rural Bank Branches

df.alpha.plot$Rural.Bank.Branches <- df.alpha.plot$Rural.Bank.Branches/df.alpha.plot$Population

##  Capex

df.alpha.plot$Capex <- df.alpha.plot$Capex/df.alpha.plot$Population

##  Credit.by.SCBs

df.alpha.plot$Credit.by.SCBs <- df.alpha.plot$Credit.by.SCBs/df.alpha.plot$GSDP.Base.1980

##  Credit/Deposit Ratio

df.alpha.plot$Credit.Deposit.Ratio <- df.alpha.plot$Credit.Deposit.Ratio/df.alpha.plot$GSDP.Base.1980

##  Credit to Agriculture

df.alpha.plot$Credit.to.Ag <- df.alpha.plot$Credit.to.Ag/df.alpha.plot$GSDP.Base.1980

##  Credit to Industry

df.alpha.plot$Credit.to.Industry <- df.alpha.plot$Credit.to.Industry/df.alpha.plot$GSDP.Base.1980

##  Gross Fixed Capital Formation

df.alpha.plot$Gross.Fixed.Capital.Formation <- df.alpha.plot$Gross.Fixed.Capital.Formation/df.alpha.plot$GSDP.Base.1980

##  Km Highways

df.alpha.plot$Km.Highways <- df.alpha.plot$Km.Highways/df.alpha.plot$Population

##  Personal Loans by SCBs

df.alpha.plot$Personal.Loans.by.SCBs <- df.alpha.plot$Personal.Loans.by.SCBs/df.alpha.plot$Population

##  Total Registered Vehicles

df.alpha.plot$Total.Registered.Vehicles <- df.alpha.plot$Total.Registered.Vehicles/df.alpha.plot$Population

##  Social Expenditure

df.alpha.plot$Social.Expenditure <- df.alpha.plot$Social.Expenditure/df.alpha.plot$GSDP.Base.1980

##  Gross Fiscal Deficit

df.alpha.plot$Gross.Fiscal.Deficit <- df.alpha.plot$Gross.Fiscal.Deficit/df.alpha.plot$GSDP.Base.1980

##  Log GSDP

df.alpha.plot$Log.GSDP <- log(df.alpha.plot$GSDP.per.capita)

##  Growth

df.alpha.plot$GSDP.Growth <- c(NA, diff(df.alpha.plot$Log.GSDP, lag = 1))





##  Linearly Interpolate Population

# time_interpolate is a helper function for TimeInterpolateByGroup
# that operates on each of the groups. In the input to this function,
# the GroupingVariable column of the data frame should be single-valued.
# The function returns a (probably longer) data frame, with estimated
# values for the times specified in the output_times array.
time_interpolate <- function(data_frame,
                             GroupingVariable,
                             time_var,
                             output_times) {
        input_times <- data_frame[, time_var]
        exclude_vars <- c(time_var, GroupingVariable)
        value_vars <- setdiff(colnames(data_frame), exclude_vars)
        output_df <- data.frame(rep(data_frame[1:315,GroupingVariable], length(315)), output_times)
        colnames(output_df) <- c(GroupingVariable, time_var)
        for (value_var in value_vars) {
                output_df[,value_var] <- approx(input_times, data_frame[, value_var], output_times)$y
        }
        return(output_df)
}

output_df <- data.frame(rep(df.alpha[1:315,"State"], length(16)),  seq(from = 1991, to = 2011, by = 1))

# A test for time_interpolate
AA <- time_interpolate(df.alpha, "State" , "Year", seq(from=1991, to=2011, by=1))

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



AB = time_interpolate(df.alpha, "State", "Year", seq(from=1991, to=2011, by=1))

##  Manual way

Pop <- as.vector(df.Andhra$Population)
Population <- na.approx(Pop)
df.Andhra$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Assam$Population)
Population <- na.approx(Pop)
df.Assam$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Bihar$Population)
Population <- na.approx(Pop)
df.Bihar$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Gujarat$Population)
Population <- na.approx(Pop)
df.Gujarat$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Haryana$Population)
Population <- na.approx(Pop)
df.Haryana$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Karnataka$Population)
Population <- na.approx(Pop)
df.Karnataka$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Kerala$Population)
Population <- na.approx(Pop)
df.Kerala$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Madhya$Population)
Population <- na.approx(Pop)
df.Madhya$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Maha$Population)
Population <- na.approx(Pop)
df.Maha$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Odisha$Population)
Population <- na.approx(Pop)
df.Odisha$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Punjab$Population)
Population <- na.approx(Pop)
df.Punjab$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Raja$Population)
Population <- na.approx(Pop)
df.Raja$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.Tamil$Population)
Population <- na.approx(Pop)
df.Tamil$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.UP$Population)
Population <- na.approx(Pop)
df.UP$Population <- as.numeric(as.character(Population))

Pop <- as.vector(df.WBengal$Population)
Population <- na.approx(Pop)
df.WBengal$Population <- as.numeric(as.character(Population))

##  Stack Overflow Shit

dataIpol <- df.alpha %>%
        group_by(State) %>% 
        arrange(State, Year) %>%
        filter(sum(!is.na(Population))>=2) %>% #filter!
        mutate(Population = approx(Year, Population, Year, 
                                   method = "linear", rule = 1, f = 0, ties = mean)$y)

dataIpol <- df.alpha %>%
        group_by(State) %>% 
        arrange(State, Year) %>%
        mutate(Population = approx(Year, Population, Year, 
                                   method = "linear", rule = 1, f = 0, ties = mean)$y)


df.beta <- df.alpha %>% 
        group_by(State) %>%
        arrange(State, Year) %>%
        ddply()

df.beta <- df.alpha %>%
        group_by(State) %>%
        arrange(State, Year) %>%
        for i in (c("Population", "Water.Access" "Birth.Rate",
                    "Corruption.Convictions", "Grain.Yields", "HDI",
                    "Percent.Irrigated", "Rural.Bank.Branches", "Capex",
                    "Credit.by.SCBs", "Credit.Deposit.Ratio", "Credit.to.Ag",
                    "Credit.to.Industry", "Gross.Fixed.Capital.Formation",
                    "Km.Highways", "Infant.Mortality.Rate", "Literacy.Rate",
                    "Per.Capita.Elec.Cons", "Personal.Loans.by.SCBs",
                    "Percentage.BPL", "Total.Registered.Vehicles",
                    "Approx.Life.Expectancy", "GSDP.Base.1980",
                    "Percentage.Ag.Share.GDP", "Share.Rural.Pop", "Social.Expenditure",
                    "Gross.FIscal.Deficit")) {
                mutate(i = approx(Year, i, Year, 
                                  method = "linear", rule = 1, f = 0, ties = mean)$y)
        }

for i in (c("Population", "Water.Access", "Corruption.Convictions", "HDI", "Infant.Mortality.Rate", 
            "Literacy.Rate", "Percentage.BPL", "Total.Registered.Vehicles", "Share.Rural.Pop"){
        mutate(i)
}

