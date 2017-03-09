#==========================================================================================
# Getting Data
#==========================================================================================


setwd("C:\\Users\\nworb95\\Desktop\\Dissertation RStudio")

checkAndDownload <- function(packageNames) {
        for(packageName in packageNames) {
                if(!isInstalled(packageName)) {
                        install.packages(packageName,repos="http://lib.stat.cmu.edu/R/CRAN") 
                } 
                library(packageName, character.only=TRUE,quietly=TRUE,verbose=FALSE)
        }
}

isInstalled <- function(mypkg){
        is.element(mypkg, installed.packages()[,1])
}

packages <- c("sp", "sandwich", "gplots", "zoo", "plm", "car", "ggplot2",
              "plyr", "rgeos", "sqldf", "RColorBrewer", "dplyr", "dtplyr",
              "data.table", "corrplot", "tseries", "lmtest", "psych")
checkAndDownload(packages)



#------------------------------------------------------------------------------------------
#  Load data
#------------------------------------------------------------------------------------------

df.alpha <- read.csv("India_Regional_Disparities.csv", stringsAsFactors = T)
head(df.alpha)

#------------------------------------------------------------------------------------------
# Line graphs to check for outliers
#------------------------------------------------------------------------------------------

ggplot(data = df.alpha, aes(x = Year, y = Water.Access, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Birth.Rate, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)  ##  Large outlier for Andhra.Pradesh

ggplot(data = df.alpha, aes(x = Year, y = Corruption.Convictions, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Grain.Yields, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Percent.Irrigated, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Rural.Bank.Branches, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Capex, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Credit.by.SCBs, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Credit.Deposit.Ratio, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Credit.to.Ag, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Credit.to.Industry, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Gross.Fixed.Capital.Formation, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Km.Highways, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5) ##  Critical outlier for Bihar

ggplot(data = df.alpha, aes(x = Year, y = Infant.Mortality.Rate, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Literacy.Rate, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Per.Capita.Elec.Cons, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Personal.Loans.by.SCBs, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Percentage.BPL, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Total.Registered.Vehicles, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Approx.Life.Expectancy, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Percentage.Ag.Share.GDP, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Share.Rural.Pop, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Social.Expenditure, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)

ggplot(data = df.alpha, aes(x = Year, y = Gross.Fiscal.Deficit, color = State), na.omit = T) +
        xlim(1991, 2015) +
        geom_line(size = 1) +
        geom_point(size = 1.5)