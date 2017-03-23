#==========================================================================================
# Regressions 
#==========================================================================================


#------------------------------------------------------------------------------------------
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

#------------------------------------------------------------------------------------------
# Coefficients of Variation
#------------------------------------------------------------------------------------------
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
years <- 