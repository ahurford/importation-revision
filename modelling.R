## Winter 2022
## Project: Importation Risk
## Zahra Mohammadi

# Revised by Amy Hurford on October 18, 2024. In the code, general linear models (GLM)
# were changed to generalized linear models (poisson).
# The original GLM code by Zahra is commented out. The 
# mechanistic model is changed to be evaluated relative to a Poisson distribution.
# Rather than reporting R^2, the code was changed to report the likelihood test ratio
# statistic, since R^2 cannot be interpreted as the proportion of the variance
# explained for generalized linear models. A section was also added to consider travel
# volume with just the IATA data.

# Revised again by Amy Hurford on June 19, 2025 to consider international countries
# of origin. To do this a new file TDF-clean.R was added to clean the TDF data so
# that all countries that comprise 2.5% or more for at least one 3 month period
# are considered. The corrections for excluded travelers from international origins are
# just those from the TDF data, and this is just a *fixed number* of individuals
# exempt from completing the TDF. Therefore, in this code, we will distribute the
# exempted international travelers across the countries of origin.

# Minor revisions by Amy Hurford on October 1, 2025. These are as requested
# by reviewers. Here, instead of multiplying travel volume by infection prevalance
# I do a complete interaction model. In "epimodel-versions.R" we do a different
# version of the mechanistic model that fits the data better. At the start of
# "plots.R" there is the deviance test as requested by the reviewer.


##---------- 1. Load libraries ------------
source("preliminaries.R")
require(MASS)
select <- dplyr::select

##---------- 2. Read data ------------
# COVID cases in origins (CA excluded NL and US would consider as International)
source("getcases.pro.R") # output: Case and pop

#Seroprevalence Data and Correction factor calculated in 
source("getsero-CF.R") # output: CF

# daily travel volume calculated in Volume-focusOrigin.R
# This is the total travel volume including corrections for exclusions
Volume <- read.csv("Data/TraVolData/TotalTraVolume.csv")
Volume$date <- as.Date(as.character(Volume$date), format = "%Y-%m-%d")

# This does not have the exemptions
Volume.Int <- read.csv("Data/TraVolData/TravelVolumeNLCHIfiltered.csv")%>%
  select(AllFR, France, Guyana, India, Norway, Philippines, Spain, UK, USA)%>%
  mutate(Other.Int = AllFR-France-Guyana-India-Norway-Philippines-Spain-UK-USA)%>%
  rename(US=USA)

Volume.Int.prop = Volume.Int/Volume.Int$AllFR
Volume.Int.prop$AllFR[is.na(Volume.Int.prop$AllFR)] = 1
Volume.Int.prop[is.na(Volume.Int.prop)] = 0
Volume.Int.with.exemptions = Volume.Int.prop*Volume$INT

Volume.Int.with.exemptions%>%select(France, Guyana, India, Norway, Philippines, Spain, UK, US, Other.Int)

# This is the travel volume for all the provinces, territories and countries
# with correction for exempted travelers.
Volume = data.frame(Volume, Volume.Int.with.exemptions)%>%
  select(-AllFR, -INT)

#break down daily volume to regular travel vs rotational workers (200 daily-rw)
rw_Volume <- Volume %>% mutate(AB = 114, ON= 30, BC= 8 , SK = 4, MB = 2, QC = 4, 
                               NB = 8, NS = 16, PEI = 4, TR = 10 , France = 0, Guyana = 0, India = 0, Norway = 0, Philippines=0, Spain = 0, UK=0, US=0, Other.Int=0)
#min(Volume$..) AB, Sk, PEI , NB and TR are less than this values needs to fix 
fixor <- c("AB","PEI", "SK", "TR", "NB")
for (i in 1:length(fixor)){
  pr <- fixor[i]
  for (j in 1:nrow(rw_Volume)){
    if (rw_Volume[j, pr] > Volume[j, pr] ){rw_Volume[j,pr] <- Volume[j, pr]}
  }
}

# Regular travelers
origins <- c("AB", "BC", "MB" ,"NB" ,"NS" , "ON","PEI", "QC","SK", "TR", "France", "Guyana", "India", "Norway", "Philippines", "Spain", "UK", "US", "Other.Int")
rt_Volume <- Volume
for (i in 1:length(origins)){
  pr <- origins[i]
  rt_Volume[pr] <- Volume[pr] - rw_Volume[pr]
}

## Reported travel-related cases in NL
Travel_case <- read.csv("Data/Travel.Case.csv") 
Travel_case$date <- as.Date(Travel_case$date, format = "%Y-%m-%d") 

##-------------3. Estimate risk importation -----------------
# Note :## note a=0..13 need to read probsym and lambda =1:14 

# Effect correction factor(CF) from sero data on case data 
# to get the correct case data (for plotting)

CFCase <- function(df1, df2, origins){
  # inputs: df1 = CF , df2 = Case
  origins <- origins
  # Cases.
  df <- df2
  for (i in 1:length(origins)){
    OR <- origins[i]
    for (j in 1:nrow(df)){
      #df[j, OR] <- (df1[1, OR])*(df2[j, OR]) + df2[j, OR]
      # different correction factors based on the year
      if(format(as.Date(df2$date[j], format="%Y-%m-%d"),"%Y")=="2020"){
      }
      df[j, OR] <- df2[j, OR]*df1[1, OR]
    }
    if(format(as.Date(df2$date[j], format="%Y-%m-%d"),"%Y")=="2021"){
    df[j, OR] <- df2[j, OR]*df1[2, OR]
  }
    }
    return(as.data.frame(df))
}
Case2 <- CFCase(CF, Case, origins)
Case <- Case2 %>%
  mutate(Canada.not.NL = AB+BC+MB+NB+NS+ON+PEI+QC+SK+TR)%>%
  mutate(Int = UK + Spain + Philippines + Norway + India + Guyana + France +US+Other.Int)
#Daily incidence proportion in departure (per date)
Dailyinc <- function(df1, pop, origins){
  # df1 = case2 , df2 = population
  df <- df1
  td <- 11       #delay between the exposed and report date
  origins <- origins
  for (i in 1:length(origins)){
    OR <- origins[i]
    pop2 <- pop$pop[pop$province == OR]
    for (j in 1:nrow(df)){
      td <- 11       #delay between the exposed and report date
      df[j, OR] <- df1[j+td, OR]/pop2}
  }
  return(df)
}

origins <- c("AB", "BC", "MB" ,"NB" ,"NS" , "ON","PEI", "QC","SK", "TR", "France", "Guyana", "India", "Norway", "Philippines", "Spain", "UK", "US", "Other.Int", "Canada.not.NL", "Int")
Dailyinfpre <- Dailyinc(Case2,pop, origins )
Dailyinfpre.unc <- Dailyinc(Case,pop, origins) # uncorrected for seroprevalence
Dailyinfpre  <- Dailyinfpre  %>% filter(Dailyinfpre$date < as.Date("2021-06-01"))
Dailyinfpre.unc  <- Dailyinfpre.unc  %>% filter(Dailyinfpre.unc$date < as.Date("2021-06-01"))

## the rate of infected travelers including pre-test or not (changed value by 
## pre test date January 7, 2021 for international and May 15, 2021 for all)
T.INT <- function(t, a, df1, df2, pr){
  # df1 = Dailyinfpre , df2 = travel volume
  rho <- 0.69 
  psi <- 0.75
  #PCR Test sensitivity - age infection 0-25
  tsens <- c(0, 0.05, 0.1, 0.55, 0.78, 0.77, 0.73, 0.68, 0.64, 0.59, 0.55, 
             0.49, 0.43,0.37, 0.31, 0.25, 0.22, 0.19, 0.16, 0.13, 0.1, 0.09,
             0.08, 0.07, 0.06, 0.05)
  #Probability develop symptom - age infection 0-16
  probsym = dgamma(seq(0,13), shape = 5.807, scale = 0.948) 
  lambda = cumsum(probsym)
  if (t < as.Date("2021-01-07")){
    x <- df2[df2$date == t, pr]*df1[df1$date == t, pr]*rho*psi*lambda[a]}
  else { 
    if (a > 2){x <- df2[df2$date == t, pr]*df1[df1$date == t, pr]*(1-tsens[a-2])}
    else {x <- 0}
      }
  return(x)
}

T.CA <- function(t, a, df1, df2, pr){
  # df1 = Dailyinfpre , df2 = travel volume (r or rw)
  rho <- 0.69 
  psi <- 0.75   
  #PCR Test sensitivity - age infection 0-25
  tsens <- c(0, 0.05, 0.1, 0.55, 0.78, 0.77, 0.73, 0.68, 0.64, 0.59, 0.55, 
             0.49, 0.43,0.37, 0.31, 0.25, 0.22, 0.19, 0.16, 0.13, 0.1, 0.09,
             0.08, 0.07, 0.06, 0.05)
  #Probability develop symptom - age infection 0-16
  probsym = dgamma(seq(0,13), shape = 5.807, scale = 0.948) 
  lambda = cumsum(probsym)
  ## note a=0..13 need to read probsym and lambda =1:14
  if (t < as.Date("2021-05-15")){
    x <- df2[df2$date == t, pr]*df1[df1$date == t, pr]*rho*psi*lambda[a]}
  else { 
    if (a > 2){x <- df2[df2$date == t, pr]*df1[df1$date == t, pr]*(1-tsens[a-2])}
    else {x <- 0}
  }
  return(x)
}

##################### International- regular travelers
R.INT.s = function(Dailyinfpre, rt_Volume, pr){
  df1 <- data.frame(date =seq(from=as.Date("2020-09-01"), to = as.Date("2021-05-31"), by = "days"))
  tr <- 3
  trep <- 1
  abar <- 1
  df1["report"] <- df1$date+ trep + abar + tr
  df1["R"] <- NA
  rho <- 0.69
  psi <- 0.75  
  PS <- 0.8
  #PCR Test sensitivity - age infection 0-25
  tsens <- c(0, 0.05, 0.1, 0.55, 0.78, 0.77, 0.73, 0.68, 0.64, 0.59, 0.55, 
            0.49, 0.43,0.37, 0.31, 0.25, 0.22, 0.19, 0.16, 0.13, 0.1, 0.09,
            0.08, 0.07, 0.06, 0.05)
  #Probability develop symptom - age infection 0-13
  probsym = dgamma(seq(0,13), shape = 5.807, scale = 0.948) 
  lambda = cumsum(probsym)
  for (i in 1:nrow(df1)){
    t <- df1$date[i]
    x <- 0
    for (a in 1:14){
      xa <- T.INT(t, a, Dailyinfpre, rt_Volume, pr)*tsens[a+abar+tr]
      x <- x + xa
      }
    df1$R[i] <- rho*PS*x
    }
  return(df1)
}

# AH: edited to consider all the different countries of origin
# rather than just the US (given reviewer-requested edits)
R.INT1.US <- R.INT.s(Dailyinfpre, rt_Volume, "US") 
R.INT1.FRA <- R.INT.s(Dailyinfpre, rt_Volume, "France")
R.INT1.GUY <- R.INT.s(Dailyinfpre, rt_Volume, "Guyana")
R.INT1.IND <- R.INT.s(Dailyinfpre, rt_Volume, "India")
R.INT1.NOR <- R.INT.s(Dailyinfpre, rt_Volume, "Norway")
R.INT1.PHL <- R.INT.s(Dailyinfpre, rt_Volume, "Philippines")
R.INT1.SPN <- R.INT.s(Dailyinfpre, rt_Volume, "Spain")
R.INT1.UK <- R.INT.s(Dailyinfpre, rt_Volume, "UK")
R.INT1.Other.Int <- R.INT.s(Dailyinfpre, rt_Volume, "Other.Int")

R.INT1<-R.INT1.US
R.INT1$R <- R.INT1.US$R+R.INT1.FRA$R+R.INT1.GUY$R+R.INT1.IND$R+R.INT1.NOR$R+R.INT1.PHL$R+R.INT1.SPN$R+R.INT1.UK$R+R.INT1.Other.Int$R

R.INT.e = function(rt_Volume, pr){
  df1 <- data.frame(date =seq(from=as.Date("2020-09-01"), to = as.Date("2021-05-31"), by = "days"))
  tr <- 3
  trep <- 1
  te <- 3
  df1["report"] <- df1$date + trep +  tr
  df1["E"] <- NA
  PA1 <- 0.01
  PA2 <- 0
  expo = c(as.Date("2020-09-27"), as.Date("2020-10-05"), as.Date("2020-10-23"),
           as.Date("2020-11-04"), as.Date("2020-11-23"), as.Date("2020-11-24"),
           as.Date("2020-12-04"), as.Date("2020-12-07"), as.Date("2020-12-11"),
           as.Date("2020-12-15"), as.Date("2020-12-17"), as.Date("2020-12-29"), 
           as.Date("2021-01-20"), as.Date("2021-02-15"), as.Date("2021-03-04"), 
           as.Date("2021-04-19"), as.Date("2021-05-02"), as.Date("2021-05-06"),
           as.Date("2021-05-11") )
  #PCR Test sensitivity - age infection 0-25
  tsens <- c(0, 0.05, 0.1, 0.55, 0.78, 0.77, 0.73, 0.68, 0.64, 0.59, 0.55, 
             0.49, 0.43,0.37, 0.31, 0.25, 0.22, 0.19, 0.16, 0.13, 0.1, 0.09,
             0.08, 0.07, 0.06, 0.05)
  #Probability develop symptom - age infection 0-13
  for (i in 1:nrow(df1)){
    t <- df1$date[i]
    if ( t  %in% expo) {
      df1$E[i] <- PA1*rt_Volume[rt_Volume$date == t, pr]*tsens[tr+te]
    }
    else {
      df1$E[i] <- PA2*rt_Volume[rt_Volume$date == t, pr]*tsens[tr+te]
    }
  }
  return(df1)
}

# As above revised to consider all international orgins.
R.INT2.US <- R.INT.e(rt_Volume, "US") 
R.INT2.FRA <- R.INT.e(rt_Volume, "France")
R.INT2.GUY <- R.INT.e(rt_Volume, "Guyana")
R.INT2.IND <- R.INT.e(rt_Volume, "India")
R.INT2.NOR <- R.INT.e(rt_Volume, "Norway")
R.INT2.PHL <- R.INT.e(rt_Volume, "Philippines")
R.INT2.SPN <- R.INT.e(rt_Volume, "Spain")
R.INT2.UK <- R.INT.e(rt_Volume, "UK")
R.INT2.Other.Int <- R.INT.e(rt_Volume, "Other.Int")

R.INT2<-R.INT2.US
R.INT2$E <- R.INT2.US$E+R.INT2.FRA$E+R.INT2.GUY$E+R.INT2.IND$E+R.INT2.NOR$E+R.INT2.PHL$E+R.INT2.SPN$E+R.INT2.UK$E+R.INT2.Other.Int$E

####################### Canadian Province -- regular travelers
R.CA.s = function(Dailyinfpre, rt_Volume, pr){
  df1 <- data.frame(date =seq(from=as.Date("2020-09-01"), to = as.Date("2021-05-31"), by = "days"))
  tr <- 3
  trep <- 1
  abar <- 1
  df1["report"] <- df1$date+ trep + abar + tr
  df1["R"] <- NA
  rho <- 0.69
  psi <- 0.75  
  PS <- 0.8
  #PCR Test sensitivity - age infection 0-25
  tsens <- c(0, 0.05, 0.1, 0.55, 0.78, 0.77, 0.73, 0.68, 0.64, 0.59, 0.55, 
             0.49, 0.43,0.37, 0.31, 0.25, 0.22, 0.19, 0.16, 0.13, 0.1, 0.09,
             0.08, 0.07, 0.06, 0.05)
  #Probability develop symptom - age infection 0-13
  probsym = dgamma(seq(0,13), shape = 5.807, scale = 0.948) 
  lambda = cumsum(probsym)
  for (i in 1:nrow(df1)){
    t <- df1$date[i]
    x <- 0
    for (a in 1:14){
      xa <- T.CA(t, a, Dailyinfpre, rt_Volume, pr)*tsens[a+abar+tr]
      x <- x + xa
    }
    df1$R[i] <-  rho*PS*x
  }
  return(df1)
}

# regular travelers is similar INT, so
Province <- function(Dailyinfpre, rt_Volume, origins){
  pro.df <- data.frame(date =seq(from=as.Date("2020-09-01"), to = as.Date("2021-05-31"), by = "days"))
  tr <- 3
  trep <- 1
  abar <- 1
  pro.df["report"] <- pro.df$date+ trep + abar + tr
  for (i in (1:length(origins))){
    pr <- origins[i]
    df <- R.CA.s(Dailyinfpre, rt_Volume, pr)  #dataframe output R.CA.s function
    pro.df[pr] <- df$R
  }
  return(pro.df)
}

origins <- c("ON", "QC","MB","NB","AB","NS","BC","SK","PEI","TR")
R.CA1 <- Province(Dailyinfpre, rt_Volume, origins )  

R.CA.e = function(rt_Volume, pr){
  df1 <- data.frame(date =seq(from=as.Date("2020-09-01"), to = as.Date("2021-05-31"), by = "days"))
  tr <- 3
  trep <- 1
  te <- 3
  df1["report"] <- df1$date + trep +  tr
  df1["E"] <- NA
  psi1 <- 0.01 # prob of being infected on flight
  psi2 <- 0.05 # prop of travelers impacted by exposure notification
  PA2 <- 0
  expo = c(as.Date("2020-09-27"), as.Date("2020-10-05"), as.Date("2020-10-23"),
           as.Date("2020-11-04"), as.Date("2020-11-23"), as.Date("2020-11-24"),
           as.Date("2020-12-04"), as.Date("2020-12-07"), as.Date("2020-12-11"),
           as.Date("2020-12-15"), as.Date("2020-12-17"), as.Date("2020-12-29"), 
           as.Date("2021-01-20"), as.Date("2021-02-15"), as.Date("2021-03-04"), 
           as.Date("2021-04-19"), as.Date("2021-05-02"), as.Date("2021-05-06"),
           as.Date("2021-05-11") )
  #PCR Test sensitivity - age infection 0-25
  tsens <- c(0, 0.05, 0.1, 0.55, 0.78, 0.77, 0.73, 0.68, 0.64, 0.59, 0.55, 
             0.49, 0.43,0.37, 0.31, 0.25, 0.22, 0.19, 0.16, 0.13, 0.1, 0.09,
             0.08, 0.07, 0.06, 0.05)
  
  for (i in 1:nrow(df1)){
    t <- df1$date[i]
    if ( t  %in% expo) {
      df1$E[i] <- psi1*psi2*rt_Volume[rt_Volume$date == t, pr]*tsens[tr+te]
    }
    else {
      df1$E[i] <- PA2*rt_Volume[rt_Volume$date == t, pr]*tsens[tr+te]
    }
  }
  return(df1)
}

Province2 <- function(rt_Volume, origins){
  pro.df <- data.frame(date =seq(from=as.Date("2020-09-01"), to = as.Date("2021-05-31"), by = "days"))
  tr <- 3
  trep <- 1
  te <- 3
  pro.df["report"] <- pro.df$date + trep +  tr
  for (i in (1:length(origins))){
    pr <- origins[i]
    df <- R.CA.e(rt_Volume, pr)  #dataframe output R.CA.e function
    pro.df[pr] <- df$E
  }
  return(pro.df)
}

origins <- c("ON", "QC","MB","NB","AB","NS","BC","SK","PEI","TR")
R.CA2 <- Province2( rt_Volume, origins )  

######################## Canadian Province -- Rotational workers
RW.CA.s = function(j1, j2, j3, Dailyinfpre, rw_Volume, pr){
  df1 <- data.frame(date =seq(from=as.Date("2020-09-01"), to = as.Date("2021-05-31"), by = "days"))
  trep <- 1
  df1["report1"] <- df1$date+ trep + j1
  df1["S1"] <- NA
  df1["report2"] <- df1$date+ trep + j2
  df1["S2"] <- NA
  df1["report3"] <- df1$date+ trep + j3
  df1["S3"] <- NA
  #PCR Test sensitivity - age infection 0-25
  tsens <- c(0, 0.05, 0.1, 0.55, 0.78, 0.77, 0.73, 0.68, 0.64, 0.59, 0.55, 
             0.49, 0.43,0.37, 0.31, 0.25, 0.22, 0.19, 0.16, 0.13, 0.1, 0.09,
             0.08, 0.07, 0.06, 0.05)
  for (i in 1:nrow(df1)){
    t <- df1$date[i]
    x1 <- 0
    x2 <- 0
    x3 <- 0
    for (a in 1:14){
      xa1 <- T.CA(t, a, Dailyinfpre, rw_Volume, pr)*tsens[a+j1]
      x1 <- x1 + xa1
      xa2 <- T.CA(t, a, Dailyinfpre, rw_Volume, pr)*tsens[a+j2]*(1 - tsens[a+j1])
      x2 <- x2 + xa2
      xa3 <- T.CA(t, a, Dailyinfpre, rw_Volume, pr)*tsens[a+j3]*(1 - tsens[a+j1])*(1 - tsens[a+j2])
      x3 <- x3 + xa3
    }
    df1$S1[i] <-  x1
    df1$S2[i] <-  x2
    df1$S3[i] <-  x3
  }
  return(df1)
}

rw.AB <- RW.CA.s(3, 5, 7, Dailyinfpre, rw_Volume, "AB") # it is tested 
rw.ON <- RW.CA.s(3, 5, 7, Dailyinfpre, rw_Volume, "ON")
rw.PEI <- RW.CA.s(3, 5, 7, Dailyinfpre, rw_Volume, "PEI")
rw.QC <- RW.CA.s(3, 5, 7, Dailyinfpre, rw_Volume, "QC")
rw.SK <- RW.CA.s(3, 5, 7, Dailyinfpre, rw_Volume, "SK")
rw.BC <- RW.CA.s(3, 5, 7, Dailyinfpre, rw_Volume, "BC")
rw.MB <- RW.CA.s(3, 5, 7, Dailyinfpre, rw_Volume, "MB")
rw.NB <- RW.CA.s(3, 5, 7, Dailyinfpre, rw_Volume, "NB")
rw.NS <- RW.CA.s(3, 5, 7, Dailyinfpre, rw_Volume, "NS")
rw.TR <- RW.CA.s(3, 5, 7, Dailyinfpre, rw_Volume, "TR") # checked

RW.CA <- function(){
  df <- rw.AB 
  cols <- c("S1", "S2", "S3")
  df[, cols] <- rw.AB[, cols] + rw.ON[, cols] + rw.PEI[, cols] + rw.QC[, cols] +
                rw.SK[, cols] + rw.BC[, cols] + rw.MB[, cols] + rw.NB[, cols] + 
                rw.NS[, cols] + rw.TR[, cols]
  return(df)
}

RW.Pro <- RW.CA()
## merge three times in RW.Pro
RW.Pro2 <- function(){
  df1 <- select(RW.Pro, c(report1,S1)) %>% rename(report = report1)
  df2 <- select(RW.Pro, c(report2, S2)) %>% rename(report = report2)
  df3 <- select(RW.Pro, c(report3, S3)) %>% rename(report = report3)
  df1 <- df1 %>% left_join(df2, by = "report")
  df1 <- df1 %>% left_join(df3, by = "report")
  df1[is.na(df1)] <- 0
  df1["RW"] <- df1$S1 + df1$S2 + df1$S3
  df1 <- select(df1, c(report, RW))
  return(df1)
}

Imp.CA.rw <- RW.Pro2() 

#################aggregate: exemptions (E) and reported travelers (R)
Imp.INT <- R.INT2 %>% left_join(R.INT1, by = "report")
Imp.INT <- select(Imp.INT, -c(date.x, date.y))
Imp.INT["RE.Int"] <- Imp.INT$E + Imp.INT$R
Imp.INT[is.na(Imp.INT)] <- 0

Imp.CA <- function(R.CA1, R.CA2){
  R.CA <- R.CA2 %>% left_join(R.CA1, by ="report")
  R.CA <- select(R.CA , -c(date.x, date.y))
  R.CA["ON"] <- R.CA$ON.x + R.CA$ON.y
  R.CA["QC"] <- R.CA$QC.x + R.CA$QC.y
  R.CA["MB"] <- R.CA$MB.x + R.CA$MB.y
  R.CA["NB"] <- R.CA$NB.x + R.CA$NB.y
  R.CA["AB"] <- R.CA$AB.x + R.CA$AB.y
  R.CA["NS"] <- R.CA$NS.x + R.CA$NS.y
  R.CA["BC"] <- R.CA$BC.x + R.CA$BC.y
  R.CA["SK"] <- R.CA$SK.x + R.CA$SK.y
  R.CA["PEI"] <- R.CA$PEI.x + R.CA$PEI.y
  R.CA["TR"] <- R.CA$TR.x + R.CA$TR.y
  R.CA <- select(R.CA, c("report", "ON", "QC","MB","NB","AB","NS","BC","SK","PEI","TR"))
  return(R.CA)
}

Imp.CA.r <- Imp.CA(R.CA1, R.CA2)  
Imp.CA.r[is.na(Imp.CA.r)] <- 0
Imp.CA.r["RE.Ca"] <- rowSums(Imp.CA.r[, c("ON", "PEI", "QC", "SK", "AB", "BC", "MB", "NB", "NS", "TR")])
# merger regular and rotational workers together and sum
Imp.CA <- Imp.CA.r %>% left_join(Imp.CA.rw, by = "report")
Imp.CA["REW.Ca"] <- rowSums(Imp.CA[, c("RE.Ca","RW")])

## one dataframe for plotting
IMP <- Imp.CA %>% left_join(select(Imp.INT , c(report, RE.Int)) , by = "report")
IMP["Total"] <- rowSums(IMP[, c("REW.Ca","RE.Int")])
## Remove September5, since I just have rw not r 
IMP <- IMP %>% filter(IMP$report >= as.Date("2020-09-06") & IMP$report < as.Date("2021-06-01"))

####################### MODELS (up until now that was all data processing)
############# Models for international
Volume = Volume%>%
  mutate(Int = France+Guyana+India+Norway+Philippines+Spain+UK+US+Other.Int)%>%
  mutate(Canada.not.NL=ON+PEI+QC+SK+AB+BC+MB+NB+NS+TR)
# Extracts reported international cases
df.model6 <- select( Travel_case, c(date, Tot.Int)) %>% left_join(Volume , by ="date")
# AH: Revised to extract all countries not just US
df.model6 <- select(df.model6, c(date, Tot.Int, France, Guyana, India, Norway, Philippines, Spain, UK, US, Other.Int, Int))%>%
  rename(Rep.Int = Tot.Int, v.France =France, v.Guyana = Guyana, v.India =India, v.Norway = Norway, v.Philippines = Philippines, v.Spain = Spain, v.UK = UK, v.US = US, v.Other.Int = Other.Int, v.Int = Int)
df.model6 <- df.model6 %>% left_join(Dailyinfpre, by ="date")
df.model6 <- select(df.model6, c(date, Rep.Int, v.France, v.Guyana, v.India, v.Norway, v.Philippines, v.Spain, v.UK, v.US, v.Other.Int, v.Int, France, Guyana, India, Norway, Philippines, Spain, UK, US, Other.Int,Int))

df.model6["volinf.France"] <- df.model6$v.France*df.model6$France
df.model6["volinf.Guyana"] <- df.model6$v.Guyana*df.model6$Guyana
df.model6["volinf.India"] <- df.model6$v.India*df.model6$India
df.model6["volinf.Norway"] <- df.model6$v.Norway*df.model6$Norway
df.model6["volinf.Philippines"] <- df.model6$v.Philippines*df.model6$Philippines
df.model6["volinf.Spain"] <- df.model6$v.Spain*df.model6$Spain
df.model6["volinf.UK"] <- df.model6$v.UK*df.model6$UK
df.model6["volinf.US"] <- df.model6$v.US*df.model6$US
df.model6["volinf.Other.Int"] <- df.model6$v.Other.Int*df.model6$Other.Int
df.model6["volinf.Int"] <- df.model6$v.Int*df.model6$Int

# Include a lag of 5 days which is consistent with the mechanistic model and
# bar{a} an average of 1 day after arrival for symtomes to develop
# t_r an average of 3 days between requesting the test and it being performed
# t_rep an average of 1 day between testing and reporting.
len = length(df.model6$date)
df.model6.lag = data.frame(df.model6[6:len,1:2],df.model6[1:(len-5),3:length(colnames(df.model6))])

# Model based on travel volume only
# AH: removed 0 intercept based on reviewer 2 comment, also considered neg binomial and lagged exp. variables (July 28, 2025)
# Removed Bangladesh, Suriname, and Thailand from the models as errors were occurring due to some
# coefficients estimating as 0. These 3 countries were selected because in Table B2 for a period
# for these countries travel volume is 0.
Int.TV <- glm(Rep.Int ~   v.France + v.Guyana + v.India + v.Norway + v.Philippines + v.Spain  + v.UK + v.US + v.Other.Int, data = df.model6.lag,family = poisson(link = "log"))
Int.TV.nb <- glm.nb(Rep.Int ~    v.France + v.Guyana + v.India + v.Norway + v.Philippines + v.Spain  + v.UK + v.US + v.Other.Int, data = df.model6.lag)
# Model based on the product of infection prevalence only (does not have other int because assumed to be the same as US)
Int.Inf <- glm(Rep.Int ~    France + Guyana + India + Norway + Philippines + Spain  + UK + US, data = df.model6.lag, family = poisson(link = "log"))
Int.Inf.nb <- glm.nb(Rep.Int ~   France + Guyana + India + Norway + Philippines + Spain  + UK + US, data = df.model6.lag)
##both travel volume and infection
#Int.TVInf <- glm(Rep.Int ~ volinf.Bangladesh + volinf.France + volinf.Guyana + volinf.India + volinf.Norway + volinf.Philippines + volinf.Spain + volinf.Suriname + volinf.Thailand + volinf.UK + volinf.US + volinf.Other.Int , data = df.model6.lag, family = poisson(link="log"))
#Int.TVInf.nb <- glm.nb(Rep.Int ~ volinf.Bangladesh + volinf.France + volinf.Guyana + volinf.India + volinf.Norway + volinf.Philippines + volinf.Spain + volinf.Suriname + volinf.Thailand + volinf.UK + volinf.US + volinf.Other.Int , data = df.model6.lag)
# October 1, 2025: the above models are changed to be interaction models
Int.TVInf <- glm(Rep.Int ~   v.France*France + v.Guyana*Guyana + v.India*India + v.Norway*Norway + v.Philippines*Philippines + v.Spain*Spain + v.UK*UK + v.US*US + v.Other.Int*Other.Int, data = df.model6.lag,family = poisson(link = "log"))
Int.TVInf.nb <- glm.nb(Rep.Int ~ + v.France*France + v.Guyana*Guyana + v.India*India + v.Norway*Norway + v.Philippines*Philippines + v.Spain*Spain + v.UK*UK + v.US*US+v.Other.Int*Other.Int, data = df.model6.lag)

#aggregated international data
Int.TV.agg <- glm(Rep.Int ~  v.Int, data = df.model6.lag,family = poisson(link = "log"))
Int.TV.agg.nb <- glm.nb(Rep.Int ~  v.Int, data = df.model6.lag)
Int.Inf.agg <- glm(Rep.Int ~ Int, data = df.model6.lag, family = poisson(link = "log"))
Int.Inf.agg.nb <- glm.nb(Rep.Int ~ Int, data = df.model6.lag)
#Int.TVInf.agg <- glm(Rep.Int ~ volinf.Int, data = df.model6.lag, family = poisson(link="log"))
#Int.TVInf.agg.nb <- glm.nb(Rep.Int ~ volinf.Int, data = df.model6.lag)
# Redone as interaction (October 1, 2025)
Int.TVInf.agg <- glm(Rep.Int ~  v.Int*Int, data = df.model6.lag,family = poisson(link = "log"))
Int.TVInf.agg.nb <- glm.nb(Rep.Int ~  v.Int*Int, data = df.model6.lag,control = glm.control(maxit =1000))
Int.const.nb <- glm.nb(Rep.Int ~ 1, data = df.model6.lag)
Int.const<- glm(Rep.Int ~ 1, data = df.model6.lag, family = poisson(link="log"))

## AIC Table for International models at the country level (Poisson)
mod <- list(Int.TV, Int.Inf, Int.TVInf, Int.TV.agg, Int.Inf.agg, Int.TVInf.agg, Int.const)
mod.name <-  c("TV", "Inf", "InfxTV", "TV-agg", "Inf-agg", "TVxInf-agg", "const")
AIC.INT = aictab(cand.set = mod, modnames = mod.name ) 
# Only 11 coefficients for infection prevalence because Other.Int is identical to US.
AIC.INT = data.frame(AIC.INT)

## AIC Table for International models at the country level (negative binomial)
mod <- list(Int.TV.nb, Int.Inf.nb, Int.TVInf.nb, Int.TV.agg.nb, Int.Inf.agg.nb, Int.TVInf.agg.nb, Int.const.nb)
mod.name <-  c("TV-nb", "Inf-nb", "InfxTV-nb", "TV-agg-nb", "Inf-agg-nb", "TVxInf-agg-nb", "const-nb")
AIC.INT.nb = aictab(cand.set = mod, modnames = mod.name ) 
# Only 11 coefficients for infection prevalence because Other.Int is identical to US.
AIC.INT.nb = data.frame(AIC.INT.nb)


######### Models for Domestic
df.model7 <- select(Travel_case, c(date, Tot.Dom)) %>% left_join(Volume , by ="date") 
df.model7 <- select(df.model7, -c(US, France, Guyana, India, Norway, Philippines, Spain, UK, Other.Int, Int)) %>% rename(Rep.Dom = Tot.Dom)
# lag five days, i.e., similar to international.
len = length(df.model7$date)
df.model7.lag = data.frame(df.model7[6:len,1:2],df.model7[1:(len-5),3:length(colnames(df.model7))])

CA.TV <- glm(Rep.Dom ~ AB + BC + MB + NB + NS + ON + PEI + QC + SK + TR, data = df.model7.lag, family=poisson(link = "log"))
CA.TV.nb <- glm.nb(Rep.Dom ~ AB + BC + MB + NB + NS + ON + PEI + QC + SK + TR, data = df.model7.lag)
CA.TV.agg=glm(Rep.Dom ~ Canada.not.NL, data= df.model7.lag, family = poisson(link= "log"))
CA.TV.agg.nb=glm.nb(Rep.Dom ~  Canada.not.NL, data= df.model7.lag)

df.model8 <- select(Travel_case, c(date, Tot.Dom)) %>% left_join( Dailyinfpre, by ="date") 
df.model8 <- select(df.model8, -c(US, France, Guyana, India, Norway, Philippines, Spain, UK, Other.Int, Int)) %>% rename(Rep.Dom = Tot.Dom)
# lag five days, i.e., similar to international.
len = length(df.model8$date)
df.model8.lag = data.frame(df.model8[6:len,1:2],df.model8[1:(len-5),3:length(colnames(df.model8))])

# The below is incorrect. Fixed in the revision by AH on July 4, 2025.
#df.model8["inf.CA"] <- rowSums(df.model8[, c("ON", "PEI", "QC", "SK", "AB", "BC", "MB", "NB", "NS", "TR")])
#infection provinces
#InfPr <- lm(Rep.Dom ~  0 + AB + BC + MB + NB + NS + ON + PEI + QC + SK + TR, data = df.model8)
#summary(InfPr)

CA.Inf <- glm(Rep.Dom ~  AB + BC + MB + NB + NS + ON + PEI + QC + SK + TR, data = df.model8.lag, family = poisson(link = "log"))
CA.Inf.nb <- glm.nb(Rep.Dom ~  AB + BC + MB + NB + NS + ON + PEI + QC + SK + TR, data = df.model8.lag)
CA.Inf.agg <- glm(Rep.Dom  ~ Canada.not.NL , data = df.model8.lag, family = poisson(link = "log"))
CA.Inf.agg.nb <- glm.nb(Rep.Dom  ~ Canada.not.NL , data = df.model8.lag)


##both travel volume*infection
multivolinf <- function(df1, df2, origins){
  # inputs: df1 = volume , df2 = infe
  origins <- origins
  df <- df1
  for (i in 1:length(origins)){
    OR <- origins[i]
    for (j in 1:nrow(df)){
      df[j, OR] <- df1[j, OR]*df2[j, OR]
    }
  }
  return(as.data.frame(df))
}
origins <- c("AB", "BC","NB", "MB","NS","ON", "PEI","QC", "SK", "TR", "Canada.not.NL")
df.model9 <- multivolinf(df.model7, df.model8, origins)
len = length(df.model9$date)
df.model9.lag = data.frame(df.model9[6:len,1:2],df.model9[1:(len-5),3:length(colnames(df.model9))])

## Doing this as an interaction model...
#CA.TVInf=glm(Rep.Dom  ~ AB + BC + MB + NB + NS + ON + PEI + QC + SK + TR ,  data = df.model9.lag,family = poisson(link = "log"))
#CA.TVInf.nb=glm.nb(Rep.Dom  ~ AB + BC + MB + NB + NS + ON + PEI + QC + SK + TR ,  data = df.model9.lag)
#aggregated
#CA.TVInf.agg <- glm(Rep.Dom  ~ Canada.not.NL , data = df.model9.lag, family = poisson(link = "log"))
#CA.TVInf.agg.nb <- glm.nb(Rep.Dom  ~ Canada.not.NL , data = df.model9.lag)
# The above is commented out, this was written by Zahra. Note that these analyses for domestic travel-related
# cases are all using different dataframes: df.model7.lag (travel volume), df.model8.lag (infections), and df.model9.lag (multiplication).
# I'm going to have to fix this by renaming the travel volume variables and appending them to the infection prevlance
# dataframe (df.model8.lag)
df.model7.lag<-df.model7.lag%>%rename(vAB=AB, vBC=BC, vMB = MB, vNB=NB, vNS = NS, vON = ON, vPEI=PEI, vQC=QC, vSK=SK, vTR=TR, vCanada.not.NL = Canada.not.NL)
df.model8.lag2<-df.model8.lag%>%select(-date,-Rep.Dom)
df.model7.lag<-data.frame(df.model7.lag, df.model8.lag2)
CA.TVInf=glm(Rep.Dom  ~ AB*vAB + BC*vBC + MB*vMB + NB*vNB + NS*vNS + ON*vON + PEI*vPEI + QC*vQC + SK*vSK + TR*vTR ,  data = df.model7.lag,family = poisson(link = "log"))
CA.TVInf.nb=glm.nb(Rep.Dom  ~ AB*vAB + BC*vBC + MB*vMB + NB*vNB + NS*vNS + ON*vON + PEI*vPEI + QC*vQC + SK*vSK + TR*vTR,  data = df.model7.lag,control = glm.control(maxit = 10000))
#aggregated
CA.TVInf.agg <- glm(Rep.Dom  ~ Canada.not.NL*vCanada.not.NL, data = df.model7.lag, family = poisson(link = "log"))
CA.TVInf.agg.nb <- glm.nb(Rep.Dom  ~ Canada.not.NL*vCanada.not.NL, data = df.model7.lag)

CA.const <- glm(Rep.Dom  ~ 1, data = df.model7.lag, family = poisson(link = "log"))
CA.const.nb <- glm.nb(Rep.Dom  ~ 1, data = df.model7.lag)


models <- list(CA.TV, CA.TV.agg, CA.Inf, CA.Inf.agg, CA.TVInf, CA.TVInf.agg, CA.const)
model.names <-  c("TV", "TV-agg", "Inf", 
                  "Inf-agg", "TVxInf",
                  "TVxInf-agg", "const")
AIC.CA = aictab(cand.set = models, modnames = model.names, c.hat = 1)
AIC.CA = data.frame(AIC.CA)

models <- list(CA.TV.nb, CA.TV.agg.nb, CA.Inf.nb, CA.Inf.agg.nb, CA.TVInf.nb, CA.TVInf.agg.nb, CA.const.nb)
model.names <-  c("TV-nb", "TV-agg-nb", "Inf-nb", 
                  "Inf-agg-nb", "TVxInf-nb",
                  "TVxInf-agg-nb", "const-nb")
AIC.CA.nb = aictab(cand.set = models, modnames = model.names, c.hat = 1)
AIC.CA.nb = data.frame(AIC.CA.nb)

################ model for epidemiology model output
df.modelfull <- select(IMP, c("report", "REW.Ca", "RE.Int")) %>% rename(date = report)
df.modelfull <- df.modelfull %>% left_join(select(Travel_case, c(date, Tot.Dom, Tot.Int)), by = "date") 
df.modelfull <- df.modelfull %>% rename(Est.Dom = REW.Ca, Est.Int = RE.Int ,
                                        Rep.Dom = Tot.Dom, Rep.Int = Tot.Int)

# No fitted parameters, calculated loglikelihood for mechanistic model
LL.CA=sum(dpois(df.modelfull$Rep.Dom, df.modelfull$Est.Dom, log = TRUE))
LL.INT=sum(dpois(df.modelfull$Rep.Int, df.modelfull$Est.Int, log = TRUE))

mech.AICc.CA = -2*LL.CA
mech.AICc.INT = -2*LL.INT

mech.mod.CA = c(Modnames = "mechanistic", K=0, AICc = mech.AICc.CA, Delta_AICc = mech.AICc.CA- as.numeric(AIC.CA$AICc[1]), ModelLik="", AICcWt = "", LL = LL.CA, Cum.Wt = "")
mech.mod.INT = c(Modnames = "mechanistic", K=0, AICc = mech.AICc.INT, Delta_AICc = mech.AICc.INT- as.numeric(AIC.INT$AICc[1]), ModelLik="", AICcWt = "", LL = LL.INT, Cum.Wt = "")

AIC.CA=rbind(AIC.CA, mech.mod.CA)
AIC.INT=rbind(AIC.INT, mech.mod.INT)

######### Results tables for the main models
AIC.CA = AIC.CA%>%select(Modnames, K, LL, AICc)
AIC.INT = AIC.INT%>%select(Modnames, K, LL, AICc)
AIC.CA.nb = AIC.CA.nb%>%select(Modnames, K, LL, AICc)
AIC.INT.nb = AIC.INT.nb%>%select(Modnames, K, LL, AICc)

LL.CA.const = as.numeric(AIC.CA$LL[which(AIC.CA$Modnames=="const")])
LL.CA.const.nb = as.numeric(AIC.CA.nb$LL[which(AIC.CA.nb$Modnames=="const-nb")])
LL.INT.const = as.numeric(AIC.INT$LL[which(AIC.INT$Modnames=="const")])
LL.INT.const.nb = as.numeric(AIC.INT.nb$LL[which(AIC.INT.nb$Modnames=="const-nb")])
AIC.CA = AIC.CA%>%mutate(LR = -2*(LL.CA.const - as.numeric(LL)))
AIC.INT = AIC.INT%>%mutate(LR = -2*(LL.INT.const - as.numeric(LL)))
AIC.CA.nb = AIC.CA.nb%>%mutate(LR = -2*(LL.CA.const.nb - as.numeric(LL)))
AIC.INT.nb = AIC.INT.nb%>%mutate(LR = -2*(LL.INT.const.nb - as.numeric(LL)))
AIC.CA = AIC.CA[order(AIC.CA$LL),]
AIC.INT = AIC.INT[order(AIC.INT$LL),]
AIC.CA.nb = data.frame(AIC.CA.nb)
AIC.CA.nb = AIC.CA.nb[order(AIC.CA.nb$LL,decreasing=TRUE),]
AIC.INT.nb = AIC.INT.nb[order(AIC.INT.nb$LL,decreasing=TRUE),]


##### ----------------------Models that represent other data gaps.
#####---------- Suppose only one type of travel data is available. Needs to
#####---------- consider only aggregated data for Canada and International because
#####---------- some data sources are only by month with limits the number of
#####--------- parameters that can be fit.
##### IATA and FC data are only by month.
Rep  <- select(Travel_case, c(date, Tot.Int,Tot.Dom )) 
Rep$year  <- strftime(Rep$date, "%Y")   
Rep$month <- strftime(Rep$date, "%m")
Rep.month <- Rep %>%
  group_by( year, month) %>%
  dplyr::summarize( RInt= sum(Tot.Int),
                    RCA = sum(Tot.Dom)
  ) %>%
  as.data.frame()

## Total travel volume data (edits on July 4 by AH because need to consider
# all international travelers not just US).
total.volume <- read.csv("Data/TraVolData/TotalTraVolume.csv")
total.volume$date <- as.Date(as.character(Volume$date), format = "%Y-%m-%d")
total.volume$year  <- strftime(total.volume$date, "%Y")   
total.volume$month <- strftime(total.volume$date, "%m")
total.volume.month <- total.volume %>%
  group_by( year, month) %>%
  dplyr::summarize( INT= sum(INT),
                    BC = sum(BC),
                    AB = sum(AB),
                    SK = sum(SK),
                    MB = sum(MB),
                    ON = sum(ON),
                    QC = sum(QC),
                    NB = sum(NB),
                    NS = sum(NS),
                    PEI = sum(PEI),
                    TR = sum(TR)
                    
  ) %>%
  as.data.frame()

total.volume.month <- mutate(total.volume.month,CAnotNL = AB+BC+SK+MB+ON+QC+NB+NS+PEI+TR)

### IATA data
IATA.Volume <- read.csv("Data/TraVolData/InboundIATA.csv")
IATA.Volume$date <- as.Date(as.character(IATA.Volume$date), format = "%Y-%m-%d")
IATA.Volume$year  <- strftime(IATA.Volume$date, "%Y")   
IATA.Volume$month <- strftime(IATA.Volume$date, "%m")
IATA.Volume %>% mutate(INT=nonUS+US)
# Remove 2020 and use 2019 only for calculations
IATA.Volume <- head(IATA.Volume,-3)
IATA.Volume <-inner_join(Rep.month, IATA.Volume, by="month")

### FC data
FC.Volume <- read.csv("Data/TraVolData/FC-NL-nocrews.csv")
fc = filter(FC.Volume,REF_DATE>as.Date("2020-08-01") & REF_DATE<as.Date("2021-06-01"))

# Canada seroprevalence corrected - monthly
inf.CA.month = select(df.model8, date,Canada.not.NL)
inf.CA.month$date <- as.Date(as.character(inf.CA.month$date), format = "%Y-%m-%d")
inf.CA.month$year  <- strftime(inf.CA.month$date, "%Y")   
inf.CA.month$month <- strftime(inf.CA.month$date, "%m")
inf.CA.month <- inf.CA.month %>%
  group_by( year, month) %>%
  dplyr::summarize( inf.CA= sum(Canada.not.NL)
  ) %>%
  as.data.frame()

# International seroprevalence corrected - monthly
inf.Int.month = select(df.model6, date,volinf.Int,Int)
inf.Int.month$date <- as.Date(as.character(inf.Int.month$date), format = "%Y-%m-%d")
inf.Int.month$year  <- strftime(inf.Int.month$date, "%Y")   
inf.Int.month$month <- strftime(inf.Int.month$date, "%m")
inf.Int.month <- inf.Int.month %>%
  group_by( year, month) %>%
  dplyr::summarize(inf.Int= sum(Int)
  ) %>%
  as.data.frame()

df.model10 = as.data.frame(inner_join(inf.CA.month,Rep.month,by="month"))
df.model10 = data.frame(df.model10,tv.CA=total.volume.month$CAnotNL, tv.INT = total.volume.month$INT,fc = fc$total, iata.CA = IATA.Volume$ALLCANotNL, iata.INT = IATA.Volume$US +IATA.Volume$nonUS)
df.model10 = mutate(df.model10, tvprod.CA = tv.CA*inf.CA, iataprod.CA =iata.CA*inf.CA)
df.model10 = select(df.model10,month,inf.CA,RInt,RCA,tv.CA,iata.CA,tvprod.CA,iataprod.CA, tv.INT, iata.INT,fc)
df.model10 = mutate(df.model10, inf.INT = inf.Int.month$inf.Int)
df.model10 = mutate(df.model10, iataprod.INT = iata.INT*inf.INT, fcprod = fc*inf.INT, tvprod.INT= tv.INT*inf.INT)

const.CA = glm.nb(RCA~1,data=df.model10)
total.tv.CA = glm.nb(RCA~tv.CA,data=df.model10)
iata.tv.CA = glm.nb(RCA~iata.CA,data=df.model10)
total.prod.CA = glm.nb(RCA~tvprod.CA,data=df.model10)
iata.prod.CA = glm.nb(RCA~iataprod.CA,data=df.model10)
const.INT = glm.nb(RInt~1,data=df.model10)
total.tv.INT = glm.nb(RInt~tv.INT,data=df.model10)
iata.tv.INT = glm.nb(RInt~iata.INT,data=df.model10)
fc.tv.INT = glm.nb(RInt~fc,data=df.model10)
total.prod.INT = glm.nb(RInt~tvprod.INT,data=df.model10)
iata.prod.INT = glm.nb(RInt~iataprod.INT,data=df.model10)
fc.prod.INT = glm.nb(RInt~fcprod,data=df.model10)


models <- list(total.tv.CA, iata.tv.CA, total.prod.CA, iata.prod.CA, const.CA)
model.names <-  c("Total volume (CA)", "IATA (CA)", "Total volume-prod (CA)", "IATA-prod (CA)", "const (CA)")
AIC.tv.CA = aictab(cand.set = models, modnames = model.names, c.hat = 1)
LL.const.CA = logLik(const.CA)[1]
AIC.tv.CA = data.frame(AIC.tv.CA)
AIC.tv.CA = AIC.tv.CA%>%mutate(LR = -2*(LL.const.CA - as.numeric(LL)))

models <- list(total.tv.INT, iata.tv.INT, fc.tv.INT, total.prod.INT, iata.prod.INT, fc.prod.INT, const.INT)
model.names <-  c("Total volume (INT)", "IATA (INT)", "FC (INT)","Total volume-prod (INT)", "IATA-prod (INT)", "FC-prod", "constant (INT)")
LL.const.INT = logLik(const.INT)[1]
AIC.tv.INT = aictab(cand.set = models, modnames = model.names, c.hat = 1)
AIC.tv.INT = data.frame(AIC.tv.INT)
AIC.tv.INT = AIC.tv.INT%>%mutate(LR = -2*(LL.const.INT - as.numeric(LL)))

AIC.tv.INT = AIC.tv.INT[order(AIC.tv.INT$LL,decreasing=TRUE),]%>%select(Modnames,LL,LR,K,AICc)
AIC.tv.CA = AIC.tv.CA[order(AIC.tv.CA$LL,decreasing=TRUE),]%>%select(Modnames,LL,LR,K,AICc)

##--------------- Infection prevalence
## Infection proportion without any corrections for seroprevalence:
inf.unc <- Dailyinc(Case,pop, origins )%>% filter(date<as.Date("2021-06-01"))%>%as.data.frame()%>%
  mutate(Rep.Dom = df.model7$Rep.Dom, Rep.Int = df.model6$Rep.Int)

len = length(inf.unc$date)
inf.unc.lag = data.frame(inf.unc[6:len,1:2],inf.unc[1:(len-5),3:length(colnames(inf.unc))])
vol.CA<-select(df.model7.lag, vAB, vBC, vSK, vMB, vON, vQC, vNB, vNS, vPEI, vTR)
inf.unc.lag<-data.frame(inf.unc.lag, vol.CA)

unc.inf = glm.nb(Rep.Dom~AB+BC+SK+MB+ON+QC+NB+NS+PEI+TR, data=inf.unc.lag)
unc.prod = glm.nb(Rep.Dom~AB*vAB+BC*vBC+SK*vSK+MB*vMB+ON*vON+QC*vQC+NB*vNB+NS*vNS+PEI*vPEI+TR*vTR,data=inf.unc.lag)

models <- list(CA.Inf.nb, CA.TVInf.nb,unc.inf, unc.prod, CA.const.nb)
model.names <-  c("Inf", "InfxTV", "Inf-unc", "InfxTV-unc", "const")
AIC.inf.unc = aictab(cand.set = models, modnames = model.names, c.hat = 1)
AIC.inf.unc = data.frame(AIC.inf.unc)
LL.const = logLik(CA.const.nb)[1]
AIC.inf.unc.CA = AIC.inf.unc%>%mutate(LR = -2*(LL.const - as.numeric(LL)))

vol.INT<-select(df.model6.lag, v.US, v.France, v.Guyana, v.India, v.Norway, v.Philippines, v.Spain,  v.UK, v.Other.Int)
inf.unc.lag<-data.frame(inf.unc.lag, vol.INT)

unc.inf = glm.nb(Rep.Int~US+France+Guyana+India+Norway+Philippines+Spain+UK+Other.Int, data=inf.unc.lag)
unc.prod = glm.nb(Rep.Int~US*v.US+France*v.France+Guyana*v.Guyana+India*v.India+Norway*v.Norway+Philippines*v.Philippines+Spain*v.Spain+UK*v.UK+Other.Int*v.Other.Int,data=inf.unc.lag)


models <- list(Int.Inf.nb, Int.TVInf.nb,unc.inf, unc.prod, Int.const.nb)
model.names <-  c("Inf", "InfxTV", "Inf-unc", "InfxTV-unc", "const")
AIC.inf.unc = aictab(cand.set = models, modnames = model.names, c.hat = 1)
AIC.inf.unc = data.frame(AIC.inf.unc)
LL.const = logLik(Int.const.nb)[1]
AIC.inf.unc = AIC.inf.unc%>%mutate(LR = -2*(LL.const - as.numeric(LL)))
AIC.inf.unc.INT = AIC.inf.unc%>%mutate(LR = -2*(LL.const - as.numeric(LL)))

AIC.inf.unc.INT = AIC.inf.unc.INT[order(AIC.inf.unc.INT$LL,decreasing=TRUE),]%>%select(Modnames,LL,LR,K,AICc)
AIC.inf.unc.CA = AIC.inf.unc.CA[order(AIC.inf.unc.CA$LL,decreasing=TRUE),]%>%select(Modnames,LL,LR,K,AICc)

######## RESULTS
print(AIC.CA) # models have > nLL than the negative binomial ones
print(AIC.INT)
print(AIC.CA.nb)
print(AIC.INT.nb)
print(AIC.tv.CA) # monthly reporting of FC and IATA makes these data not very good
print(AIC.tv.INT)
print(AIC.inf.unc.CA) # small differences in LR when using corrected versus uncorrected data
print(AIC.inf.unc.INT)
