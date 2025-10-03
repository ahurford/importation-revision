# This file is in response to a reviewer request for versions of the
# epidemiological model with different parameter values. The code written
# by Zahra in Winter 2022 (now named modelling.R) has statistical modelling
# and mechanistic mathematical modelling interspersed (as does the plotting
# file plots.R so here I will try to separate these out
# - Amy Hurford (October 2025)

##---------- 1. Load libraries ------------
source("preliminaries.R")

##---------- 2. Read data ------------
# COVID cases in origins (CA excluded NL and US would consider as International)
source("getcases.pro.R") # output: Case and pop

#Seroprevalence Data and Correction factor calculated in 
source("getsero-CF.R") # output: CF
# This is a scenario - suppose there are actually 2x more infections than estimated.
# we achieved this scenario by multiplying all the correction factors by 2.
CF<-10*CF[1,2:23]

# daily travel volume calculated in Volume-focusOrigin.R
# This is the total travel volume including corrections for exclusions
Volume <- read.csv("Data/TraVolData/TotalTraVolume.csv")
Volume$date <- as.Date(as.character(Volume$date), format = "%Y-%m-%d")

# This does not have the exemptions
Volume.Int <- read.csv("Data/TraVolData/TravelVolumeNLCHIfiltered.csv")%>%
  select(AllFR, Bangladesh, France, Guyana, India, Norway, Philippines, Spain, Suriname, Thailand, UK, USA)%>%
  mutate(Other.Int = AllFR-Bangladesh-France-Guyana-India-Norway-Philippines-Spain-Suriname-Thailand-UK-USA)%>%
  rename(US=USA)

Volume.Int.prop = Volume.Int/Volume.Int$AllFR
Volume.Int.prop$AllFR[is.na(Volume.Int.prop$AllFR)] = 1
Volume.Int.prop[is.na(Volume.Int.prop)] = 0
Volume.Int.with.exemptions = Volume.Int.prop*Volume$INT

Volume.Int.with.exemptions%>%select(Bangladesh, France, Guyana, India, Norway, Philippines, Spain, Suriname, Thailand, UK, US, Other.Int)

# This is the travel volume for all the provinces, territories and countries
# with correction for exempted travelers.
Volume = data.frame(Volume, Volume.Int.with.exemptions)%>%
  select(-AllFR, -INT)

#break down daily volume to regular travel vs rotational workers (200 daily-rw)
rw_Volume <- Volume %>% mutate(AB = 114, ON= 30, BC= 8 , SK = 4, MB = 2, QC = 4, 
                               NB = 8, NS = 16, PEI = 4, TR = 10 , Bangladesh=0, France = 0, Guyana = 0, India = 0, Norway = 0, Philippines=0, Spain = 0, Suriname=0, Thailand=0, UK=0, US=0, Other.Int=0)
#min(Volume$..) AB, Sk, PEI , NB and TR are less than this values needs to fix 
fixor <- c("AB","PEI", "SK", "TR", "NB")
for (i in 1:length(fixor)){
  pr <- fixor[i]
  for (j in 1:nrow(rw_Volume)){
    if (rw_Volume[j, pr] > Volume[j, pr] ){rw_Volume[j,pr] <- Volume[j, pr]}
  }
}

# Regular travelers
origins <- c("AB", "BC", "MB" ,"NB" ,"NS" , "ON","PEI", "QC","SK", "TR", "Bangladesh", "France", "Guyana", "India", "Norway", "Philippines", "Spain", "Suriname", "Thailand", "UK", "US", "Other.Int")
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
  mutate(Int = UK + Thailand + Suriname + Spain + Philippines + Norway + India + Guyana + France + Bangladesh+US+Other.Int)
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

origins <- c("AB", "BC", "MB" ,"NB" ,"NS" , "ON","PEI", "QC","SK", "TR", "Bangladesh", "France", "Guyana", "India", "Norway", "Philippines", "Spain", "Suriname", "Thailand", "UK", "US", "Other.Int", "Canada.not.NL", "Int")
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
R.INT1.BAN <- R.INT.s(Dailyinfpre, rt_Volume, "Bangladesh") 
R.INT1.FRA <- R.INT.s(Dailyinfpre, rt_Volume, "France")
R.INT1.GUY <- R.INT.s(Dailyinfpre, rt_Volume, "Guyana")
R.INT1.IND <- R.INT.s(Dailyinfpre, rt_Volume, "India")
R.INT1.NOR <- R.INT.s(Dailyinfpre, rt_Volume, "Norway")
R.INT1.PHL <- R.INT.s(Dailyinfpre, rt_Volume, "Philippines")
R.INT1.SPN <- R.INT.s(Dailyinfpre, rt_Volume, "Spain")
R.INT1.SUR <- R.INT.s(Dailyinfpre, rt_Volume, "Suriname")
R.INT1.THA <- R.INT.s(Dailyinfpre, rt_Volume, "Thailand")
R.INT1.UK <- R.INT.s(Dailyinfpre, rt_Volume, "UK")
R.INT1.Other.Int <- R.INT.s(Dailyinfpre, rt_Volume, "Other.Int")

R.INT1<-R.INT1.US
R.INT1$R <- R.INT1.US$R+R.INT1.BAN$R+R.INT1.FRA$R+R.INT1.GUY$R+R.INT1.IND$R+R.INT1.NOR$R+R.INT1.PHL$R+R.INT1.SPN$R+R.INT1.SUR$R+R.INT1.THA$R+R.INT1.UK$R+R.INT1.Other.Int$R

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
R.INT2.BAN <- R.INT.e(rt_Volume, "Bangladesh") 
R.INT2.FRA <- R.INT.e(rt_Volume, "France")
R.INT2.GUY <- R.INT.e(rt_Volume, "Guyana")
R.INT2.IND <- R.INT.e(rt_Volume, "India")
R.INT2.NOR <- R.INT.e(rt_Volume, "Norway")
R.INT2.PHL <- R.INT.e(rt_Volume, "Philippines")
R.INT2.SPN <- R.INT.e(rt_Volume, "Spain")
R.INT2.SUR <- R.INT.e(rt_Volume, "Suriname")
R.INT2.THA <- R.INT.e(rt_Volume, "Thailand")
R.INT2.UK <- R.INT.e(rt_Volume, "UK")
R.INT2.Other.Int <- R.INT.e(rt_Volume, "Other.Int")

R.INT2<-R.INT2.US
R.INT2$E <- R.INT2.US$E+R.INT2.BAN$E+R.INT2.FRA$E+R.INT2.GUY$E+R.INT2.IND$E+R.INT2.NOR$E+R.INT2.PHL$E+R.INT2.SPN$E+R.INT2.SUR$E+R.INT2.THA$E+R.INT2.UK$E+R.INT2.Other.Int$E

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
  phi1 <- 0.01 # prob of being infected on flight
  phi2 <- 0.05 # prop of travelers impacted by exposure notification
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
      df1$E[i] <- phi1*phi2*rt_Volume[rt_Volume$date == t, pr]*tsens[tr+te]
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

################ model for epidemiology model output
df.modelfull <- select(IMP, c("report", "REW.Ca", "RE.Int")) %>% rename(date = report)
df.modelfull <- df.modelfull %>% left_join(select(Travel_case, c(date, Tot.Dom, Tot.Int)), by = "date") 
df.modelfull <- df.modelfull %>% rename(Est.Dom = REW.Ca, Est.Int = RE.Int ,Rep.Dom = Tot.Dom, Rep.Int = Tot.Int)

dfplt <- df.modelfull
# For the mechanistic model below these are based on Poisson measurement error
dfplt["Est.Dom.low"] =qpois(0.025, dfplt$Est.Dom)
dfplt["Est.Dom.high"] =qpois(0.975, dfplt$Est.Dom)
dfplt["Est.Int.low"] =qpois(0.025, dfplt$Est.Int)
dfplt["Est.Int.high"] =qpois(0.975, dfplt$Est.Int)
df1.Int <- select(dfplt, c(date, Est.Int, Est.Int.low, Est.Int.high))

p1mech <- ggplot() + 
  geom_ribbon(data=df1.Int, aes(x=date, ymin=Est.Int.low, ymax=Est.Int.high), fill=cpalete[1],alpha = 0.3)+
  geom_line(data=df1.Int, aes(x=date, y=Est.Int),color=cpalete[1], size = 1.5) +
  geom_point(data=dfplt, aes(x=date, y= Rep.Int),color='black', size=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16))+
  labs(x="", y="\n Travel-related cases (daily)",
       title="International") +
  ylim(c(0,15))+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))


df1.Dom <- select(dfplt, c(date, Est.Dom, Est.Dom.low, Est.Dom.high))

p2mech <- ggplot() + 
  geom_ribbon(data=df1.Dom, aes(x=date, ymin=Est.Dom.low, ymax= Est.Dom.high), fill=cpalete[2],alpha = 0.3)+
  geom_line(data=df1.Dom, aes(x=date, y=Est.Dom),color=cpalete[2], size=1.5) +
  geom_point(data=dfplt, aes(x=date, y= Rep.Dom),color='black', size=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16))+
  labs(x="", y="\n Travel-related cases (daily)",
       title="Canada") +
  ylim(c(0,15))+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))

p2mech/p1mech + plot_annotation(tag_levels = 'A')
ggsave("Figure/alt_mech_models.png", width = 10, , height = 10, dpi =500)
