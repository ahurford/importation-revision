## Winter 2022
## Project: Importation Risk
## Zahra Mohammadi
## Revised by Amy Hurford on June 19, 2025. Consulting the manuscript there is an error as the
## unreported cases per reported case, u_i, is less than 1, which does not make sense.
## However, in considering the international locations of origin the seroprevalence
## estimates are for each half year, so we will complete a revision that also reflects
## this.

setwd("~/Desktop/Work/Research/Research_Projects/2025/importation-revision/")
source("preliminaries.R")

#################### 1.get data
# Revision: June 24, 2025 by AH. For consistency with the other international origin
# countries, the IHME data source will be used for USA too. Therfore, this CDC source
# is commented out.
## USA from (https://covid.cdc.gov/covid-data-tracker/#nationwide-blood-donor-seroprevalence)
#seUS = rbind(data.frame(date = c(seq(as.Date("2020-07-01"), as.Date("2021-06-01"), by = "month"))
#  ,
#  US = c(3.5, 4.7, 4.9, 5.9, 8.2, 11.9, 15.9, 18.4, 19.8, 20.2, 20.6, 20.7)
#))


## Canada from (https://www.covid19immunitytaskforce.ca/seroprevalence-in-canada/)
seCA = read_excel("Data/Pro-seroprevalence.xlsx", sheet = "main")
seCA$date  <- as.Date(as.character(seCA$date), format = "%Y-%m-%d")
# use NL sero data for territories (so for consistency use TR name)
seCA <- seCA %>% rename( "TR" ="NL")

################ 2. collect the date you want for Sero AND cummulative 
# AH on July 1, 2025 - commented out replace by IHME data source.
sero <- seCA  
#%>% left_join(seUS, by = "date")  

# note: "09-01" is actually end of September in Sero (End September,30, 2020 and End of May,29 2021)
sero <- sero %>% filter(sero$date %in% c(as.Date("2020-09-01"), as.Date("2021-01-01"),as.Date("2021-05-01")))
diffsero <- data.frame(date = "2nd-2020", sero[2, -1] - sero[1, -1])
diffsero <- rbind(diffsero, data.frame(date = "1st-2021", sero[3, -1] - sero[2, -1]))
diffsero[1,(diffsero[1,1:11]<1)]=1
diffsero[2,diffsero[2,1:11]<1]=1
diffsero = data.frame(diffsero, "Bangladesh" = c(112.2, 76.9))
diffsero = data.frame(diffsero, "France" = c(2.1, 1.4))
diffsero = data.frame(diffsero, "Guyana" = c(13.1, 10.6))
diffsero = data.frame(diffsero, "India" = c(28.9, 29.9))
diffsero = data.frame(diffsero, "Norway" = c(2.1, 1.2))
diffsero = data.frame(diffsero, "Philippines" = c(28.5, 26.2))
diffsero = data.frame(diffsero, "Spain" = c(2.2, 1.6))
diffsero = data.frame(diffsero, "Suriname" = c(9.9, 8.8))
diffsero = data.frame(diffsero, "Thailand" = c(23.7, 6.2))
diffsero = data.frame(diffsero, "UK" = c(2.6, 1.4))
diffsero = data.frame(diffsero, "US" = c(2.6, 1.9))
# Other Int is assumed to be US
diffsero = data.frame(diffsero, "Other.Int" = c(2.6, 1.9))

# AH July 1, 2025: The below is commented out because I don't think it is needed.
# Cumulative 13 days earlier (Sep 17 2020, and May 16)
# source("getcases.pro.R") 
# get the percentage infected in population
#origins <- c("AB", "BC", "MB" ,"NB" ,"NS" , "ON","PEI", "QC","SK", "TR", "US"  )
#Percum <- Cum %>% filter(Cum$date %in% c(as.Date("2020-09-17"), as.Date("2021-05-16")))
#for (i in 1:length(origins)){
#  pr <- origins[i]
#  poporig <- pop$pop[pop$province == pr]
#  Percum[pr] <- Percum1[pr]*100 / poporig
#}
#diffcum <- data.frame(date = "Change2", Percum[2, -1] - Percum[1, -1])

######### 3. computing constant CF 
# compute correction factor for each region
CF <- diffsero
# Below is added by AH on July 1, 2025 for the international correction factors
# from IHME source


# AH on July 1, 2025 code below is commented out - not sure what it was doing.
#origins <- c("AB", "BC", "MB" ,"NB" ,"NS" , "ON","PEI", "QC","SK", "TR")
#for (i in 1:length(origins)){
#  pr <- origins[i]
#  a <- diffsero[pr][1,] 
#  b <- diffcum[pr][1,] 
#  CF[pr] <- a/b
#}


