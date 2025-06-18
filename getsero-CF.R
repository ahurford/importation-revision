## Winter 2022
## Project: Importation Risk
## Zahra Mohammadi

source("preliminaries.R")

#################### 1.get data
## USA from (https://covid.cdc.gov/covid-data-tracker/#nationwide-blood-donor-seroprevalence)
seUS = rbind(data.frame(date = c(seq(as.Date("2020-07-01"), as.Date("2021-06-01"), by = "month"))
  ,
  US = c(3.5, 4.7, 4.9, 5.9, 8.2, 11.9, 15.9, 18.4, 19.8, 20.2, 20.6, 20.7)
))


## Canada from (https://www.covid19immunitytaskforce.ca/seroprevalence-in-canada/)
seCA = read_excel("Data/Pro-seroprevalence.xlsx", sheet = "main")
seCA$date  <- as.Date(as.character(seCA$date), format = "%Y-%m-%d")
# use NL sero data for territories (so for consistency use TR name)
seCA <- seCA %>% rename( "TR" ="NL")

################ 2. collect the date you want for Sero AND cummulative 
sero <- seCA  %>% left_join(seUS, by = "date")  

# note: "09-01" is actually end of September in Sero (End September,30, 2020 and End of May,29 2021)
sero <- sero %>% filter(sero$date %in% c(as.Date("2020-09-01"), as.Date("2021-05-01")))
diffsero <- data.frame(date = "Change", sero[2, -1] - sero[1, -1])


# Cummulative 13 days earlier (Sep 17 2020, and May 16)
source("getcases.pro.R") 
# get the percentage infected in population
origins <- c("AB", "BC", "MB" ,"NB" ,"NS" , "ON","PEI", "QC","SK", "TR", "US"  )
Percum <- Cum %>% filter(Cum$date %in% c(as.Date("2020-09-17"), as.Date("2021-05-16")))
for (i in 1:length(origins)){
  pr <- origins[i]
  poporig <- pop$pop[pop$province == pr]
  Percum[pr] <- Percum[pr]*100 / poporig
}
diffcum <- data.frame(date = "Change2", Percum[2, -1] - Percum[1, -1])

######### 3. computing constant CF 
# compute correction factor for each region
CF <- diffsero
origins <- c("AB", "BC", "MB" ,"NB" ,"NS" , "ON","PEI", "QC","SK", "TR", "US"  )
for (i in 1:length(origins)){
  pr <- origins[i]
  a <- diffsero[pr][1,] 
  b <- diffcum[pr][1,] 
  CF[pr] <- a/b
}

