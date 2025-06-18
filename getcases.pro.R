## Winter 2022
## Project: Importation Risk
## Zahra Mohammadi

source("preliminaries.R")

#USA cases (CSSE at Johns Hopkins University is releasing cumulative counts)
COVID.JH <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv', header = TRUE, check.names = FALSE) %>%
  rename(Country = "Country/Region" )
COVID.JH <- COVID.JH %>% subset(Country =="US")
COVID.JH <- COVID.JH[,-1:-3]
COVID.US <- COVID.JH %>%
  gather(date, value, -"Long") %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y"))
COVID.US <- select(COVID.US, -("Long")) 
COVID.US["US"] <- diff(c(0, COVID.US$value))
COVID.US <- COVID.US %>% 
  filter((COVID.US$date < "2021-06-12") & (COVID.US$date >= "2020-09-01" ) ) %>%
  rename(cumUS = value )


#CA provinces cases 
COVID.CA <- read.csv("Data/covid19-download.csv")
COVID.CA <- select(COVID.CA ,c(date,prname,numtoday, numtotal))
COVID.CA$date  <- as.Date(as.character(COVID.CA$date), format = "%Y-%m-%d")
COVID.CA <- COVID.CA %>% filter((COVID.CA$date < "2021-06-12") & (COVID.CA$date >= "2020-09-01" ) )
COVID.CA = COVID.CA[!COVID.CA$prname %in% c("Repatriated travellers", "Canada" ,"Newfoundland and Labrador"),] 
# change names
COVID.CA <- COVID.CA %>%
  mutate(prname = recode(prname, 'British Columbia' = 'BC', 'Alberta' = 'AB', 'Saskatchewan' = 'SK',
                         'Manitoba' = 'MB', 'Ontario' = 'ON', 'Quebec' = 'QC', 'New Brunswick' = 'NB',
                          'Nova Scotia' = 'NS', 'Prince Edward Island' = 'PEI','Yukon' = 'YU', 
                          'Northwest Territories' = 'NWT', 'Nunavut' = 'NU'))

##### Case dataframe
Case  = select(COVID.CA ,c(date,prname,numtoday))%>%
  pivot_wider(
    id_cols = date,
    names_from = prname,
    values_from = numtoday
  )
Case["TR"] <- rowSums(Case[,c("NU", "NWT" ,"YU" )])   # sum territories as one region
Case <- select(Case , -c("NU", "NWT" ,"YU"))          #remove extra (territories) 
Case <- select(Case , c("date", "AB", "BC", "MB", "NB", "NS", "ON", "PEI", "QC", "SK", "TR")) # reorder columns

###### check we have all dates 
FullSeq <- seq.Date(from = min(Case$date), to = max(Case$date), by = 1)
Missing <- FullSeq[!FullSeq %in% Case$date]
Missing


##### Cummulative data frame
Cum  = select(COVID.CA ,c(date,prname,numtotal))%>%
  pivot_wider(
    id_cols = date,
    names_from = prname,
    values_from = numtotal
  )
Cum["TR"] <- rowSums(Cum[,c("NU", "NWT" ,"YU" )])   # sum territories as one region
Cum <- select(Cum , -c("NU", "NWT" ,"YU"))          #remove extra (territories) 
Cum <- select(Cum , c("date", "AB", "BC", "MB", "NB", "NS", "ON", "PEI", "QC", "SK", "TR")) # reorder columns

####################### Fix missing values no data on Christmas and weekends(just AB, BC before October) 
#MB
Case$MB[Case$date == as.Date("2020-12-25")] <- round(Case$MB[Case$date == as.Date("2020-12-27")]/3)
Case$MB[Case$date == as.Date("2020-12-26")] <- round(Case$MB[Case$date == as.Date("2020-12-27")]/3)
Case$MB[Case$date == as.Date("2020-12-27")] <- round(Case$MB[Case$date == as.Date("2020-12-27")]/3)
#NS
Case$NS[Case$date == as.Date("2020-12-25")] <- round(Case$NS[Case$date == as.Date("2020-12-28")]/4)
Case$NS[Case$date == as.Date("2020-12-26")] <- round(Case$NS[Case$date == as.Date("2020-12-28")]/4)
Case$NS[Case$date == as.Date("2020-12-27")] <- round(Case$NS[Case$date == as.Date("2020-12-28")]/4)
Case$NS[Case$date == as.Date("2020-12-28")] <- round(Case$NS[Case$date == as.Date("2020-12-28")]/4)
#QC
Case$QC[Case$date == as.Date("2020-12-25")] <- round(Case$QC[Case$date == as.Date("2020-12-26")]/2)
Case$QC[Case$date == as.Date("2020-12-26")] <- round(Case$QC[Case$date == as.Date("2020-12-26")]/2)
## AB and BC has weekend zero until the end october
Case$AB[Case$date == as.Date("2020-09-05")] <- round(Case$AB[Case$date == as.Date("2020-09-08")]/4)
Case$AB[Case$date == as.Date("2020-09-06")] <- round(Case$AB[Case$date == as.Date("2020-09-08")]/4)
Case$AB[Case$date == as.Date("2020-09-07")] <- round(Case$AB[Case$date == as.Date("2020-09-08")]/4)
Case$AB[Case$date == as.Date("2020-09-08")] <- round(Case$AB[Case$date == as.Date("2020-09-08")]/4)
Case$AB[Case$date == as.Date("2020-10-10")] <- round(Case$AB[Case$date == as.Date("2020-10-13")]/4)
Case$AB[Case$date == as.Date("2020-10-11")] <- round(Case$AB[Case$date == as.Date("2020-10-13")]/4)
Case$AB[Case$date == as.Date("2020-10-12")] <- round(Case$AB[Case$date == as.Date("2020-10-13")]/4)
Case$AB[Case$date == as.Date("2020-10-13")] <- round(Case$AB[Case$date == as.Date("2020-10-13")]/4)

Case$AB[Case$date == as.Date("2020-09-12")] <- round(Case$AB[Case$date == as.Date("2020-09-14")]/3)
Case$AB[Case$date == as.Date("2020-09-13")] <- round(Case$AB[Case$date == as.Date("2020-09-14")]/3)
Case$AB[Case$date == as.Date("2020-09-14")] <- round(Case$AB[Case$date == as.Date("2020-09-14")]/3)
Case$AB[Case$date == as.Date("2020-09-19")] <- round(Case$AB[Case$date == as.Date("2020-09-21")]/3)
Case$AB[Case$date == as.Date("2020-09-20")] <- round(Case$AB[Case$date == as.Date("2020-09-21")]/3)
Case$AB[Case$date == as.Date("2020-09-21")] <- round(Case$AB[Case$date == as.Date("2020-09-21")]/3)
Case$AB[Case$date == as.Date("2020-09-26")] <- round(Case$AB[Case$date == as.Date("2020-09-28")]/3)
Case$AB[Case$date == as.Date("2020-09-27")] <- round(Case$AB[Case$date == as.Date("2020-09-28")]/3)
Case$AB[Case$date == as.Date("2020-09-28")] <- round(Case$AB[Case$date == as.Date("2020-09-28")]/3)
Case$AB[Case$date == as.Date("2020-10-03")] <- round(Case$AB[Case$date == as.Date("2020-10-05")]/3)
Case$AB[Case$date == as.Date("2020-10-04")] <- round(Case$AB[Case$date == as.Date("2020-10-05")]/3)
Case$AB[Case$date == as.Date("2020-10-05")] <- round(Case$AB[Case$date == as.Date("2020-10-05")]/3)
Case$AB[Case$date == as.Date("2020-10-17")] <- round(Case$AB[Case$date == as.Date("2020-10-19")]/3)
Case$AB[Case$date == as.Date("2020-10-18")] <- round(Case$AB[Case$date == as.Date("2020-10-19")]/3)
Case$AB[Case$date == as.Date("2020-10-19")] <- round(Case$AB[Case$date == as.Date("2020-10-19")]/3)
Case$AB[Case$date == as.Date("2020-10-24")] <- round(Case$AB[Case$date == as.Date("2020-10-26")]/3)
Case$AB[Case$date == as.Date("2020-10-25")] <- round(Case$AB[Case$date == as.Date("2020-10-26")]/3)
Case$AB[Case$date == as.Date("2020-10-26")] <- round(Case$AB[Case$date == as.Date("2020-10-26")]/3)
#BC
Case$BC[Case$date == as.Date("2020-09-05")] <- round(Case$BC[Case$date == as.Date("2020-09-08")]/4)
Case$BC[Case$date == as.Date("2020-09-06")] <- round(Case$BC[Case$date == as.Date("2020-09-08")]/4)
Case$BC[Case$date == as.Date("2020-09-07")] <- round(Case$BC[Case$date == as.Date("2020-09-08")]/4)
Case$BC[Case$date == as.Date("2020-09-08")] <- round(Case$BC[Case$date == as.Date("2020-09-08")]/4)
Case$BC[Case$date == as.Date("2020-10-10")] <- round(Case$BC[Case$date == as.Date("2020-10-13")]/4)
Case$BC[Case$date == as.Date("2020-10-11")] <- round(Case$BC[Case$date == as.Date("2020-10-13")]/4)
Case$BC[Case$date == as.Date("2020-10-12")] <- round(Case$BC[Case$date == as.Date("2020-10-13")]/4)
Case$BC[Case$date == as.Date("2020-10-13")] <- round(Case$BC[Case$date == as.Date("2020-10-13")]/4)

Case$BC[Case$date == as.Date("2020-09-12")] <- round(Case$BC[Case$date == as.Date("2020-09-14")]/3)
Case$BC[Case$date == as.Date("2020-09-13")] <- round(Case$BC[Case$date == as.Date("2020-09-14")]/3)
Case$BC[Case$date == as.Date("2020-09-14")] <- round(Case$BC[Case$date == as.Date("2020-09-14")]/3)
Case$BC[Case$date == as.Date("2020-09-19")] <- round(Case$BC[Case$date == as.Date("2020-09-21")]/3)
Case$BC[Case$date == as.Date("2020-09-20")] <- round(Case$BC[Case$date == as.Date("2020-09-21")]/3)
Case$BC[Case$date == as.Date("2020-09-21")] <- round(Case$BC[Case$date == as.Date("2020-09-21")]/3)
Case$BC[Case$date == as.Date("2020-09-26")] <- round(Case$BC[Case$date == as.Date("2020-09-28")]/3)
Case$BC[Case$date == as.Date("2020-09-27")] <- round(Case$BC[Case$date == as.Date("2020-09-28")]/3)
Case$BC[Case$date == as.Date("2020-09-28")] <- round(Case$BC[Case$date == as.Date("2020-09-28")]/3)
Case$BC[Case$date == as.Date("2020-10-03")] <- round(Case$BC[Case$date == as.Date("2020-10-05")]/3)
Case$BC[Case$date == as.Date("2020-10-04")] <- round(Case$BC[Case$date == as.Date("2020-10-05")]/3)
Case$BC[Case$date == as.Date("2020-10-05")] <- round(Case$BC[Case$date == as.Date("2020-10-05")]/3)
Case$BC[Case$date == as.Date("2020-10-17")] <- round(Case$BC[Case$date == as.Date("2020-10-19")]/3)
Case$BC[Case$date == as.Date("2020-10-18")] <- round(Case$BC[Case$date == as.Date("2020-10-19")]/3)
Case$BC[Case$date == as.Date("2020-10-19")] <- round(Case$BC[Case$date == as.Date("2020-10-19")]/3)
Case$BC[Case$date == as.Date("2020-10-24")] <- round(Case$BC[Case$date == as.Date("2020-10-26")]/3)
Case$BC[Case$date == as.Date("2020-10-25")] <- round(Case$BC[Case$date == as.Date("2020-10-26")]/3)
Case$BC[Case$date == as.Date("2020-10-26")] <- round(Case$BC[Case$date == as.Date("2020-10-26")]/3)

####################### 

#merging and make one data frame for canadain provinces and US
Case <- Case %>% left_join(COVID.US, by= "date") 
Case <- select(Case , -("cumUS"))

Cum <- Cum %>% left_join(COVID.US, by = "date")
Cum <- select(Cum , -c("US"))
Cum <- Cum %>% rename("US" = "cumUS")

#population of provinces and US
pop <- read.csv("https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/other/prov_map.csv") 
pop <- select(pop, c("province_short", "pop")) %>% rename("province" = "province_short")
pop$province[pop$province == "PE"] <- "PEI"
## add three territories population together
pop[nrow(pop) + 1,] <- c("TR", pop$pop[pop$province == "NU"] + pop$pop[pop$province == "YT"] + pop$pop[pop$province == "NT"] )
pop[nrow(pop)+1, ] <- c("US", 332660077 )
pop <- subset(pop, !province %in% c("NU", "YT", "NT", "NL", "RP"))
pop$pop <- as.numeric(pop$pop)


## convert all column to numeric except date
Case <- Case %>% mutate_at(c('NB', 'ON','SK','PEI'), as.numeric)
Cum <- Cum %>% mutate_at(c('AB','BC', 'MB','NB','NS','ON','PEI','QC','SK', 'US'), as.numeric)


