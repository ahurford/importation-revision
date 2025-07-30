require(lubridate)
require(tidyr)

TDF = read.csv('~/Desktop/Work/Students/Postdoc/Mohammadi/Risk-Importation-main/Data/TraVolData/TravelVolumeNLCHI.csv')
TDF = mutate(TDF, TR = YT+NU+NT)
TDF$Date <- as.Date(as.character(TDF$Date), format = "%Y-%m-%d")
TDF.Int = select(TDF, -ON, -PEI, -QC, -SK,-TR, -SPM, -YT, -AB, -BC, -MB, -NB, -NL, -NS, -NT, -NU, -CA_nop, -AllCA, -ALLNL, -ALLCANotNL, -AllFR)
TDF.long = TDF.Int %>% 
  pivot_longer(
    cols = `Argentina`:`Mauritania`, 
    names_to = "region",
    values_to = "volume"
  )

TDF.monthly=TDF.long%>%
  mutate(year=lubridate::year(Date),
        month=lubridate::month(Date))%>%
  group_by(region,year,month)%>%
  summarize(month.volume = sum(volume))

TDF.quarterly=mutate(TDF.monthly, quarter = case_when(month==9 |month==10 | month==11 & year==2020 ~ 1, month==12 & year==2020 ~ 2, month==1 | month==2 & year==2021 ~2, month==3 |month==4 | month==5 & year==2021 ~ 3))%>%
  group_by(quarter,region)%>%
  summarize(quarter.volume = sum(month.volume))%>%
  group_by(quarter,region)

quarter.total = TDF.quarterly%>%
  group_by(quarter)%>%
  summarize(total = sum(quarter.volume))

TDF.quarterly = left_join(TDF.quarterly,quarter.total, join_by(quarter))%>%
  ungroup()%>%
  mutate(percent = 100*quarter.volume/total)

TDF.max = TDF.quarterly%>%
  group_by(region)%>%
  summarise(max.percent = max(percent))%>%
  filter(max.percent > 2.5)

TDF.quarterly = right_join(TDF.quarterly, TDF.max)%>%
  select(quarter,region,percent)
print(TDF.quarterly, n=36)

TDF.filtered = select(TDF, Date,ON,PEI, QC, SK, AB, BC, MB, NB, NS, TR, ALLCANotNL, AllFR, Bangladesh, France, Guyana, India, Norway, Philippines, Spain, Suriname, Thailand, UK, USA)%>%
  rename(date=Date)
write.csv(TDF.filtered,'~/Desktop/Work/Research/Research_Projects/2025/importation-revision/Data/TraVolData/TravelVolumeNLCHIfiltered.csv', row.names = F)

TDF.filtered.long =  TDF.filtered %>% 
  pivot_longer(
    cols = `ON`:`USA`, 
    names_to = "region",
    values_to = "volume"
  )

TDF.filtered.monthly=TDF.filtered.long%>%
  mutate(year=lubridate::year(date),
         month=lubridate::month(date))%>%
  group_by(region,year,month)%>%
  summarize(month.volume = sum(volume))%>%
  pivot_wider(names_from = region, values_from = month.volume)
TDF.filtered.monthly = data.frame(date = as.Date(c("2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01")),TDF.filtered.monthly)%>%
  select(-month,-year)
write.csv(TDF.filtered.monthly,'~/Desktop/Work/Research/Research_Projects/2025/importation-revision/Data/TraVolData/TravelVolumeNLCHIfilteredMonthly.csv',row.names = F)
