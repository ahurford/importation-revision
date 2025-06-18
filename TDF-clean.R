require(lubridate)
require(tidyr)
TDF = read.csv('~/Desktop/Work/Students/Postdoc/Mohammadi/Risk-Importation-main/Data/TraVolData/TravelVolumeNLCHI.csv')
TDF$Date <- as.Date(as.character(TDF$Date), format = "%Y-%m-%d")
TDF.Int = select(TDF, -ON, -PEI, -QC, -SK, -SPM, -YT, -AB, -BC, -MB, -NB, -NL, -NS, -NT, -NU, -CA_nop, -AllCA, -ALLNL, -ALLCANotNL, -AllFR)
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

TDF.filtered = select(TDF, ON, PEI, QC, SK, YT, AB, BC, MB, NB, NL, NS, NT, NU, CA_nop, AllCA, ALLNL, ALLCANotNL, AllFR, Bangladesh, France, Guyana, India, Norway, Philippines, Spain, Suriname, Thailand, UK, USA)
write.csv('~/Desktop/Work/Students/Postdoc/Mohammadi/Risk-Importation-main/Data/TraVolData/TravelVolumeNLCHIfiltered.csv')
                     