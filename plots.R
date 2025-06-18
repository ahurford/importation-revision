## Winter 2022
## Project: Importation Risk
## Zahra Mohammadi
#-----------------------------
source("Epimodel.R")
## IMP and dfplot data frames will be used for plotting
## IMP : output epi model Canadian provinces, rotational workers, regular travelers, INT
## dfplot : included best model, epi model and reported CA and INT

##----- Make 50% prediction intervals assuming no parameter estimation uncertainty
dfplt["Est.Dom.low"] =qpois(0.025, dfplt$Est.Dom)
dfplt["Est.Dom.high"] =qpois(0.975, dfplt$Est.Dom)
dfplt["Est.Int.low"] =qpois(0.025, dfplt$Est.Int)
dfplt["Est.Int.high"] =qpois(0.975, dfplt$Est.Int)
dfplt["bestDom.low"] =qpois(0.025, dfplt$bestDom)
dfplt["bestDom.high"] =qpois(0.975, dfplt$bestDom)
dfplt["bestInt.low"] =qpois(0.025, dfplt$bestInt)
dfplt["bestInt.high"] =qpois(0.975, dfplt$bestInt)

###---------- make monthly version dataframe ------------
dfplt$year  <- strftime(dfplt$date, "%Y")
dfplt$month <- strftime(dfplt$date, "%m")

dfplt.month <- dfplt %>%
  group_by(year, month) %>%
  dplyr::summarize(CA = sum(Est.Dom), INT = sum(Est.Int),
                   bCA = sum(bestDom), bInt= sum(bestInt)
  ) %>%
  as.data.frame()

#monthly reported case
Rep  <- select(Travel_case, c(date, Tot.Int,Tot.Dom )) 
Rep$year  <- strftime(Rep$date, "%Y")   
Rep$month <- strftime(Rep$date, "%m")
Rep.month <- Rep %>%
  group_by( year, month) %>%
  dplyr::summarize( RInt= sum(Tot.Int),
                    RCA = sum(Tot.Dom)
  ) %>%
  as.data.frame()

dfplt.month  <- dfplt.month  %>% left_join(Rep.month , by ="month")
#add column date to it 
dfplt.month <- dfplt.month %>%
  mutate(date = with(., sprintf("%s-%02s", year.x, month)))
dfplt.month$date <- as.Date(paste(dfplt.month$date,"-01",sep=""))  # convert to date

##-------------------- plots ---------------------
### 1. estimated infected - bar plots
df1.sub <- select(IMP, c("report" , "RE.Int", "REW.Ca" ))
df1.sub <- df1.sub %>% rename(INT = RE.Int, CA = REW.Ca)
mdf1.sub <- melt(df1.sub, id=c("report"))

m1 <- ggplot() +
  geom_bar(data=mdf1.sub , aes(x=report, y=value, fill=variable), stat="identity") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(color=" ", 
       title="Mechanisitic model predictions",
       y="\n Reported travel-related cases (daily)",
       x="") +
  guides(fill = guide_legend(title = "Departure Origin"))+
  theme(legend.position="right") +
  scale_fill_manual(values=c(cpalete[1], cpalete[2])) + 
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1.2)), 
                          legend.position="bottom" ,
                          legend.text=element_text(size=rel(1.2)),
                          plot.title=element_text(size=rel(1.3), face="bold"),
                          axis.title = element_text(size=rel(1.1)),
                          axis.text.y = element_text(size=rel(1.2)))


### 2. infected percentage rw and r in Canada and International
df2.sub <- select(IMP, c("report", "RE.Int", "RE.Ca", "RW", "Total"))

for (i in 1:nrow(df2.sub)){
  for (j in 2:length(df2.sub)){
    df2.sub[i,j] <- (df2.sub[i,j]*100)/(df2.sub$Total[i])
  }
}
df2.sub <- select(df2.sub, -c("Total")) %>% rename(CA.r = RE.Ca, CA.rw = RW, INT = RE.Int)
mdf2.sub <- melt(df2.sub, id=c("report"))

m2 <- ggplot() +
  geom_bar(data=mdf2.sub , aes(x=report, y=value, fill=variable), stat="identity") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(color=" ", 
       title="Mechanistic model predictions",
       y="\n Reported travel-related cases (daily %)",
       x="") +
  guides(fill = guide_legend(title = "Type of Travelers"))+
  theme(legend.position="right") +
  scale_fill_manual(values=c(cpalete[1], cpalete[2], "#FFCC63")) + 
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1.3)), 
                          legend.position="bottom" ,
                          legend.text=element_text(size=rel(1.2)),
                          plot.title=element_text(size=rel(1.3), face="bold"),
                          axis.title = element_text(size=rel(1.1)),
                          axis.text.y = element_text(size=rel(1.2)))+ 
  scale_y_continuous(labels = function(x) paste0(x*1, "%"))


### 3. comparison models plots
### 3.1. daily International
df1.Int <- select(dfplt, c(date, bestInt, Est.Int, Est.Int.low, Est.Int.high,bestInt.low, bestInt.high))

p1mech <- ggplot() + 
  geom_ribbon(data=df1.Int, aes(x=date, ymin=Est.Int.low, ymax=Est.Int.high), fill=cpalete[1],alpha = 0.3)+
  geom_line(data=df1.Int, aes(x=date, y=Est.Int),color=cpalete[1], size = 1.5) +
  geom_point(data=dfplt, aes(x=date, y= Rep.Int),color='black', size=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16))+
  labs(x="", y="\n Travel-related cases (daily)",
       title="International: Mechanistic model") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))

p1best <- ggplot() + 
  geom_ribbon(data=df1.Int, aes(x=date, ymin=bestInt.low, ymax=bestInt.high), fill=cpalete[1],alpha = 0.3)+
  geom_line(data=df1.Int, aes(x=date, y=bestInt),color=cpalete[1],size=1.5) +
  geom_point(data=dfplt, aes(x=date, y= Rep.Int),color='black', size=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16))+
  labs(x="", y="\n Travel-related cases (daily)",
       title="International: Best model") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))



#### 3.2. daily Canada
df1.Dom <- select(dfplt, c(date, bestDom, Est.Dom, bestDom.low, bestDom.high, Est.Dom.low, Est.Dom.high))

p2mech <- ggplot() + 
  geom_ribbon(data=df1.Dom, aes(x=date, ymin=Est.Dom.low, ymax=Est.Dom.high), fill=cpalete[2],alpha = 0.3)+
  geom_line(data=df1.Dom, aes(x=date, y=Est.Dom),color=cpalete[2], size=1.5) +
  geom_point(data=dfplt, aes(x=date, y= Rep.Dom),color='black', size=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16))+
  labs(x="", y="\n Travel-related cases (daily)",
       title="Canada: Mechanistic model") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))

p2best <- ggplot() + 
  geom_ribbon(data=df1.Dom, aes(x=date, ymin=bestDom.low, ymax=bestDom.high), fill=cpalete[2],alpha = 0.3)+
  geom_line(data=df1.Dom, aes(x=date, y=bestDom),color=cpalete[2],size=1.5) +
  geom_point(data=dfplt, aes(x=date, y= Rep.Dom),color='black', size=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16))+
  labs(x="", y="\n Travel-related cases (daily)",
       title="Canada: Best model") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))



### 3.3. monthly International
df2.Int <- select(dfplt.month, c(date, bInt, INT))
#df2.Int <- melt(df2.Int, id=c("date"))
df2.Int["bInt.low"] = qpois(0.025, df2.Int$bInt)
df2.Int["bInt.high"] = qpois(0.975, df2.Int$bInt)
df2.Int["INT.low"] = qpois(0.025, df2.Int$INT)
df2.Int["INT.high"] = qpois(0.975, df2.Int$INT)

p3 <- ggplot()+
  geom_ribbon(data=df2.Int, aes(x=date, ymin=bInt.low, ymax=bInt.high, fill = cpalete[8]), alpha = 0.3)+
  geom_ribbon(data=df2.Int, aes(x=date, ymin=INT.low, ymax=INT.high, fill = cpalete[1]), alpha = 0.3)+
  geom_line(data= df2.Int, aes(x=date, y=bInt), size=0.5, color=cpalete[8])+
  geom_line(data= df2.Int, aes(x=date, y=INT), size=0.5, color=cpalete[1])+
  geom_point(data= df2.Int, aes(x=date, y=bInt), size=1, color=cpalete[8])+
  geom_point(data= df2.Int, aes(x=date, y=INT), size=1, color=cpalete[1])+
  #geom_bar(data=df2.Int, aes(x=date, y=value, fill=variable), stat="identity", position=position_dodge(),alpha=.8) +
  #geom_line(data= dfplt.month, aes(x=date, y=RInt),stat="identity",color="black", size=0) +
  geom_point(data= dfplt.month,  aes(x=date, y=RInt),color="black", size =1.5) +
  labs(x="", y="\n Reported travel-related cases (monthly)", title="International" ) +
  ylim(0, 20) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11))+
  labs(x="", y="\n Reported travel-related cases (monthly)",
       title="Comparison of model predictions to reported travel-related cases \n International") +
  scale_fill_manual(name = "", labels = c("Best model","Mechanistic model", "Data"), values = c( cpalete[8],cpalete[1], "black")) +
  scale_color_manual(values = plt1)+
  guides(color = guide_legend(title = ""), size=rel(1.4)) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1.2)),
                          legend.position="bottom",
                          legend.text=element_text(size=rel(1.2)),
                          plot.title=element_text(size=rel(1.3), face="bold"),
                          axis.title = element_text(size=rel(1.1)),
                          axis.text.y = element_text(size=rel(1.2)))


# 3.4. monthly Canada
df2.Dom <- select(dfplt.month, c(date, bCA, CA))
df2.Dom["bCA.low"] = qpois(0.025, df2.Dom$bCA)
df2.Dom["bCA.high"] = qpois(0.975, df2.Dom$bCA)
df2.Dom["CA.low"] = qpois(0.025, df2.Dom$CA)
df2.Dom["CA.high"] = qpois(0.975, df2.Dom$CA)


p4 <- ggplot() +
  geom_ribbon(data=df2.Dom, aes(x=date, ymin=bCA.low, ymax=bCA.high, fill = cpalete[8]), alpha = 0.3)+
  geom_ribbon(data=df2.Dom, aes(x=date, ymin=CA.low, ymax=CA.high, fill = cpalete[2]), alpha = 0.3)+
  geom_line(data= df2.Dom, aes(x=date, y=bCA), size=0.5, color=cpalete[8])+
  geom_line(data= df2.Dom, aes(x=date, y=CA), size=0.5, color=cpalete[2])+
  geom_point(data= df2.Dom, aes(x=date, y=bCA), size=1, color=cpalete[8])+
  geom_point(data= df2.Dom, aes(x=date, y=CA), size=1, color=cpalete[2])+
  #geom_bar(data=df2.Dom, aes(x=date, y=value, fill=variable), stat="identity", position=position_dodge(),alpha=.8 ) +
  #geom_line(data= dfplt.month, aes(x=date, y= RCA),stat="identity",color="black", size=1) +
  geom_point(data= dfplt.month,  aes(x=date, y= RCA),color="black", size =1.5) +
  ylim(0, 120) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11))+
  labs(x="", y="\n Reported travel-related cases (monthly)",
       title="Comparison of model predictions to reported travel-related cases \n Canada") +
  scale_color_manual(values = plt2)+
  scale_fill_manual(name = "", labels = c( "Best model","Mechanistic model", "Data"), values = c( cpalete[8],cpalete[2], "black")) +
  guides(color = guide_legend(title = ""), size=rel(1.4)) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1.2)),
                          legend.position="bottom",
                          legend.text=element_text(size=rel(1.2)),
                          plot.title=element_text(size=rel(1.3), face="bold"),
                          axis.title = element_text(size=rel(1.1)),
                          axis.text.y = element_text(size=rel(1.2)))



m2
ggsave("Figure/figureApp1.png", width = 5, , height = 5, dpi =500)


(p2best+p2mech)/(p1best+p1mech) + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 20))
ggsave("Figure/Figure3.png", width = 18, , height = 12, dpi =500)


