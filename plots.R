## Winter 2022
## Project: Importation Risk
## Zahra Mohammadi
#-----------------------------
source("Epimodel.R")
## IMP and dfplot data frames will be used for plotting
## IMP : output epi model Canadian provinces, rotational workers, regular travelers, INT
## dfplot : included best model, epi model and reported CA and INT

bestmodel.Int <- Int.Inf.nb$fitted.values # infection prevalence
bestmodel.Int.Agg <-Int.Inf.agg.nb$fitted.values 
bestmodel.CA <- CA.TVInf.nb$fitted.values # travel volume x infection prevalence
bestmodel.CA.Agg <- CA.TVInf.agg.nb$fitted.values # infection prevalence
# make one dataframe including epi model, best model and reported for plotting 
# these are 273 values start from Sep 1 whereas predicted Imp start from Sep 6, 
# do adjust to make dataframe
dfplt <- df.modelfull
dfplt["bestInt"] <- bestmodel.Int  
dfplt["bestDom"] <- bestmodel.CA
dfplt["bestIntAgg"] <- bestmodel.Int.Agg
dfplt["bestDomAgg"] <- bestmodel.CA.Agg 

##----- Make 95% prediction intervals
# For the mechanistic model below these are based on Poisson measurement error
dfplt["Est.Dom.low"] =qpois(0.025, dfplt$Est.Dom)
dfplt["Est.Dom.high"] =qpois(0.975, dfplt$Est.Dom)
dfplt["Est.Int.low"] =qpois(0.025, dfplt$Est.Int)
dfplt["Est.Int.high"] =qpois(0.975, dfplt$Est.Int)
## AH: I am going to change this to confidence intervals for the glms as per the
# reviewers comments - although technically for glms this is prediction interval
# the errors bars for the glms are based on parameter that have 95% likelihood of
# explaining the data used to estimate the uncertainty on the predictions.
#dfplt["bestDom.low"] =qpois(0.025, dfplt$bestDom)
#dfplt["bestDom.high"] =qpois(0.975, dfplt$bestDom)
#dfplt["bestInt.low"] =qpois(0.025, dfplt$bestInt)
#dfplt["bestInt.high"] =qpois(0.975, dfplt$bestInt)
require(ciTools)

bestDomCI <- add_pi(df.model9.lag, CA.TVInf.nb, names = c("lpb", "upb"), alpha = 0.05, nsims = 20000)
bestIntCI <- add_pi(df.model6.lag, Int.Inf.nb, names = c("lpb", "upb"), alpha = 0.05, nsims = 20000)
bestDomAggCI <- add_pi(df.model9.lag, CA.TVInf.agg.nb, names = c("lpb", "upb"), alpha = 0.05, nsims = 20000)
bestIntAggCI <- add_pi(df.model6.lag, Int.Inf.agg.nb, names = c("lpb", "upb"), alpha = 0.05, nsims = 20000)
dfplt["bestDom.low"] = bestDomCI$lpb
dfplt["bestDom.high"] = bestDomCI$upb
dfplt["bestInt.low"] = bestIntCI$lpb
dfplt["bestInt.high"] = bestIntCI$upb 
dfplt["bestIntAgg.low"] = bestIntAggCI$lpb
dfplt["bestIntAgg.high"] = bestIntAggCI$upb
dfplt["bestDomAgg.low"] = bestDomAggCI$lpb
dfplt["bestDomAgg.high"] = bestDomAggCI$upb 

# AH: I deleted as section that is not important for the manuscript. This a reviewer
# comment was also to delete this.

### 3. comparison models plots
### 3.1. daily International
df1.Int <- select(dfplt, c(date, bestInt, Est.Int, Est.Int.low, Est.Int.high,bestInt.low, bestInt.high, bestIntAgg, bestIntAgg.low, bestIntAgg.high))


p1mech <- ggplot() + 
  geom_ribbon(data=df1.Int, aes(x=date, ymin=Est.Int.low, ymax=Est.Int.high), fill=cpalete[1],alpha = 0.3)+
  geom_line(data=df1.Int, aes(x=date, y=Est.Int),color=cpalete[1], size = 1.5) +
  geom_point(data=dfplt, aes(x=date, y= Rep.Int),color='black', size=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16))+
  labs(x="", y="\n Travel-related cases (daily)",
       title="International: Without travel-related cases") +
  ylim(c(0,11))+
  annotate("text", label = "Mechanistic model", x = as.Date("2020-11-05"), y = 11, size = 8, colour = "black")+
  annotate("text", label = "nLL = 192", x = as.Date("2020-10-15"), y = 10, size = 8, colour = "black") +
  annotate("text", label = "K = 0", x = as.Date("2020-10-01"), y = 9, size = 8, colour = "black") +
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
  ylim(c(0,11))+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))+
  annotate("text", label = "Infection prevalence", x = as.Date("2020-11-05"), y = 11, size = 8, colour = "black")+
  annotate("text", label = "nLL = 136", x = as.Date("2020-10-10"), y = 10, size = 8, colour = "black") +
  annotate("text", label = "LR = 34", x = as.Date("2020-10-06"), y = 9, size = 8, colour = "black") +
  annotate("text", label = "K = 13", x = as.Date("2020-10-01"), y = 8, size = 8, colour = "black")
# Best model is based only on infection prevalence.
# K=11, LR=692.7

p1agg <- ggplot() + 
  geom_ribbon(data=df1.Int, aes(x=date, ymin=bestIntAgg.low, ymax=bestIntAgg.high), fill=cpalete[1],alpha = 0.3)+
  geom_line(data=df1.Int, aes(x=date, y=bestIntAgg),color=cpalete[1],size=1.5) +
  geom_point(data=dfplt, aes(x=date, y= Rep.Int),color='black', size=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16))+
  labs(x="", y="\n Travel-related cases (daily)",
       title="International: Best model without country of origin") +
  ylim(c(0,11))+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))+
  annotate("text", label = "Infection prevalence", x = as.Date("2020-11-05"), y = 11, size = 8, colour = "black")+
annotate("text", label = "nLL = 145", x = as.Date("2020-10-10"), y = 10, size = 8, colour = "black") +
  annotate("text", label = "LR = 15", x = as.Date("2020-10-07"), y = 9, size = 8, colour = "black") +
  annotate("text", label = "K = 3", x = as.Date("2020-09-28"), y = 8, size = 8, colour = "black")


#### 3.2. daily Canada
df1.Dom <- select(dfplt, c(date, bestDom, Est.Dom, bestDom.low, bestDom.high, Est.Dom.low, Est.Dom.high, bestDomAgg,bestDomAgg.low, bestDomAgg.high))

p2mech <- ggplot() + 
  geom_ribbon(data=df1.Dom, aes(x=date, ymin=Est.Dom.low, ymax= Est.Dom.high), fill=cpalete[2],alpha = 0.3)+
  geom_line(data=df1.Dom, aes(x=date, y=Est.Dom),color=cpalete[2], size=1.5) +
  geom_point(data=dfplt, aes(x=date, y= Rep.Dom),color='black', size=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16))+
  labs(x="", y="\n Travel-related cases (daily)",
       title="Canada: Without travel-related cases") +
  ylim(c(0,20))+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))+
  annotate("text", label = "Mechanistic model", x = as.Date("2020-11-05"), y = 19.5, size = 8, colour = "black")+
  annotate("text", label = "nLL = 538", x = as.Date("2020-10-14"), y = 18, size = 8, colour = "black") +
  annotate("text", label = "K = 0", x = as.Date("2020-10-02"), y = 16.5, size = 8, colour = "black")

p2agg <- ggplot() + 
  geom_ribbon(data=df1.Dom, aes(x=date, ymin=bestDomAgg.low, ymax= bestDomAgg.high), fill=cpalete[2],alpha = 0.3)+
  geom_line(data=df1.Dom, aes(x=date, y=bestDomAgg),color=cpalete[2], size=1.5) +
  geom_point(data=dfplt, aes(x=date, y= Rep.Dom),color='black', size=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16))+
  labs(x="", y="\n Travel-related cases (daily)",
       title="Canada: Best model without province of origin") +
  ylim(c(0,20))+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))+
  annotate("text", label = "Infection prevalence x travel volume", x = as.Date("2020-12-20"), y = 19.5, size = 8, colour = "black")+
  annotate("text", label = "nLL = 318", x = as.Date("2020-10-13"), y = 18, size = 8, colour = "black") +
  annotate("text", label = "LR = 31", x = as.Date("2020-10-07"), y = 16.5, size = 8, colour = "black") +
  annotate("text", label = "K = 3", x = as.Date("2020-09-30"), y = 15, size = 8, colour = "black")


p2best <- ggplot() + 
  geom_ribbon(data=df1.Dom, aes(x=date, ymin=bestDom.low, ymax=bestDom.high), fill=cpalete[2],alpha = 0.3)+
  geom_line(data=df1.Dom, aes(x=date, y=bestDom),color=cpalete[2],size=1.5) +
  geom_point(data=dfplt, aes(x=date, y= Rep.Dom),color='black', size=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16))+
  labs(x="", y="\n Travel-related cases (daily)",
       title="Canada: Best model") +
  ylim(c(0,20))+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))+
  annotate("text", label = "Infection prevalence x travel volume", x = as.Date("2020-12-20"), y = 19.5, size = 8, colour = "black")+
  annotate("text", label = "nLL = 269", x = as.Date("2020-10-12"), y = 18, size = 8, colour = "black") +
  annotate("text", label = "LR = 129", x = as.Date("2020-10-10"), y = 16.5, size = 8, colour = "black") +
  annotate("text", label = "K = 12", x = as.Date("2020-10-03"), y = 15, size = 8, colour = "black")


# AH: Not needed - commented out.
### 3.3. monthly International
#df2.Int <- select(dfplt.month, c(date, bInt, INT))
#df2.Int <- melt(df2.Int, id=c("date"))
#df2.Int["bInt.low"] = qpois(0.025, df2.Int$bInt)
#df2.Int["bInt.high"] = qpois(0.975, df2.Int$bInt)
#df2.Int["INT.low"] = qpois(0.025, df2.Int$INT)
#df2.Int["INT.high"] = qpois(0.975, df2.Int$INT)

#p3 <- ggplot()+
#  geom_ribbon(data=df2.Int, aes(x=date, ymin=bInt.low, ymax=bInt.high, fill = cpalete[8]), alpha = 0.3)+
#  geom_ribbon(data=df2.Int, aes(x=date, ymin=INT.low, ymax=INT.high, fill = cpalete[1]), alpha = 0.3)+
#  geom_line(data= df2.Int, aes(x=date, y=bInt), size=0.5, color=cpalete[8])+
#  geom_line(data= df2.Int, aes(x=date, y=INT), size=0.5, color=cpalete[1])+
#  geom_point(data= df2.Int, aes(x=date, y=bInt), size=1, color=cpalete[8])+
#  geom_point(data= df2.Int, aes(x=date, y=INT), size=1, color=cpalete[1])+
  #geom_bar(data=df2.Int, aes(x=date, y=value, fill=variable), stat="identity", position=position_dodge(),alpha=.8) +
  #geom_line(data= dfplt.month, aes(x=date, y=RInt),stat="identity",color="black", size=0) +
#  geom_point(data= dfplt.month,  aes(x=date, y=RInt),color="black", size =1.5) +
#  labs(x="", y="\n Reported travel-related cases (monthly)", title="International" ) +
#  ylim(0, 20) +
#  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11))+
#  labs(x="", y="\n Reported travel-related cases (monthly)",
#       title="Comparison of model predictions to reported travel-related cases \n International") +
#  scale_fill_manual(name = "", labels = c("Best model","Mechanistic model", "Data"), values = c( cpalete[8],cpalete[1], "black")) +
  #scale_color_manual(values = plt1)+
#  guides(color = guide_legend(title = ""), size=rel(1.4)) +
#  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1.2)),
#                          legend.position="bottom",
#                          legend.text=element_text(size=rel(1.2)),
#                          plot.title=element_text(size=rel(1.3), face="bold"),
#                          axis.title = element_text(size=rel(1.1)),
#                          axis.text.y = element_text(size=rel(1.2)))


# 3.4. monthly Canada
#df2.Dom <- select(dfplt.month, c(date, bCA, CA))
#df2.Dom["bCA.low"] = qpois(0.025, df2.Dom$bCA)
#df2.Dom["bCA.high"] = qpois(0.975, df2.Dom$bCA)
#df2.Dom["CA.low"] = qpois(0.025, df2.Dom$CA)
#df2.Dom["CA.high"] = qpois(0.975, df2.Dom$CA)


#p4 <- ggplot() +
#  geom_ribbon(data=df2.Dom, aes(x=date, ymin=bCA.low, ymax=bCA.high, fill = cpalete[8]), alpha = 0.3)+
#  geom_ribbon(data=df2.Dom, aes(x=date, ymin=CA.low, ymax=CA.high, fill = cpalete[2]), alpha = 0.3)+
#  geom_line(data= df2.Dom, aes(x=date, y=bCA), size=0.5, color=cpalete[8])+
#  geom_line(data= df2.Dom, aes(x=date, y=CA), size=0.5, color=cpalete[2])+
#  geom_point(data= df2.Dom, aes(x=date, y=bCA), size=1, color=cpalete[8])+
#  geom_point(data= df2.Dom, aes(x=date, y=CA), size=1, color=cpalete[2])+
  #geom_bar(data=df2.Dom, aes(x=date, y=value, fill=variable), stat="identity", position=position_dodge(),alpha=.8 ) +
  #geom_line(data= dfplt.month, aes(x=date, y= RCA),stat="identity",color="black", size=1) +
#  geom_point(data= dfplt.month,  aes(x=date, y= RCA),color="black", size =1.5) +
#  ylim(0, 120) +
#  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11))+
#  labs(x="", y="\n Reported travel-related cases (monthly)",
#       title="Comparison of model predictions to reported travel-related cases \n Canada") +
  #scale_color_manual(values = plt2)+
#  scale_fill_manual(name = "", labels = c( "Best model","Mechanistic model", "Data"), values = c( cpalete[8],cpalete[2], "black")) +
#  guides(color = guide_legend(title = ""), size=rel(1.4)) +
#  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1.2)),
#                          legend.position="bottom",
#                          legend.text=element_text(size=rel(1.2)),
#                          plot.title=element_text(size=rel(1.3), face="bold"),
#                          axis.title = element_text(size=rel(1.1)),
#                          axis.text.y = element_text(size=rel(1.2)))



#m2
#ggsave("Figure/figureApp1.png", width = 5, , height = 5, dpi =500)


(p2best+p1best)/(p2agg+p1agg)/(p2mech+p1mech) + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 20))
ggsave("Figure/Figure3.png", width = 18, , height = 18, dpi =500)

(p2best+p1best)/(p2agg+p1agg) + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 20))
ggsave("Figure/Figure3A.png", width = 18, , height = 12, dpi =500)

(p2best+p1best)/(p2mech+p1mech)+ plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 20))
ggsave("Figure/Figure3B.png", width = 18, , height = 12, dpi =500)

(p2best+p2mech)+ plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 20))
ggsave("Figure/Figure3C.png", width = 18, , height = 6, dpi =500)
