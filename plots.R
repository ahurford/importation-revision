## Winter 2022
## Project: Importation Risk
## Zahra Mohammadi
#-----------------------------
#source("modelling.R")
## IMP and dfplot data frames will be used for plotting
## IMP : output epi model Canadian provinces, rotational workers, regular travelers, INT
## dfplot : included best model, epi model and reported CA and INT

bestmodel.Int <- Int.TVInf.nb$fitted.values # infection prevalence
bestmodel.Int.Agg <-Int.TVInf.agg.nb$fitted.values 
bestmodel.CA <- CA.TVInf$fitted.values # travel volume x infection prevalence
bestmodel.CA.Agg <- CA.TVInf.agg.nb$fitted.values # infection prevalence

# Added October 1, 2025. We need to also do a deviance test, is a test of whether the data
# could plausibly have come from the best models.
require(vcdExtra)
LRstats(Int.TVInf.nb)
LRstats(CA.TVInf)


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

bestDomCI <- add_pi(df.model7.lag, CA.TVInf.nb, names = c("lpb", "upb"))
bestIntCI <- add_pi(df.model6.lag, Int.TVInf.nb, names = c("lpb", "upb"))
bestDomAggCI <- add_pi(df.model7.lag, CA.TVInf.agg.nb, names = c("lpb", "upb"))
bestIntAggCI <- add_pi(df.model6.lag, Int.TVInf.agg.nb, names = c("lpb", "upb"))
dfplt["bestDom.low"] = bestDomCI$lpb
dfplt["bestDom.high"] = bestDomCI$upb
dfplt["bestInt.low"] = bestIntCI$lpb
dfplt["bestInt.high"] = bestIntCI$upb
vals=which(dfplt$bestInt.high>14) # fixes a plotting problem with prediction intervals
dfplt$bestInt.high[vals]=14
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
  ylim(c(0,14))+
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
  ylim(c(0,14))+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))+
  annotate("text", label = "LR = 42.5", x = as.Date("2020-10-07"), y = 14, size = 8, colour = "black") +
  annotate("text", label = "Res Dev = 125.4", x = as.Date("2020-10-27"), y = 13, size = 8, colour = "black")
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
  ylim(c(0,14))+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))+
  annotate("text", label = "LR = 15.0", x = as.Date("2020-10-07"), y = 14, size = 8, colour = "black")


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
  ylim(c(0,21))+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))

p2agg <- ggplot() + 
  geom_ribbon(data=df1.Dom, aes(x=date, ymin=bestDomAgg.low, ymax= bestDomAgg.high), fill=cpalete[2],alpha = 0.3)+
  geom_line(data=df1.Dom, aes(x=date, y=bestDomAgg),color=cpalete[2], size=1.5) +
  geom_point(data=dfplt, aes(x=date, y= Rep.Dom),color='black', size=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16))+
  labs(x="", y="\n Travel-related cases (daily)",
       title="Canada: Best model without province of origin") +
  ylim(c(0,21))+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))+
  annotate("text", label = "LR = 43.1", x = as.Date("2020-10-07"), y = 21, size = 8, colour = "black")


p2best <- ggplot() + 
  geom_ribbon(data=df1.Dom, aes(x=date, ymin=bestDom.low, ymax=bestDom.high), fill=cpalete[2],alpha = 0.3)+
  geom_line(data=df1.Dom, aes(x=date, y=bestDom),color=cpalete[2],size=1.5) +
  geom_point(data=dfplt, aes(x=date, y= Rep.Dom),color='black', size=1.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16))+
  labs(x="", y="\n Travel-related cases (daily)",
       title="Canada: Best model") +
  ylim(c(0,21))+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(2)),
                          plot.title=element_text(size=rel(2), face="bold"),
                          axis.title = element_text(size=rel(2)),
                          axis.text.y = element_text(size=rel(2)))+
              annotate("text", label = "LR = 177.7", x = as.Date("2020-10-07"), y = 21, size = 8, colour = "black") +
              annotate("text", label = "Res Dev = 233", x = as.Date("2020-10-21"), y = 19.5, size = 8, colour = "black")


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
