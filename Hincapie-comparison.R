#Estimate of the maximum number of importations as plotted in
#Figure 3 righthand columns vertical axis of Hincapie et al.
#https://academic.oup.com/jtm/article/29/8/taac100/6679266?login=true

data = data.frame(jurisdiction = c("YT", "NT", "NL", "NB", "NS", "PE"), max.importations = c(10, 35, 45, 9.5, 25, 2.5))

# as compared to the total number of importations reported in Hurford et al.
# https://www.sciencedirect.com/science/article/pii/S0022519322003691?via%3Dihub

reported = c(12, 10, 259, 204, 239, 112)

data = data.frame(data, reported)

g1 = ggplot(data, aes(x=max.importations, y= reported, group=jurisdiction))+
  geom_point()+
  geom_text(label=data$jurisdiction, nudge_x = 3)+
  theme(legend.position = "none")+
  xlab("maximum daily importations")+
  ylab("reported importations")

ggsave("Figure/Hincapie.png", width=3, height=3)
  