require(ggplot2)
a = seq(0,13,1)
g =1-cumsum(dgamma(a,5.809,0.948))
g = g/sum(g)

tt = c(0, 0.05, 0.1, 0.55, 0.78, 0.77, 0.73, 0.68, 0.64, 0.59, 0.55, 0.49, 0.43, 0.31)
pretest = c(1,1,1-tt)
pretest = head(pretest,14)
pretest = pretest/sum(pretest)

df=data.frame(a,g)
df2 = data.frame(a,pretest=pretest)
df3 = data.frame(a,tsens = head(tt,14))
df4 = data.frame(a,lambda_a = dgamma(a,5.809,0.948))
p1<-ggplot(data=df, aes(x=a, y=g)) +
  geom_bar(stat="identity", fill = "orange")+xlab("days since exposure at arrival, a")+ylab("probability")+theme_classic()
  
p2<-ggplot(data=df2, aes(x=a, y=pretest)) +
               geom_bar(stat="identity", fill = "orange")  + xlab("days since exposure at arrival, a") + ylab("probability")+ theme_classic()

p3<-ggplot(data=df3, aes(x=a, y=tsens)) +
  geom_bar(stat="identity", fill = "darkseagreen") + ggtitle("Positive PCR test result") + xlab("days since exposure, a") + ylab("probability")+ theme_classic()

p4<-ggplot(data=df4, aes(x=a, y=lambda_a)) +
  geom_bar(stat="identity", fill = "darkseagreen") + ggtitle("Timing of first symptom onset") + xlim(c(0,13))+ xlab("days since exposure, a") + ylab("probability")+ theme_classic()

