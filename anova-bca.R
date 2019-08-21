df<-read.csv("C:/Users/trinh/Desktop/ANOVA.csv",header=T)

require(ggplot2)
ggplot(df, aes(x=Type, y=Avg)) + geom_boxplot(fill="grey80", color="blue")+
  scale_x_discrete() + xlab("Type") + ylab(expression(paste(mu,"moles/min/mL enzyme")))

dmod <- lm(Avg~Type, data=df)
summary(dmod)
anova(dmod)
confint(dmod)