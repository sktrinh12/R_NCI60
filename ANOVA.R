plant.df<-PlantGrowth
plant.df$group<-factor(plant.df$group, labels=c("Control", "Treatment 1", "Treatment 2"))
require(ggplot2)
ggplot(plant.df, aes(x=group, y=weight)) + geom_boxplot(fill="grey80", color="blue")+
  scale_x_discrete() + xlab("Treatment Group") + ylab("Dried weight of plants")
#geom_boxplot() is used to specify background and outline colors of boxes

plant.mod1 <- lm(weight~group, data=plant.df)
summary(plant.mod1)
#since the p-value is <0.05, we can make a haunch on the differences in variances
anova(plant.mod1) #verify that there is difference in average growth for 2nd treatment compared to control group
confint(plant.mod1) #confidence interval at 95%

plant.mod<-data.frame(Fitted=fitted(plant.mod1), residuals=resid(plant.mod1), treatment=plant.df$group)
ggplot(plant.mod,aes(Fitted, residuals, color=treatment)) + geom_point()
