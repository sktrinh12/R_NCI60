library(ggplot2)
library(gtable)
library(grid)
require(ggplot2)
df1 <- data.frame(frax=c(0,30,60,114),solvb=c(0,0,100,100))
df2 <-data.frame(
  type = factor(c("mascot","mstat"), levels=c("mascot","mstat")), frax = c(30,35,40,45,50,55), phos=c(542,413,233,500,600,650))

p1<-ggplot(df2,aes(x=frax, y=phos,fill=type)) + geom_bar(stat="identity",position="dodge") + scale_x_continuous("fractions",breaks=seq(1,115,2)) + scale_y_continuous("Phospho hits",breaks=seq(0,1400,250))
p2<-ggplot(df1,aes(x=frax,y=solvb)) + geom_line(colour="blue") +theme(panel.background = element_rect(fill = "transparent"))
#stat=identity coerces height of bar to represent values in data, position is separate bar plots (not stacked)
p3<-ggplot() + geom_bar(data=df2, aes(x=frax, y=phos, fill=type), stat="identity",position="dodge") + scale_x_continuous("fractions",breaks=seq(2,114,2)) + scale_y_continuous("Phospho hits",breaks=seq(0,1400,250)) + geom_line(data=df1, aes(x=frax, y=solvb*6.5), colour="blue")+theme(axis.text.x = element_text(angle = 90))
p3
#extract gtable
g1<-ggplot_gtable(ggplot_build(p1))
g2<-ggplot_gtable(ggplot_build(p2))

#overlap the panel of 2nd plot on that of 1st plot
pp <-c(subset(g1$layout, name == "panel", se=t:r))
g<-gtable_add_grob(g1,
                   g2$grobs[[which(g2$layout$name == "panel")]],
                   pp$t,pp$l,pp$b,pp$l)

#axis tweaks
alab<-g2$grobs[[which(g2$layout$name=="ylab")]]
ia<-which(g2$layout$name == "axis-l")
ga<-g2$grobs[[ia]]
ax<-ga$children[[2]]
ax$widths<-rev(ax$widths)
ax$grobs<-rev(ax$grobs)
ax$grobs[[1]]$x<-ax$grobs[[1]]$x-unit(1,"npc")+
  unit(0.15,"cm")
g<-gtable_add_cols(g,g2$widths[g2$layout[ia,]$l],
                   length(g$widths)-1)
g<-gtable_add_cols(g, g2$widths[g2$layout[ia,]$l],
                   length(g$widths)-1)
g<-gtable_add_grob(g,ax,pp$t,length(g$widths) - 2,pp$b)
g<-gtable_add_grob(g,alab,pp$t,length(g$widths) - 1,pp$b)

grid.draw(g)
