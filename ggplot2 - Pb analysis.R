library(ggplot2)

z<-data.frame(conc=c(0,4.85,9.66), imean=c(-197.7,-402.3,-593.9))

reg <- function(z){
  m <- lm(imean ~ conc, z);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

ggplot(z,aes(conc,imean))+geom_point(shape=1) + #shape of each point
  geom_smooth(method='lm',se=F)+geom_text(x = 1.75, y = 600, label = reg(z), parse = TRUE)+scale_y_reverse( lim=c(0,-700))    #if T, R will parse into expressions and displayed
                                          
  