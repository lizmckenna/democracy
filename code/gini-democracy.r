rm(list=ls())

library(ggplot2)
library(extrafont)
library(scales)
library(plotly)
library(stargazer)
library(ggrepel)

nl=read.csv(file="/Users/lizmckenna/Dropbox/00_dissertation/data/quant/datasets/politics/latinobarometro/gini-democracy-natl.csv", header=TRUE,row.names=1,check.names=FALSE)
# here's the link to the file: https://www.dropbox.com/s/6cn0umo0sm3ou6w/gini-democracy-natl.csv?dl=0


p <- ggplot(nl, aes(gini, democracy, label = label_2))+
  geom_point(aes(color = factor(country))) +
  geom_smooth(method = "lm", se = FALSE, color="black") +
  geom_label_repel(aes(label = label_2, fill = country),
                   fontface = 'bold',
                   box.padding = unit(.6, "lines"),
                   point.padding = unit(0, "lines"),
                   segment.color = 'black') + theme_bw()
p

p1 <- p + theme(legend.position = "none") + labs(x = "Gini coefficient", 
  y = "Popular support for democracy (%)",
subtitle = "Labels indicate the country average on the DV over the 18-year time series, e.g., the 'democracy support' average for this period in Mexico was 48 percent.",
caption = "Data source: CEPAL for Gini coefficient, Latinobarómetro for dependent variable, which was the percent of respondents in a national survey who agreed with the statement: 'Democracy has its problems, but
is preferable to all other forms of government.'") +
  ggtitle("Relationship between income inequality and democracy in Latin America, 2000-2018")

p1 + theme(plot.title = element_text(color = "black", size = 16, face = "bold"),  
           plot.caption = element_text(hjust = 0),
           axis.title.x = element_text(color="black", size=14, face="bold"),
           axis.title.y = element_text(color="black", size=14, face="bold"))

######

linear.1 <- lm(democracy ~ gini, data=nl)
stargazer(linear.1)

#Export

dev.copy(png,'gini-democracy.png', height =5, width = 8, units = 'in', res=400)
dev.off()
