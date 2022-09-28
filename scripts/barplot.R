#former using base plot:

names <- c("GSOCmap", "HWSD", "HWSDa", "FAO2007", "WISE", "DSMW", "soilgrids250")
values <- c(682,967,699,710,504,574,1267)
xx <- barplot(values, names.arg = names, cex.names= 0.7, col = c(rgb(0.6,0.2,0.06), rep("darkgrey", 6)))
text(x=xx, y = values/2, label=paste(values, "Pg"), srt = 90, cex = 1.2)

##############################################
# using ggplot
names <- c("GSOCmap", "HWSD", "HWSDa", "FAO2007", "WISE", "DSMW", "soilgrids250")
values <- c(682,967,699,710,504,574,1267)
factor <- c(1,rep(0,6))
order <- seq(1,7,1)
df <- cbind.data.frame(order,names,values,factor)
df <- arrange(df, by =order)
df$factor <- factor(df$factor)

library(ggplot2)
barplot <- ggplot(df, aes(x = names, y = values, fill = factor)) + 
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("#666666","#990000")) +
  scale_y_continuous(breaks=c(0,200,400,600,800,1000,1200,1400), 
                     limits = c(0,1300), expand = c(0,0)) +
  xlab("") + ylab("") + scale_x_discrete(limits = names) +
  guides(fill=FALSE) +theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x = element_blank()) + theme_light()+
  theme(axis.text = element_text(colour = "black")) +
  theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank()) +
  geom_text(x = names, y = values/2, label = paste(values, "Pg"), angle = 90, 
            colour = "black", fontface = "bold")+
  annotate("text",x = 1, y = values[1]/2, label = paste(values[1], "Pg"), 
           angle = 90, col = "white", fontface = "bold") 

ggsave(filename = "images/global_assessments.png", plot = barplot, dpi = 300)