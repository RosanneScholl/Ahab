library (ggplot2)
library (Hmisc)


filterhours1 <- wideMerged1_wideMerged[which(wideMerged1_wideMerged$hours<=30),]
prim<- recode(filterhours1$primaryness,"'I use Firefox <strong>more often</strong> than I use the other browser(s) that I sometimes choose.'='Firefox primary'; 'FF only'='Firefox only';'FF secondary'= 'Firefox secondary'")

ggplot(filterhours1) +
  geom_bar (aes(prim, hours, fill=as.factor(prim)),
            position="dodge", stat="summary", fun.y= "mean",fill="#3366FF", color="black")+
  labs(x ="",
       y = "Subsession hours")+
  theme(text = element_text(size=20)
  )



filteruri <- wideMerged1_wideMerged[which(wideMerged1_wideMerged$uri<=3000),]
prim<- recode(filteruri$primaryness,"'I use Firefox <strong>more often</strong> than I use the other browser(s) that I sometimes choose.'='Firefox primary'; 'FF only'='Firefox only';'FF secondary'= 'Firefox secondary'")

ggplot(filteruri) +
  geom_bar (aes(prim, uri, fill=as.factor(prim)),
            position="dodge", stat="summary", fun.y= "mean",fill="#3366FF", color="black")+
  labs(x ="",
       y = "URI count")+
  theme(text = element_text(size=20)
  )

filtersearch <- wideMerged1_wideMerged[which(wideMerged1_wideMerged$uri<=100),]
prim<- recode(filtersearch$primaryness,"'I use Firefox <strong>more often</strong> than I use the other browser(s) that I sometimes choose.'='Firefox primary'; 'FF only'='Firefox only';'FF secondary'= 'Firefox secondary'")

ggplot(filtersearch) +
  geom_bar (aes(prim, uri, fill=as.factor(prim)),
            position="dodge", stat="summary", fun.y= "mean",fill="#3366FF", color="black")+
  labs(x ="",
       y = "Search count")+
  theme(text = element_text(size=20)
  )
