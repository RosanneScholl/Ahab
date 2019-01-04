library (ggplot2)
library (Hmisc)
install.packages("car")
library (car)

summary (wideMerged1_wideMerged$`On my side`)
summary (wideMerged1_wideMerged$hb_engaged)


#hours and on my side
surveyhours <- wideMerged1_wideMerged[which(wideMerged1_wideMerged$hours<=30),]
surveyhours$onmyside<- surveyhours$`On my side`
summary (surveyhours$onmyside)



ggplot(surveyhours) +
  geom_bar (aes(surveyhours$onmyside,hours, fill=as.factor(surveyhours$onmyside)),
            position="dodge", stat="summary", fun.y= "mean",fill="#3366FF", color="black")+
        scale_x_continuous( breaks = c(0,1),
                           label = c("'On my side' not selected", "'On my side' selected"))+
          labs(x ="When I choose a browser, I'm looking for one that is...",
               y = "Subsession hours")+
          theme(text = element_text(size=20)
          )
t.test(surveyhours$hours ~ surveyhours$onmyside, alternative = c("two.sided"), mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)
#the difference is about 28 minutes of subsession hours, but the difference is not statistically significant. Users who choose 'on my side' as something they're looking for when they choose a browser are not heavier users than those who don't choose 'on my side'
(7.6829-7.2116)*60

#another look at the same thing:
surveyhours$bins <- cut(surveyhours$hours,breaks = 15)
ggplot(surveyhours, aes(x = surveyhours$bins, y = surveyhours$onmyside)) +
  stat_summary(fun.y = "mean", geom = "point")+
  labs(x ="Binned subsession hours",
       y = "Percent who chose 'on my side'")+
  theme(text = element_text(size=20),
          axis.text.x = element_text(size=14,angle=45, hjust=1)) 



#uri and on my side
surveyuri <- wideMerged1_wideMerged[which(wideMerged1_wideMerged$uri<=3000),]
surveyuri$onmyside<- surveyuri$`On my side`
summary (surveyuri$onmyside)

ggplot(surveyuri) +
  geom_bar (aes(surveyuri$onmyside,uri, fill=as.factor(surveyuri$onmyside)),
            position="dodge", stat="summary", fun.y= "mean",fill="#3366FF", color="black")+
  scale_x_continuous( breaks = c(0,1),
                      label = c("'On my side' not selected", "'On my side' selected"))+
  labs(x ="When I choose a browser, I'm looking for one that is...",
       y = "URI count")+
  theme(text = element_text(size=20)
  )
t.test(surveyuri$uri ~ surveyuri$onmyside, alternative = c("two.sided"), mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)
#the difference is about 16.5 more URIs in an average day over the previous week, but the difference is not statistically significant. Users who choose 'on my side' as something they're looking for when they choose a browser are not heavier users than those who don't choose 'on my side'
226.3218-209.7742




#search and on my side
surveysearch <- wideMerged1_wideMerged[which(wideMerged1_wideMerged$search<=100),]
surveysearch$onmyside<- surveysearch$`On my side`
summary (surveysearch$onmyside)

ggplot(surveysearch) +
  geom_bar (aes(surveysearch$onmyside,surveysearch$search, fill=as.factor(surveysearch$onmyside)),
            position="dodge", stat="summary", fun.y= "mean",fill="#3366FF", color="black")+
  scale_x_continuous( breaks = c(0,1),
                      label = c("'On my side' not selected", "'On my side' selected"))+
  labs(x ="When I choose a browser, I'm looking for one that is...",
       y = "Search count")+
  theme(text = element_text(size=20)
  )
t.test(surveysearch$search ~ surveysearch$onmyside, alternative = c("two.sided"), mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)
#the difference is about 1 more searches in an average day over the previous week, but the difference is not statistically significant- although it's really close. Users who choose 'on my side' as something they're looking for when they choose a browser are not heavier users than those who don't choose 'on my side'
6.952085-5.913405


#another look at the same thing:
surveysearch$bins <- cut(surveysearch$search,breaks = 10)
ggplot(surveysearch, aes(x = surveysearch$bins, y = surveysearch$onmyside)) +
  stat_summary(fun.y = "mean", geom = "point")+
  labs(x ="Binned search count",
       y = "Percent who chose 'on my side'")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(size=14,angle=45, hjust=1)) 








