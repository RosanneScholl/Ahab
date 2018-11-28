library(readr)
wideMerged1_wideMerged <- read_csv("~/Downloads/wideMerged1 - wideMerged.csv")
View(wideMerged1_wideMerged)

library (ggplot2)
library (Hmisc)

################################################################
################reason elaboration##############################
##################################
#reason elab ~ duration

filterhours1 <- wideMerged1_wideMerged[which(wideMerged1_wideMerged$hours<=30),]
pelab_reason<-filterhours1$elab_reason*100

ggplot(data=filterhours1, aes(y=pelab_reason, x=filterhours1$lnhours))+
  geom_point(data=filterhours1, aes(y=pelab_reason, x=filterhours1$lnhours),shape=21, colour="#3366FF", position=position_jitter(height =1.5)) +
  geom_smooth (method=lm)+
  xlim(-2.75,3.75)+
  scale_x_continuous(limits=c(-2.75,3.5) , breaks = c(log(.1),log(1),log(10),log(24),log(30)),
    label = c(0.1,1,  10,24,30))+
  labs(x ="Subsession hours",
       y = "% elaborated reasons")+
  theme(text = element_text(size=20)
  )

fit30 <- lm(pelab_reason ~ filterhours$hours, data = filterhours)
summary(fit30)
exp(3)


##################################
#reason elab ~ uri
qplot(wideMerged1_wideMerged$uri, geom="histogram")+xlim(-1,3000)


filteruri <- wideMerged1_wideMerged[which(wideMerged1_wideMerged$uri<=3000),]
pelab_reason<-filteruri$elab_reason*100

ggplot(data=filteruri, aes(y=pelab_reason, x=filteruri$lnuri))+
  geom_point(data=filteruri, aes(y=pelab_reason, x=filteruri$lnuri),shape=21, colour="#3366FF", position=position_jitter(height =1.5)) +
  geom_smooth (method=lm)+
  scale_x_continuous(breaks = c(0,log(5),log(10),log(100),log(3000)),
                     label = c(0,5,10,100,3000))+
  labs(x ="URI count",
       y = "% elaborated reasons")+
  theme(text = element_text(size=20)
  )

fit31 <- lm(pelab_reason ~ filteruri$uri, data = filteruri)
summary(fit31)


##################################
#reason elab ~ search


################################################################
################ task variety ##############################
##################################

##################################
#task variety ~ duration

##################################
#task variety ~ uri

##################################
#task variety ~ search




################################################################
################ affect ##############################
##################################

##################################
#affect ~ duration

##################################
#affect ~ uri

##################################
#affect ~ search

